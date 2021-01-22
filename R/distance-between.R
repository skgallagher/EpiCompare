## distance_functions -------

#' Distance/angle between points along path relative to eculidean distance (2d
#' path)
#'
#' @description Calculates the distance and angle between each point of a path
#'
#' @param data_df (n x 2) data.frame, each row is a pair of values (x,y),
#'   assumes rows are ordered in path
#'
#' @return
#' \item{distance}{vector of distances beween points (n - 1)}
#' \item{angle}{vector of angle change between points (n - 1) - in radians}
#' @export
dist_along_path_angle <- function(data_df){
  assertthat::assert_that(assertthat::are_equal(ncol(data_df), 2),
                          msg = "data_df is expected to have 2 columns")
  n <- nrow(data_df)
  output_d <- numeric(n - 1)
  output_a <- numeric(n - 1)

  for (i in 1:(n - 1)) {
    output_d[i] <- stats::dist(data_df[c(i, i+1),])

    delta_x <- data_df[i+1, 1] - data_df[i, 1]
    delta_y <- data_df[i+1, 2] - data_df[i, 2]
    theta_radians <- atan2(delta_y, delta_x)
    output_a[i] <- theta_radians
  }

  return(list(distance = output_d, angle = output_a))
}

#' Distance/direction between points along path relative to eculidean distance
#'
#' @description Calculates the distance and direction between each point of a
#'   path
#'
#' @param data_df (n x d) data.frame, assumes rows are ordered in path
#'
#' @return
#' \item{distance}{vector of distances beween points (n - 1)}
#' \item{direction}{data.frame of direction between points ((n - 1) x d)}
#' @export
dist_along_path_direction <- function(data_df){
  n <- nrow(data_df)
  d <- ncol(data_df)
  output_dist <- numeric(n - 1)
  output_dir  <- data_df[1:(n-1),]
  # this could be dangerous but preserves data.frame structure

  for (i in 1:(n-1)){
    output_dist[i] <- stats::dist(data_df[c(i, i+1),])
    output_dir[i,] <- data_df[i+1,] - data_df[i, ]
  }
  return(list(distance = output_dist, direction = output_dir))
}


#' Distance/direction between points along path relative to eculidean distance
#'
#' @description Calculates the distance and direction between each point of a
#'   path
#'
#' @param df (n x d) data.frame, assumes rows are ordered in path
#'
#' @return
#' \item{distance}{vector of distances beween points (n - 1)}
#' \item{direction}{data.frame of direction between points ((n - 1) x d)}
#' @export
distanglepath_df <- function(df){
  out = distalongpath(as.matrix(df))
  df_out <- as.data.frame(out[[2]])
  names(df_out) <- names(df)
  out[[2]] <- df_out
  return(out)
}



#' step along 2d path with angle
#'
#' @param start_point (x,y) starting point
#' @param angle angle away from point (in radians)
#' @param distance distance away from point for the new point
#'
#' @return next point along the correct angle and distance
#' @export
step_along_angle <- function(start_point, angle, distance){
  next_point <- start_point + distance * c(cos(angle), sin(angle))
  return(next_point)
}


#' step along path with direction
#'
#' @param start_point (d vector) starting point
#' @param direction path away from starting point
#' @param distance distance away from point for the new point
#'
#' @return next point along the correct angle and distance
#' @export
step_along_direction <- function(start_point, direction, distance){
  direction_distance <- sqrt(sum(direction^2))
  next_point <- start_point + distance/direction_distance * direction
  return(next_point)
}


#' point compression (2d)
#'
#' Coverts list of point locations to a set of equally spaced points through
#' linear interpolation.
#'
#' @param df 2 column data frame with all points (x,y)
#' @param num_splits integer number of points for the path to be represented in
#'
#' @return new_compression data.frame (num_splits x 2) of points along the path
#' equally spaced
#' @export
#'
#' @examples
#' library(dplyr)
#' my_df <- data.frame(x = 1:20) %>%
#'   mutate(y = x)
#' my_df_compression <- equa_dist_points_angle(my_df)
#' my_df_compression
equa_dist_points_angle <- function(df, num_splits = 13){

  dist_and_bearing <- dist_along_path_angle(df)
  dist <- dist_and_bearing[[1]]
  bearing <- dist_and_bearing[[2]]

  total_dist <- sum(dist)
  step_all <- total_dist/(num_splits - 1) # n-1 equa-distance points along path
  cum_steps <- step_all*(1:(num_splits - 2))
  cum_dist <- cumsum(dist)[c(-length(dist))]

  new_compression <- data.frame(matrix(0, nrow = num_splits, ncol = 2))
  index <- 2
  for (step in 1:length(cum_steps)) {
    step_full_dist <- cum_steps[step]
    start <- sum(cum_dist <= step_full_dist) + 1
    start_point <- df[start,]
    start_bearing <- (bearing[start])
    if (start != 1) {
      step_dist <- step_full_dist - cum_dist[start - 1]
    }else{ # if no points other than the first is correct
      step_dist <- step_full_dist
    }
    new_point <- step_along_angle(start_point, start_bearing, step_dist)

    new_compression[index,] <- new_point
    index <- index + 1
  }
  new_compression[1,] <- df[1,]
  new_compression[num_splits,] <- df[nrow(df),]

  names(new_compression) <- names(df)

  return(new_compression)
}


#' point compression
#'
#' Coverts list of point locations to a set of equally spaced points through
#' linear interpolation.
#'
#' @param df column data frame with all points (each row a point)
#' @param num_splits integer number of points for the path to be represented in
#'
#' @return new_compression data.frame (num_splits x d) of points along the path
#' equally spaced
#' @export
#'
#' @examples
#' library(dplyr)
#' my_df <- data.frame(x = 1:20) %>%
#'   mutate(y = x)
#' my_df_compression <- equa_dist_points_direction(my_df)
#' my_df_compression
equa_dist_points_direction <- function(df, num_splits = 13){
  d <- ncol(df)

  dist_and_direct <- distanglepath_df(df)
  dist <- dist_and_direct[[1]]
  direction <- dist_and_direct[[2]]

  total_dist <- sum(dist)
  step_all <- total_dist/(num_splits - 1) # n-1 equa-distance points along path
  cum_steps <- step_all*(1:(num_splits - 2))
  cum_dist <- cumsum(dist)[c(-length(dist))]
  
  if (total_dist > 0){
    new_compression <- data.frame(matrix(0, nrow = num_splits, ncol = d))
    index <- 2
    for (step in 1:length(cum_steps)) {
      step_full_dist <- cum_steps[step]
      start <- sum(cum_dist <= step_full_dist) + 1
      start_point <- df[start,]
      start_direction <- direction[start,]
      if (start != 1) {
        step_dist <- step_full_dist - cum_dist[start - 1]
      }else{ # if no points other than the first is correct
        step_dist <- step_full_dist
      }
      new_point <- step_along_direction(start_point,
                                        start_direction, step_dist)
  
      new_compression[index,] <- new_point
      index <- index + 1
    }
    new_compression[1,] <- df[1,]
    new_compression[num_splits,] <- df[nrow(df),]
  
    names(new_compression) <- names(df)
  } else {
    ## if there is no distance traveled, then just return a matrix with
    ## the same point `num_splits` number of times.
    new_compression <- data.frame(matrix(0, nrow = num_splits, ncol = d))
    for (step in 1:nrow(new_compression)){
      new_compression[step,] <- df[1,]
    }
    names(new_compression) <- names(df)
    
      
  }

  return(new_compression)
}

#' List of compressions (2d)
#'
#' Creates list of same length of compression for each path
#'
#' @param list_df list of dfs, where the (x,y) points are in the position
#' columns
#' @param position the columns of the data frames that contain the desired
#' (x,y) coordinates
#' @param num_splits  number of points for the path to be represented in
#' @param verbose boolean for having a progress bar
#'
#' @return list will the paths with the same length
#' @export
equa_dist_points_listable_angle <- function(list_df, position = 1:2,
                                            num_splits = 13,
                                            verbose = TRUE){
  out_list <- list()
  n_path <- length(list_df)

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Compressing [:bar] :percent eta: :eta",
      total = n_path, clear = FALSE, width = 38)
  }

  if (is.null(names(list_df))) {
    iterator_names <- 1:length(list_df)
  } else{
    iterator_names <- names(list_df)
  }

  for (path_name in iterator_names) {
    df_pulled_out <- list_df[[path_name]][,position]
    out_list[[path_name]] <- equa_dist_points_angle(df_pulled_out, num_splits)
    if (verbose) {
      pb$tick()
    }
  }

  return(out_list)
}



#' List of compressions
#'
#' Creates list of same length of compression for each path
#'
#' @param list_df list of dfs, where the rows contain each point
#' @param position the columns of the data frames that contain the desired
#' (x,y) coordinates
#' @param num_splits  number of points for the path to be represented in
#' @param verbose boolean for having a progress bar
#'
#' @return list will the paths with the same length
#' @export
equa_dist_points_listable_direction <- function(list_df, position = 1:2,
                                                num_splits = 13,
                                                verbose = TRUE){
  out_list <- list()
  n_path <- length(list_df)

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Compressing [:bar] :percent eta: :eta",
      total = n_path, clear = FALSE, width = 38)
  }

  if (is.null(names(list_df))) {
    iterator_names <- 1:length(list_df)
  } else{
    iterator_names <- names(list_df)
  }

  for (path_name in iterator_names) {
    df_pulled_out <- list_df[[path_name]][,position]
    out_list[[path_name]] <- equa_dist_points_direction(df_pulled_out,
                                                        num_splits)
    if (verbose) {
      pb$tick()
    }
  }

  return(out_list)
}


#' Calculates the distance matrix between a set of paths (Euclidean based). This
#' is actually d^2 (2d)
#'
#' @details This is currently built on raster::pointDistance, which only works
#'   in the two dimensional setting.
#'
#' @param path_list list of paths (data frames) - need to have the same num rows
#' @param position column index of (x,y) euclidean coords.
#' @param verbose boolean logic if should have print outs while computing
#' distance matrix
#'
#' @return distance matrix of dimension n x n
#' @export
dist_matrix_innersq_2d <- function(path_list,  position = NULL,
                                   verbose = FALSE){
  rnames <- names(path_list)
  
  assertthat::assert_that(
    (!is.null(position)) && length(position) == 2 &&
      all(position == floor(position)),
    msg = paste("this function requires an integer vector of",
                "length 2 for 'position'."))

  number_of_row_lengths <- sapply(path_list, nrow) %>% unique %>% length
  assertthat::assert_that(number_of_row_lengths == 1,
                          msg = paste("each data frame in 'path_list' needs",
                                      "to have the same length"))

  n_mat <- length(path_list)


  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Creating Distance Matrix [:bar] :percent eta: :eta",
      total = ((n_mat)*(n_mat + 1))/2, clear = FALSE, width = 51)
  }

  output_mat <- matrix(0, nrow = n_mat, ncol = n_mat)

  for (i in c(1:n_mat)) {
    for (j in c(i:n_mat)) {
      output_mat[i,j] <- sum(raster::pointDistance(path_list[[i]][,position],
                                                   path_list[[j]][,position],
                                                   lonlat = FALSE,
                                                   allpairs=FALSE)^2)
      output_mat[j,i] <- output_mat[i,j]

      if (verbose) {
        pb$tick()
      }
    }
  }
  
  rownames(output_mat) <- rnames
  
  return(output_mat)
}

#'@rdname dist_matrix_innersq_2d
dist_matrix_innersq_angle <- dist_matrix_innersq_2d




#' Calculates the distance matrix between a set of paths (Euclidean based). This
#' is actually d^2
#'
#'
#' @param x list of paths (data frames) - need to have the same num rows or a nested_df
#' @param position column index of (x,y,...) euclidean coords.
#' @param verbose boolean logic if should have print outs while computing
#' distance matrix
#' @param tdm_out boolean logic if we should return a \code{tidy_dist_mat} 
#' object or just a matrix.
#'
#' @return distance matrix of dimension n x n or data.frame - DESCRIBE
#' @export
dist_matrix_innersq_direction <- function(x, position = NULL,
                                          verbose = FALSE,
                                          tdm_out = FALSE){
    UseMethod("dist_matrix_innersq_direction")
}

#' inner l2 distance between filaments
#'
#' @param m1 data.frame (n x p)
#' @param m2 data.frame (n x p)
#'
#' @return distance between filaments
l2filamentdist_df  <- function(m1, m2){
  l2filamentdist(as.matrix(m1), as.matrix(m2))
}

#' @export
#' @rdname dist_matrix_innersq_direction
dist_matrix_innersq_direction.list <- function(x,  position = NULL,
                                          verbose = FALSE, tdm_out = FALSE){
  
  rnames <- names(x)
  
  n_mat <- length(x)
  n_row <- nrow(x[[1]])

  assertthat::assert_that(
    (!is.null(position)) &&
      all(position == floor(position)),
    msg = paste("this function requires an vector of integer values for",
                "'position'."))

  number_of_row_lengths <- sapply(x, nrow) %>% unique %>% length
  assertthat::assert_that(number_of_row_lengths == 1,
                          msg = paste("each data frame in 'x' needs",
                                      "to have the same length"))


  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Creating Distance Matrix [:bar] :percent eta: :eta",
      total = ((n_mat)*(n_mat + 1))/2, clear = FALSE, width = 51)
  }

  output_mat <- matrix(0, nrow = n_mat, ncol = n_mat)

  for (i in c(1:n_mat)) {
    for (j in c(i:n_mat)) {

      output_mat[i,j] <- l2filamentdist_df(x[[i]][, position],
                                           x[[j]][, position])
      #output_mat[i,j] <- sum(
      #  (x[[i]][, position] - x[[j]][, position])^2
      #)
      # ^ same as:
      # sum(apply(
      # (x[[i]][, position] - x[[j]][, position])^2,
      # 1, sum))
      output_mat[j,i] <- output_mat[i,j]

      if (verbose) {
        pb$tick()
      }
    }
  }
  
  # what type of object do we return?
  if (tdm_out){
    if (is.null(rnames)){
      rnames <- 1:length(x)
    }
    rownames_df <- data.frame(id = rnames)
    output_tdm <- tidy_dist_mat(output_mat,rownames_df,rownames_df)
    return(output_tdm)
  } else {
    rownames(output_mat) <- rnames
    return(output_mat)
  }
}

if (r_new_interface()){
  .S3method("dist_matrix_innersq_direction", "list")
}

#' @export
#' @rdname dist_matrix_innersq_direction
dist_matrix_innersq_direction.data.frame <- function(x, position = NULL,
                                                     verbose = FALSE, 
                                                     tdm_out = FALSE){
  
  
  
  id_info <- x %>% dplyr::select(-.data$data) 
  path_list <- x$data
  
  if (!tdm_out) {
    inner_names <- id_info %>% 
      tidyr::unite(col = "names", dplyr::everything(), sep = "|") %>% 
      dplyr::pull(.data$names)
    names(path_list) <- inner_names
  } else {
    names(path_list) <- 1:length(path_list)
  }
  
  dist_mat <- dist_matrix_innersq_direction.list(x = path_list,
                                                 position = position, 
                                                 verbose = verbose,
                                                 tdm_out = FALSE)
  
  if (tdm_out) {
    rownames_df <- id_info
    output_tdm <- tidy_dist_mat(dist_mat,rownames_df,rownames_df)
    return(output_tdm)
  } else {
    return(dist_mat)
  }
  
}

if (r_new_interface()){
  .S3method("dist_matrix_innersq_direction", "data.frame")
}

#' @export
#' @rdname dist_matrix_innersq_direction
dist_matrix_innersq_direction.grouped_df <- function(x, position = NULL,
                                                     verbose = FALSE, 
                                                     tdm_out = FALSE){
  
  x_inner <- x %>% tidyr::nest()
  id_info <- x_inner %>% dplyr::select(-.data$data) 
  path_list <- x_inner$data
  
  if (!tdm_out) {
    inner_names <- id_info %>% 
      tidyr::unite(col = "names", dplyr::everything(), sep = "|") %>% 
      dplyr::pull(.data$names)
    names(path_list) <- inner_names
  } else {
    names(path_list) <- 1:length(path_list)
  }
  
  # dealing with update of position vector
  group_names <- x %>% dplyr::groups() %>% 
    lapply(as.character) %>% unlist
  group_ids <- c(1:ncol(x))[names(x) %in% group_names]
  non_group_ids <- c(1:ncol(x))[!(names(x) %in% group_names)]
  position_inner <- c(1:length(non_group_ids))[non_group_ids %in% position]
  
  assertthat::assert_that(length(position_inner) == length(position),
                          msg = paste("possible that position vector is",
                                      "incorrect, possible that position is",
                                      "part also a grouping variable -",
                                      "which doesn't make sense"))
  
  dist_mat <- dist_matrix_innersq_direction.list(x = path_list,
                                                 position = position_inner, 
                                                 verbose = verbose,
                                                 tdm_out = FALSE)
  
  if (tdm_out) {
    rownames_df <- id_info %>% dplyr::ungroup() 
    output_tdm <- tidy_dist_mat(dist_mat,rownames_df,rownames_df)
    return(output_tdm)
  } else {
    return(dist_mat)
  }
  
}

if (r_new_interface()){
  .S3method("dist_matrix_innersq_direction", "grouped_df")
}

#' Distance between points
#'
#' \strong{Is this used?}
#'
#' @description Calculates the distance between each point of every path.
#'
#' @param data_df_p1 (n x p) data.frame, each row is a point
#' @param data_df_p2 (n x p) second data.frame to be compared, each row is a
#'   point
#'
#' @return vector of n distances between each order point in the two data frames
#' @export
dist_between_paths <- function(data_df_p1, data_df_p2){
  n <- dim(data_df_p1)[1]
  p <- dim(data_df_p1)[2]
  assertthat::assert_that(assertthat::are_equal(n, dim(data_df_p2)[1]),
                          msg = "DFs have different dimension (n)")
  assertthat::assert_that(assertthat::are_equal(p, dim(data_df_p2)[2]),
                          msg = "DFs have different dimension (p)")

  output_d <- sapply(1:n, function(i) {
    stats::dist(rbind(data_df_p1[i,], data_df_p2[i,]))
  })

  return(output_d)
}


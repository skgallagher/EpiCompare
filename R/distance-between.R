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

  dist_and_direct <- dist_along_path_direction(df)
  dist <- dist_and_direct[[1]]
  direction <- dist_and_direct[[2]]

  total_dist <- sum(dist)
  step_all <- total_dist/(num_splits - 1) # n-1 equa-distance points along path
  cum_steps <- step_all*(1:(num_splits - 2))
  cum_dist <- cumsum(dist)[c(-length(dist))]

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

  return(output_mat)
}

#'@rdname dist_matrix_innersq_2d
dist_matrix_innersq_angle <- dist_matrix_innersq_2d

#' Calculates the distance matrix between a set of paths (Euclidean based). This
#' is actually d^2
#'
#'
#' @param path_list list of paths (data frames) - need to have the same num rows
#' @param position column index of (x,y) euclidean coords.
#' @param verbose boolean logic if should have print outs while computing
#' distance matrix
#'
#' @return distance matrix of dimension n x n
#' @export
dist_matrix_innersq_direction <- function(path_list,  position = NULL,
                                          verbose = FALSE){

  n_mat <- length(path_list)
  n_row <- nrow(path_list[[1]])

  assertthat::assert_that(
    (!is.null(position)) &&
      all(position == floor(position)),
    msg = paste("this function requires an vector of integer values for",
                "'position'."))

  number_of_row_lengths <- sapply(path_list, nrow) %>% unique %>% length
  assertthat::assert_that(number_of_row_lengths == 1,
                          msg = paste("each data frame in 'path_list' needs",
                                      "to have the same length"))


  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Creating Distance Matrix [:bar] :percent eta: :eta",
      total = ((n_mat)*(n_mat + 1))/2, clear = FALSE, width = 51)
  }

  output_mat <- matrix(0, nrow = n_mat, ncol = n_mat)

  for (i in c(1:n_mat)) {
    for (j in c(i:n_mat)) {

      output_mat[i,j] <- sum(
        (path_list[[i]][, position] - path_list[[j]][, position])^2
      )
      # ^ same as:
      # sum(apply(
      # (path_list[[i]][, position] - path_list[[j]][, position])^2,
      # 1, sum))
      output_mat[j,i] <- output_mat[i,j]

      if (verbose) {
        pb$tick()
      }
    }
  }

  return(output_mat)
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


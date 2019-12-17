# utility functions ---------

#' Distance/angle between points along path relative to eculidean distance
#'
#' @description Calculates the distance and angle between each point of a path
#'
#' MAYBE REWRITE - using library(raster)?
#'
#' @param data_df (n x 2) data.frame, each row is a pair of values (x,y)
#'
#' @return
#' \item{distance}{vector of distances beween points (n - 1)}
#' \item{angle}{vector of angle change between points (n - 1) - in radians}
#' @export
#'
dist_along_path <- function(data_df){
  #MAYBE REWRITE THIS FUNCTION?
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


#' step along path
#'
#' @param start_point (x,y) starting point
#' @param angle angle away from point (in radians)
#' @param distance distance away from point for the new point
#'
#' @return next point along the correct angle and distance
#' @export
#'
step_along <- function(start_point, angle, distance){
  next_point <- start_point + distance * c(cos(angle), sin(angle))
  return(next_point)
}

#' point compression
#'
#' Coverts list of point locations to a set of equally spaced points through
#' linear interpolation.
#'
#' @param df 2 column data frame with all points (x,y)
#' @param num_splits number of points for the path to be represented in
#'
#' @return new_13compression data.frame (13 x 2) of points along the path
#' equally spaced
#' @export
#'
#' @examples
#' library(dplyr)
#' my_df <- data.frame(x = 1:20) %>%
#'   mutate(y = x)
#' my_df_compression <- equa_dist_points(my_df)
#' my_df_compression
equa_dist_points <- function(df, num_splits = 13){

  dist_and_bearing <- dist_along_path(df)
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
    new_point <- step_along(start_point, start_bearing, step_dist)

    new_compression[index,] <- new_point
    index <- index + 1
  }
  new_compression[1,] <- df[1,]
  new_compression[num_splits,] <- df[nrow(df),]

  names(new_compression) <- names(df)

  return(new_compression)
}

#' List of compressions
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
equa_dist_points_listable <- function(list_df, position = 1:2,
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
    out_list[[path_name]] <- equa_dist_points(df_pulled_out, num_splits)
    if (verbose) {
      pb$tick()
    }
  }

  return(out_list)
}

#' Calculates the distance matrix between a set of paths (Euclidean based). This
#' is actually d^2
#'
#' @param path_list list of paths (data frames) - need to have the same num rows
#' @param position column index of (x,y) euclidean coords.
#' @param verbose boolean logic if should have print outs while computing
#' distance matrix
#'
#' @return distance matrix of dimension n x n
#' @export
dist_matrix_innersq <- function(path_list,  position = 3:4,
                                verbose = FALSE){
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

#' Distance between points
#'
#' @description Calculates the distance between each point of every path.
#'
#' @param data_df_p1 (n x 2) data.frame, each row is a pair of values (x, y)
#' @param data_df_p2 (n x 2) second data.frame to be compared, each row is a
#' pair of values (x, y)
#'
#' @return vector of n distances between each order point in the two data frames
#' @export
dist_between_paths <- function(data_df_p1, data_df_p2){
  # WHY ARE WE USING RASTER HERE?
  n <- dim(data_df_p1)[1]
  p <- dim(data_df_p1)[2]
  assertthat::assert_that(assertthat::are_equal(n, dim(data_df_p2)[1]),
                          msg = "DFs have different dimension (n)")
  assertthat::assert_that(assertthat::are_equal(p, dim(data_df_p2)[2]),
                          msg = "DFs have different dimension (p)")
  assertthat::assert_that(assertthat::are_equal(p, 2),
                          msg = "DFs should have ncol = 2")

  output_d <- sapply(1:n, function(i) {
    stats::dist(rbind(data_df_p1[i,], data_df_p2[i,]))
  })

  return(output_d)
}


#' get xy ternary coordinates from xyz based data.frame
#'
#' @description note that this does not need x,y,z to be scaled (but it should).
#' This is just a data.frame wrapper for ggtern::tlr2xy.
#'
#' @param X_SIR data.frame with columns in xyz_col
#' @param xyz_col string vector (length 3) to match with x, y, and z
#'
#' @return X_SIR motified to have columns "x" and "y" with the ternary
#' coordinates
#' @export
#'
get_xy_coord <- function(X_SIR, xyz_col = c("S","I","R")){
  crd <- ggtern::coord_tern()
  xy_transform <- X_SIR %>% dplyr::rename(x = xyz_col[1],
                                          y = xyz_col[2],
                                          z = xyz_col[3]) %>%
    ggtern::tlr2xy(coord = crd)
  return(xy_transform)
}


#' create a grid of points indicating near border or not (and inside or outside)
#'
#' @param border_points data.frame of points from delta ball approach that are
#' "border points"
#' @param inner_points data.frame of points from delta ball approach that are
#' interior points.
#' @param delta float, size of delta ball radius
#' @param xrange vector, ordered values to examine in the x dimension (default
#' is NULL - will then be created using gridbreaks)
#' @param yrange vector, ordered values to examine in the y dimension (default
#' is NULL - will then be created using gridbreaks)
#' @param gridbreaks int, number of gridpoint in x and y dimensions if xrange
#' or yrange is not provided
#'
#' @return data frame, with expand.grid of xrange, yrange in columns x and y
#' and a column z that indicates if it is: (1) not within delta to border points
#' or inner_points, (2) if closest to border_points and (3) if closest to an
#' inner_point.
#'
#' @export
#'
get_closest <- function(border_points, inner_points, delta,
                        xrange = NULL, yrange = NULL, gridbreaks = 100){
  if (is.null(xrange)) {
    xrange <- seq(min(border_points$x), max(border_points$x),
                  length.out = gridbreaks)
  }
  if (is.null(yrange)) {
    yrange <- seq(min(border_points$y), max(border_points$y),
                  length.out = gridbreaks)
  }
  gridpoints <- expand.grid(xrange, yrange) %>%
    dplyr::rename(x = "Var1", y = "Var2")

  if (nrow(border_points) > 1){
    first <- raster::pointDistance(gridpoints,border_points,
                                   lonlat = FALSE,
                                   allpairs = TRUE)
    first_min <- apply(first, 1, min)
  } else if (nrow(border_points) == 1) {
    first_min <- raster::pointDistance(gridpoints,border_points,
                                       lonlat = FALSE,
                                       allpairs = TRUE)
  } else {
    first_min <- rep(Inf, nrow(gridpoints))
  }

  if (nrow(inner_points) > 1) {
    second <- raster::pointDistance(gridpoints,inner_points,
                                    lonlat = FALSE,
                                    allpairs = TRUE)
    second_min <- apply(second, 1, min)
  } else if (nrow(inner_points) == 1) {
    second_min <- raster::pointDistance(gridpoints,inner_points,
                                        lonlat = FALSE,
                                        allpairs = TRUE)
  } else {
    second_min <- rep(Inf, nrow(gridpoints))
  }

  z = apply(cbind(delta, first_min, second_min), 1, which.min)

  updated_gridpoints <- gridpoints %>%
    dplyr::mutate(z = z)

  return(updated_gridpoints)
}



#' Find delta for covering
#'
#' @description
#' Find the minimum distance (delta) such that all points are within delta of at
#' least one other point
#'
#' @details
#' This function is a simplification and rewrite of a function with the same
#' name from the \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}
#' package.
#'
#' @param data continuous data frame of individual points (in each row)
#' @param dist_mat distance matrix, calculated otherwise via euclidean distance
#'
#' @return
#' \describe{
#' \item{dist_mat}{distance matrix between points}
#' \item{mm_delta}{the minimum distance (delta)}
#' }
#' @export
get_delta <- function(data = NULL, dist_mat = NULL){
  if (is.null(dist_mat)) {
    dist_mat <- as.matrix(stats::dist(data))
  }
  diag(dist_mat) <- max(dist_mat)
  mm_delta <- apply(dist_mat, MARGIN = 1, min) %>% max
  diag(dist_mat) <- 0
  return(list(dist_mat = dist_mat, mm_delta = mm_delta))
}



#' Performs delta ball approach
#'
#' @param data_deep_points data deep points from depth function
#' @param xy_columns strings for column names of the points (x,y) - default is
#' actually \code{"lat", "long"} as we are currently using an function from the
#' \code{TCpredictionbands} package.
#'
#' @return
#' \describe{
#' \item{structure}{data frame of non-ordered lines of contour}
#' \item{delta}{optimal delta for covering}
#' }
#' @export
delta_structure <- function(data_deep_points, xy_columns = c("lat", "long")){
  data_deep_points <- data_deep_points %>%
    dplyr::select(dplyr::one_of(xy_columns)) %>%
    dplyr::rename(lat = xy_columns[1], long = xy_columns[2])

  d_out <- get_delta(data_deep_points)
  delta = d_out$mm_delta
  structure_df <- TCpredictionbands::delta_ball_wrapper(data_deep_points,
                                                        remove_duplicates = TRUE)

  names(structure_df)[names(structure_df) == "lat"] <- xy_columns[1]
  names(structure_df)[names(structure_df) == "long"] <- xy_columns[2]

  out <- list()
  out[["structure"]] <- structure_df
  out[["delta"]] <- delta
  return(out)
}


project_simplex <- function(x) olpsR::projsplx(x, b = 1)
project_simplex_vec <- function(x) { t(apply(x, 1, project_simplex))}

#' project onto a standard 3d simplex.
#'
#' @param df_3d data frame
#' @param column_names names of columns for the 3 dimensions.
#'
#' @return an updated version of \code{df_3d} with points projected onto
#' simplex.
#' @export
project_to_simplex <- function(df_3d, column_names = c("x","y","z") ){
  df_3d_inner <- df_3d %>% dplyr::select(dplyr::one_of(column_names))
  df_3d_inner <- project_simplex_vec(df_3d_inner)
  df_3d[,column_names] <- df_3d_inner
  return(df_3d)
}



# stats and geoms -------------------------

#' stat object for use in kde based stat_confidence_band and
#' geom_confidence_band
#' @export
StatConfBandKDE <- ggplot2::ggproto("StatConfBandKDE",
                                    ggplot2::Stat,
                                    compute_group =
  function(data, scales, grid_size = rep(300,2), alpha_level = .1){
    assertthat::assert_that(class(data$sim_group) != "factor",
                            msg = paste("'sim_group' cannot be a factor"))

    info_inner <- data[, c("PANEL", "group")] %>%
      sapply(unique)

    data <- data %>% mutate(sim_group = factor(sim_group))

    data2d <- data %>% get_xy_coord(xyz_col = c("x", "y", "z"))

    data2d_list <- split(x = data2d, f = data2d$sim_group)

    xy_position <- which(names(data2d_list[[1]]) %in% c("x","y"))
    #kde style
    kde_ci_list <- TCpredictionbands::kde_from_tclist(dflist = data2d_list,
                                                      grid_size = grid_size,
                                                      alpha = alpha_level,
                                                      position = xy_position)

    kde_ci_df <- kde_ci_list$contour %>% lapply(as.data.frame) %>%
      dplyr::bind_rows(.id = "kde_poly")

    kde_ci_df3 <- ggtern::xy2tlr(data = kde_ci_df %>%
                                   select(-kde_poly, -level),
                                 coord = ggtern::coord_tern()) %>%
      cbind(kde_poly = kde_ci_df$kde_poly, .) %>%
      dplyr::mutate(PANEL = info_inner[1],
                    piece = as.integer(kde_poly),
                    group = as.integer(kde_poly)) %>%
      # ^this seems like an odd approach
      project_to_simplex(column_names = c("x","y","z"))

    return(kde_ci_df3)            },
                                    required_aes = c("x", "y", "z", "sim_group"))

#' stat object for use in delta_ball based stat_confidence_band and
#' geom_confidence_band
#' @export
StatConfBandDeltaBall <- ggplot2::ggproto("StatConfBandDeltaBall",
                                          ggplot2::Stat,
                                          compute_group =
  function(data, scales, grid_size = rep(300,2), over_delta = .1,
           alpha_level = .1){

    assertthat::assert_that(class(data$sim_group) != "factor",
                            msg = paste("'sim_group' cannot be a factor"))


    info_inner <- data[, c("PANEL", "group")] %>% sapply(unique)

    data2d <- data %>% get_xy_coord(xyz_col = c("x", "y", "z"))

    data2d_list <- split(x = data2d, f = data2d$sim_group)
    xy_position <- which(names(data2d_list[[1]]) %in% c("x","y"))

    dist_mat <- dist_matrix_innersq(data2d_list,
                                    position = xy_position,
                                    verbose = FALSE)
    data_deep_points <- TCpredictionbands::depth_curves_to_points(data2d_list,
                                                        alpha = alpha_level,
                                                        dist_mat = dist_mat) %>%
      dplyr::rename(lat = "x", long = "y")

    delta_info <- delta_structure(data2d_list)

    structure <- delta_info$structure %>%
      dplyr::rename(x = "long", y = "lat")

    delta <- delta_info$delta

    inner_df <- dplyr::setdiff(data_deep_points %>%
                                 dplyr::rename(x = "lat", y = "long") %>%
                                 dplyr::select(x,y),
                               structure %>%
                                 dplyr::select(x,y))

    border_points <- structure %>% dplyr::select(x,y)
    inner_points <- inner_df




    xrange <- seq(min(border_points$x) - over_delta,
                  max(border_points$x) + over_delta,
                  length.out = grid_size[1])

    yrange <- seq(min(border_points$y) - over_delta,
                  max(border_points$y) + over_delta,
                  length.out = grid_size[2])

    updated_gridpoints <- get_closest(border_points, inner_points,
                                      delta,
                                      xrange = xrange,
                                      yrange = yrange,
                                      gridbreaks = NULL)

    if (tidyr_new_interface()){
      update_gridpoints_mat <- tidyr::pivot_wider(updated_gridpoints,
                                                  names_from = "y",
                                                  values_from = "z") %>%
        dplyr::select(-x) %>% as.matrix
    } else {
      update_gridpoints_mat <- tidyr::spread(updated_gridpoints,
                                                  key = "y",
                                                  value = "z") %>%
        dplyr::select(-x) %>% as.matrix
    }



    cl <- grDevices::contourLines(x = xrange, y = yrange,
                       z = update_gridpoints_mat,levels = c(2))

    lengths <- vapply(cl, function(x) length(x$x), integer(1))
    xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
    ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
    pieces <- rep(seq_along(cl), lengths)

    vis_df <- data.frame(
      x = xs,
      y = ys,
      piece = pieces,
      group = pieces
    ) %>% ggtern::xy2tlr(coord = ggtern::coord_tern()) %>%
      project_to_simplex(column_names = c("x","y","z"))

    return(vis_df)

  }, required_aes = c("x", "y", "z", "sim_group"))


#' stat object for use in delta_ball based stat_confidence_band and
#' geom_confidence_band
#' @export
StatConfBandSpherical <- ggplot2::ggproto("StatConfBandDeltaBall",
                                          ggplot2::Stat,
                                          compute_group =
  function(data, scales, grid_size = rep(300,2), over_delta = .1,
           alpha_level = .1, quantiles = c("time", "depth")){
    if (length(quantiles) > 1){
      quantiles <- quantiles[1]
    }




                                            },
                                          required_aes = c("x", "y", "z", "sim_group", "t"))

#' @export
#' @rdname geom_confidence_band
stat_confidence_band <- function(mapping = NULL, data = NULL, geom = "polygon",
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE,
                                 cb_type = c("kde", "delta_ball"),
                                 grid_size = rep(300, 2),
                                 alpha_level = .1,
                                 ...) {

  if (length(cb_type) > 1){
    cb_type <- cb_type[1]
  }

  assertthat::assert_that(cb_type %in% c("kde", "delta_ball"),
                          msg = paste("bc_type needs to either be 'kde' or ",
                                      "'delta_ball'."))

  ggplot2::layer(
    stat = list(StatConfBandKDE,
                StatConfBandDeltaBall)[which(c("kde", "delta_ball") == cb_type)][[1]],
    data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, alpha_level = alpha_level,
                  grid_size = grid_size, ...)
  )
}


#' The confidenct_band geom/stat
#'
#'
#' @param mapping Set of aesthetic mappings created by
#' \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. If specified
#' and \code{inherit.aes = TRUE} (the
#' default), it is combined with the default mapping at the top level of the
#' plot. You must supply mapping if there is no plot mapping
#' @param data The data to be displayed in this layer. There are three options:
#' If \code{NULL}, the default, the data is inherited from the plot data as
#' specified in the call to \code{\link[ggplot2]{ggplot}}.
#'
#' A \code{data.frame}, or other object, will override the plot data. All
#' objects will be fortified to produce a data frame. See
#' \code{\link[ggplot2]{fortify}} for which variables will be created.
#'
#' A \code{function} will be called with a single argument, the plot data. The
#' return value must be a \code{data.frame}, and will be used as the layer data.
#' A function can be created from a formula (e.g. \code{~ head(.x, 10)}).
#' @param geom string associated with desired geom. \code{stat} is
#' otherwise controlled by the \code{cb_type} parameter.
#' @param stat string associated with desired stat \code{geom} is
#' otherwise controlled by the \code{cb_type} parameter.
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a
#' warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#' never includes, and \code{TRUE} always includes. It can also be a named
#' logical vector to finely select the aesthetics to display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that
#' define both data and aesthetics and shouldn't inherit behaviour from the
#' default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param cb_type String indicating which confidence band type to use. Currently
#' only \code{"kde"} and \code{"delta_ball"} inputs are expected. See details
#' for more information.
#' @param grid_size integer vector, length 2. Size of the grid which is going to
#' be used to approximate confidence band (if needed). Can be reduced to
#' speed-up computation.
#' @param alpha_level confidence level for confidence band. Creates a
#' \code{1-alpha_level} level confidence band.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}. These are
#' often aesthetics, used to set an aesthetic to a fixed value,
#' like \code{colour = "red"} or \code{size = 3}. They may also be parameters to
#' the paired \code{geom}/\code{stat}.
#'
#' @details
#'
#' I SHOULD PUT INFORMATION ON THE DIFFERENT TYPES OF CONFIDENCE BANDS...
#'
#' @section Aesthetics:
#' \code{stat_confidence_band} understands the following aesthetics (required
#' aesthetics are in bold):
#'
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{z}}
#'   \item \strong{\code{sim_group}} - note: this cannot be a factor
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{group}
#'   \item \code{linetype}
#'   \item \code{size}
#'   \item \code{weight}
#' }
#' Learn more about setting these aesthetics in
#' \code{vignette("ggplot2-specs")}.
#'
#' @export
#' @examples
#' ## need to get new U_sims_tidy
#' library(ggplot2)
#' library(ggtern)
#' library(dplyr)
#' if (FALSE){
#' all_df %>% xy2tlr(coord = coord_tern()) %>%
#'   ggplot() + geom_line(aes(x = x, y = y, z = z, group = sim)) +
#'   coord_tern()
#'
#' all_df %>% xy2tlr(coord = coord_tern()) %>%
#'   mutate(sim = as.numeric(as.character(sim))) %>%
#'   ggplot() + stat_confidence_band(aes(x = x, y = y, z = z,
#'                                       sim_group = sim),
#'                                   cb_type = "kde") +
#'   coord_tern()
#'
#' all_df %>% xy2tlr(coord = coord_tern()) %>%
#'   mutate(sim = as.numeric(as.character(sim))) %>%
#'   ggplot() + stat_confidence_band(aes(x = x, y = y, z = z,
#'                                       sim_group = sim),
#'                                   cb_type = "delta_ball") +
#'   coord_tern()
#'
#' all_df %>% xy2tlr(coord = coord_tern())  %>%
#'   mutate(sim = as.numeric(as.character(sim))) %>%
#'   ggplot() + geom_confidence_band(aes(x = x, y = y, z = z,
#'                                       sim_group = sim),
#'                                       cb_type = "kde") +
#'   coord_tern()
#'
#' all_df %>% xy2tlr(coord = coord_tern()) %>%
#'   mutate(sim = as.numeric(as.character(sim))) %>%
#'   ggplot() + geom_confidence_band(aes(x = x, y = y, z = z,
#'                                       sim_group = sim),
#'                                   cb_type = "delta_ball") +
#'   coord_tern()

#' }
geom_confidence_band <- function(mapping = NULL, data = NULL,
                                 stat = list("ConfBandKDE",
                                             "ConfBandDeltaBall")[c("kde", "delta_ball") == cb_type][[1]],
                                 position = "identity",
                                 ...,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 cb_type = c("kde", "delta_ball"),
                                 grid_size = rep(300, 2),
                                 alpha_level = .1) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomConfBand,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' GeomConfBand
#' @rdname GeomConfBand
#' @format NULL
#' @usage NULL
#' @export
GeomConfBand <- ggplot2::ggproto("GeomConfBand", ggplot2::GeomPolygon,
                        default_aes = ggplot2::aes(colour = "black",
                                                   fill = "NA", size = 0.5,
                                                   linetype = 1, alpha = NA,
                                                   subgroup = NULL)
)




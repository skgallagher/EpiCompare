# utility functions ---------

.number_contours <- 1

## coordinate transformations ---------------

#' get xy ternary coordinates from xyz based data frame
#'
#' @description note that this does not need x,y,z to be scaled (but it should).
#'   This is just a data.frame wrapper for ggtern::tlr2xy.
#'
#' @param X_SIR data.frame with columns in xyz_col
#' @param xyz_col string vector (length 3) to match with x, y, and z. The example
#' is c(S,I,R) or c("S", "I", "R"). Both styles work
#'
#' @return X_SIR motified to have columns "x" and "y" with the ternary
#'   coordinates
#' @export
get_xy_coord <- function(X_SIR, xyz_col = c("S","I","R")){

  # quos style
  xyz_col_q <- dplyr::enquos(xyz_col)
  xyz_col <- unname(tidyselect::vars_select(dplyr::tbl_vars(X_SIR),
                                            !!!xyz_col_q))


  crd <- ggtern::coord_tern()
  xy_transform <- X_SIR %>% data.frame %>%
    dplyr::rename(x = xyz_col[1],
                  y = xyz_col[2],
                  z = xyz_col[3]) %>%
    ggtern::tlr2xy(coord = crd)
  return(xy_transform)
}

#' create a grid of points indicating near border or not (and inside or outside)
#'
#' This is internal code
#'
#' @param border_points data.frame of points from delta ball approach that are
#'   "border points"
#' @param inner_points data.frame of points from delta ball approach that are
#'   interior points.
#' @param delta float, size of delta ball radius
#' @param xrange vector, ordered values to examine in the x dimension (default
#'   is NULL - will then be created using gridbreaks)
#' @param yrange vector, ordered values to examine in the y dimension (default
#'   is NULL - will then be created using gridbreaks)
#' @param gridbreaks int, number of gridpoint in x and y dimensions if xrange or
#'   yrange is not provided
#'
#' @return data frame, with \code{expand.grid} of xrange, yrange in columns
#'   \code{x} and \code{y} and a column \code{z} that indicates if it is: (1)
#'   not within delta to border points or inner_points, (2) if closest to
#'   border_points and (3) if closest to an inner_point.
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

  z <- apply(cbind(delta, first_min, second_min), 1, which.min)

  updated_gridpoints <- gridpoints %>%
    dplyr::mutate(z = z)

  return(updated_gridpoints)
}

#' Project onto a simplex where observations in the unit simplex x
#'
#' Minimizes: \eqn{1/2 ||w-v||^2_2 \quad} s.t. \eqn{sum_i w_i = 1, w_i \geq 0}
#'
#' @param y n dimensional vector to be projected onto the simplex
#'
#' @return proj_y projection of y onto the unit simplex of dimension n
#'
#' @details This code replicates a solution referenced on
#' \href{https://math.stackexchange.com/questions/2402504/orthogonal-projection-onto-the-unit-simplex}{stackexchange},
#' which linked to following
#' \href{https://github.com/RoyiAvital/StackExchangeCodes/blob/master/Mathematics/Q2327504/ProjectSimplex.m}{Matlab
#' code}.
#' @export
#'
#' @examples
#' library(ggplot2)
#' x1 <- runif(2, -5, 5)
#' x2 <- c(.1, 1.6)
#' x3 <- c(.1, 1.1)
#' x4 <- c(.1,.1)
#'
#' x_vals <- list(x1, x2, x3, x4)
#'
#' proj_xs <- lapply(x_vals, project_onto_simplex)
#'
#' vis_list <- list()
#' for (idx in 1:4){
#'   x <- x_vals[[idx]]
#'   proj_x <- proj_xs[[idx]]
#'   data1 <- data.frame(X = x[1], Y = x[2],
#'                       X_proj = proj_x[1],
#'                       Y_proj = proj_x[2])
#'
#'   data_simplex <- data.frame(X_low = 0,
#'                              Y_low = 1,
#'                              X_high = 1,
#'                              Y_high = 0)
#'
#'   vis_list[[idx]] <- ggplot() + geom_segment(data = data1,
#'                                         aes(x = X, y = Y,
#'                                             xend = X_proj,
#'                                             yend = Y_proj)) +
#'     geom_point(data = data1, aes(x = X, y = Y)) +
#'     geom_point(data = data1, aes(x = X_proj, y = Y_proj), color = "blue") +
#'     geom_segment(data = data_simplex, aes(x = X_low, y = Y_low,
#'                                           xend = X_high,
#'                                           yend = Y_high), color = "blue") +
#'     coord_fixed()
#' }
#' gridExtra::grid.arrange(grobs = vis_list, nrow = 2)
project_onto_simplex <- function(y){
  n <- length(y)
  bget <- FALSE

  s <- sort(y, decreasing = T)
  tmpsum <- 0

  for (idx in 1:(n-1)) {
    tmpsum <- tmpsum + s[idx]
    tmax <- (tmpsum - 1)/(idx)
    if (tmax >= s[idx+1]){
      bget <- TRUE
      break
    }
  }

  if (!bget){
    tmax <- (tmpsum + s[n] - 1)/n
  }

  proj_y <- (y - tmax) * ( y - tmax > 0)

  return(proj_y)
}

project_simplex_vec <- function(x){t(apply(x, 1, project_onto_simplex))}

#' project onto a standard 3d simplex.
#'
#' @param df_3d data frame
#' @param column_names names of columns for the 3 dimensions. Can be in the
#' form \code{c(x,y,z)} or \code{c("x","y","z")}.
#'
#' @return an updated version of \code{df_3d} with points projected onto
#' simplex.
#' @export
project_to_simplex <- function(df_3d, column_names = c("x","y","z") ){
  # quos style
  column_names_q <- dplyr::enquos(column_names)
  column_names <- unname(tidyselect::vars_select(dplyr::tbl_vars(df_3d),
                                                 !!!column_names_q))

  df_3d_inner <- df_3d %>% dplyr::select(dplyr::one_of(column_names))
  df_3d_inner <- project_simplex_vec(df_3d_inner)
  df_3d[,column_names] <- df_3d_inner
  return(df_3d)
}

#' assert if observation is inside elipsoid
#'
#' See
#' https://stats.stackexchange.com/questions/29860/confidence-interval-of-multivariate-gaussian-distribution
#' for details.
#'
#' @param data data.frame or matrix of data (row is observation), ncol = p,
#'   scalar columns
#' @param Sigma covariance matrix (p x p)
#' @param center center of elipsoid (vector length p)
#' @param bound contraint for equation of ellipsoid
#' @param suppress_warning logic to suppress warning if just returning all
#'   \code{FALSE} relative to non PSD Sigma or bound <= 0.
#'
#' @return boolean vector if data is contained in ellipsoid. All \code{FALSE} if
#'   Sigma not PSD
#' @export
check_inside_elipsoid <- function(data, Sigma, center, bound,
                                  suppress_warning = FALSE){
  if (det(Sigma) <= 0 | bound <= 0){
    if(!suppress_warning){
    warning("Sigma is not PSD or bound <= 0")
    }
    return(rep(FALSE, nrow(data)))
  }
  m_data <- as.matrix(data)
  m_dist <- stats::mahalanobis(x = m_data,
                               center = center,
                               cov = Sigma)
  return(m_dist <= bound)
}

#' create a function assert if observations are inside elipsoid
#'
#' @param Sigma covariance matrix (p x p)
#' @param center center of elipsoid (vector length p)
#' @param bound contraint for equation of ellipsoid
#' @param suppress_warning logic to suppress warning if just returning all false
#' relative to non PSD Sigma or bound <= 0.
#'
#' @return check_inside_elipsoid_function, that takes in \code{data} only
check_inside_elipsoid_func <- function(Sigma, center, bound,
                                       suppress_warning = FALSE){
  check_inside_elipsoid_function <- function(data){
    return(check_inside_elipsoid(data, Sigma, center, bound,
                                 suppress_warning = suppress_warning))
  }
  return(check_inside_elipsoid_function)
}


#' create a grid of points indicating whether in a set of 2d elipsoids
#'
#' @details See \code{check_inside_elipsoid} for functional idea
#'
#' @param inside_func_list list of functions that assess if observation is in
#'   elipsoid (technically just need a function that takes in 2d
#'   \code{data.frame}s and returns boolean values for each row )
#' @param xrange vector, ordered values to examine in the x dimension
#' @param yrange vector, ordered values to examine in the y dimension
#'
#' @return updated_gridpoints (defined by yrange, xrange) with indication column
#'   \code{included} if gridpoint is contained.
get_grid_elipsoid_containment <- function(inside_func_list,
                                          xrange, yrange){

  gridpoints <- expand.grid(xrange, yrange) %>%
    dplyr::rename(x = "Var1", y = "Var2")

  containment <- inside_func_list %>% sapply(function(f){f(gridpoints)})
  updated_gridpoints <- gridpoints %>%
    cbind(included = containment %>% apply(MARGIN = 1, max))

  return(updated_gridpoints)
}


#' checks the provided dist_params to make sure they are what is expected 
#'
#' @param dist_params named list of parameters
#' @param data data for visual 
#'
#' @return error or updated dist_params
check_dist_params <- function(dist_params, data){
  if (dist_params[["dist_approach"]] == "auto"){
    dist_params[["dist_approach"]] <- "equa_dist"
  }
  if (dist_params[["dist_approach"]] == "equa_dist" & 
      dist_params[["num_steps"]] == "auto"){
    dist_params[["num_steps"]] <- 20
  }
  if (dist_params[["dist_approach"]] == "equa_dist"){
    x <- dist_params[["num_steps"]]
    tol <- 2*.Machine$double.eps
    assertthat::assert_that(min(abs(c(x %% 1, x %% 1-1))) < tol,
                            msg = paste("'dist_params$num_steps' must
                                          be an positive integer or string 
                                          'auto'."))
  }
  
  if (dist_params[["dist_approach"]] == "temporal"){
    counts <- data %>% as.data.frame() %>% 
      dplyr::group_by(.data$sim_group) %>%
      dplyr::summarize(total = dplyr::n())
    assertthat::assert_that(length(unique(counts$total)) == 1,
                            msg = paste("if dist_params$dist_approach is 
                                          'temporal', then every path must
                                          have the same number of points."))
  }
  return(dist_params)
}


# stats and geoms -------------------------

#' stat object for use in kde based stat_prediction_band and
#' geom_prediction_band
#' @export

StatPredBandKDE <- ggplot2::ggproto("StatPredBandKDE",
  ggplot2::Stat,
  compute_layer = function(self, data, params, layout){
    # first run the regular layer calculation to infer densities

    data <- ggplot2::ggproto_parent(ggplot2::Stat,self = self)$compute_layer(data, params, layout)

    # required piece and group to be cleaned up
    data_cleaned_up <- data %>% dplyr::mutate(piece_old = .data$piece,
                                       group_old = .data$group,
                                       piece = as.numeric(
                                         factor(paste(.data$PANEL,
                                                      .data$piece_old,
                                                      .data$group_old,
                                                      sep = "*"))),
                                       group = piece)

    return(data_cleaned_up)
  },
  compute_group = function(data, scales, params,
                           pb_type = NULL,
                           #^ needed to match same format as stat/geom_prediction_band
                           grid_size = rep(100,2),
                           conf_level = .9, over_delta = NULL,
                           dist_params = NULL){
    assertthat::assert_that(!is.factor(data$sim_group),
                            msg = paste("'sim_group' cannot be a factor"))

    info_inner <- data[, c("PANEL", "group")] %>% sapply(unique)

    data <- data %>% dplyr::mutate(sim_group = factor(sim_group))

    data2d <- data %>% get_xy_coord(xyz_col = c("x", "y", "z"))

    data2d_list <- split(x = data2d, f = data2d$sim_group)

    xy_position <- which(names(data2d_list[[1]]) %in% c("x","y"))
    #kde style
    kde_ci_list <- kde_from_list(dflist = data2d_list,
                                 grid_size = grid_size,
                                 alpha = 1 - conf_level, # switch from alpha to conf_level
                                 position = xy_position)

    kde_ci_df <- kde_ci_list %>% lapply(as.data.frame) %>%
      dplyr::bind_rows(.id = "kde_poly")

    kde_ci_df3 <- ggtern::xy2tlr(data = kde_ci_df %>%
                                   dplyr::select(-kde_poly, -level),
                                 coord = ggtern::coord_tern()) %>%
      cbind(., piece = as.integer(kde_ci_df$kde_poly)) %>%
      dplyr::mutate(PANEL = info_inner[1],
                    group = info_inner[2]) %>%
      # ^this seems like an odd approach
      project_to_simplex(column_names = c("x","y","z"))

    return(kde_ci_df3)
  },
  required_aes = c("x", "y", "z", "sim_group"))


#' stat object for use in delta_ball based stat_prediction_band and
#' geom_prediction_band
#' @export
StatPredBandDeltaBall <- ggplot2::ggproto("StatPredBandDeltaBall",
  ggplot2::Stat,
  compute_layer =
    function(self, data, params, layout){
      # first run the regular layer calculation to infer densities
      
      data <- ggplot2::ggproto_parent(ggplot2::Stat,self = self)$compute_layer(data, params, layout)

      # required piece and group to be cleaned up
      data_cleaned_up <- data %>% dplyr::mutate(piece_old = .data$piece,
                                         group_old = .data$group,
                                         piece = as.numeric(
                                           factor(paste(.data$PANEL,
                                                        .data$piece_old,
                                                        .data$group_old,
                                                        sep = "*"))),
                                         group = piece)

      return(data_cleaned_up)
  },
  compute_group =
  function(data, scales, params,
           pb_type = NULL,
           #^ needed to match same format as stat/geom_prediction_band
           grid_size = rep(100,2),
           conf_level = .9, over_delta = .1,
           dist_params = list("dist_approach"= "auto",
                          "num_steps" = "auto")){
    ## Checks
    # sim_group input
    assertthat::assert_that(!is.factor(data$sim_group),
                            msg = paste("'sim_group' cannot be a factor"))
    # distance parameters 
    dist_params <- check_dist_params(dist_params, data)
    

    info_inner <- data[, c("PANEL", "group")] %>%
      sapply(unique) %>% unname

    data2d <- data %>% as.data.frame() %>%
      get_xy_coord(xyz_col = c("x", "y", "z"))

    if (dist_params$dist_approach == "equa_dist"){ 
      data2d_equa_dist <- data2d %>%
        dplyr::group_by(.data$sim_group) %>%
        filament_compression(data_columns = c("x","y"), 
                             number_points = dist_params$num_steps) 
      
      data2d_list <- split(x = data2d_equa_dist, f = data2d_equa_dist$sim_group)
      xy_position <- which(names(data2d_list[[1]]) %in% c("x","y"))
      
      
      dist_mat <- dist_matrix_innersq_direction(data2d_list,
                                                position = xy_position,
                                                verbose = FALSE)
      
      data2d_list <- split(x = data2d, f = data2d$sim_group)
      
    } else {
      
      data2d_list <- split(x = data2d, f = data2d$sim_group)
      xy_position <- which(names(data2d_list[[1]]) %in% c("x","y"))
      
      
      dist_mat <- dist_matrix_innersq_direction(data2d_list,
                                                position = xy_position,
                                                verbose = FALSE)
    }
    

    data_deep_points <- depth_curves_to_points(data2d_list,
                                               alpha = 1 - conf_level, # switch from alpha to conf_level
                                               dist_mat = dist_mat)

    delta_info <- delta_structure(data_deep_points)

    structure <- delta_info$structure

    delta <- delta_info$delta

    inner_df <- dplyr::setdiff(data_deep_points %>%
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
      group = info_inner[2],
      PANEL = info_inner[1]
    ) %>%
      ggtern::xy2tlr(coord = ggtern::coord_tern()) %>%
      project_to_simplex(column_names = c("x","y","z"))

    return(vis_df)

  },
  required_aes = c("x", "y", "z", "sim_group"))


#' stat object for use in spherical ball based stat_prediction_band and
#' geom_prediction_band
#' @export
StatPredBandSpherical <- ggplot2::ggproto("StatPredBandSpherical",
   ggplot2::Stat,
   compute_layer =
     function(self, data, params, layout){
       # first run the regular layer calculation to infer densities
       data <- ggplot2::ggproto_parent(ggplot2::Stat,self = self)$compute_layer(data, params, layout)

       # required piece and group to be cleaned up
       data_cleaned_up <- data %>% dplyr::mutate(piece_old = .data$piece,
                                          group_old = .data$group,
                                          piece = as.numeric(
                                            factor(paste(.data$PANEL,
                                                         .data$piece_old,
                                                         .data$group_old,
                                                         sep = "*"))),
                                          group = piece)

       return(data_cleaned_up)
     },
   compute_group =
     function(data, scales, params,
              pb_type = NULL,
              #^ needed to match same format as stat/geom_prediction_band
              grid_size = rep(100,2),
              conf_level = .9, over_delta = .1,
              dist_params = NULL){
        assertthat::assert_that(!is.factor(data$t),
                                msg = paste("'t' cannot be a factor"))

        info_inner <- data[, c("PANEL", "group")] %>%
          sapply(unique) %>% unname()

        data2d <- data %>% as.data.frame() %>%
          get_xy_coord(xyz_col = c("x", "y", "z"))

        x_dim <- 2

        a <- data2d %>% dplyr::group_by(t) %>%
          tidyr::nest() %>%
          dplyr::mutate(n = purrr::map(data, function(df){nrow(df)}),
                 mean = purrr::map(data,function(df){df %>%
                     dplyr::select(x,y) %>%
                     sapply(mean)}),
                 Sigma = purrr::map(data, function(df){df %>%
                     dplyr::select(x,y) %>%
                     cov})
          ) %>%
          dplyr::mutate(
            bound = purrr::map(n, function(n) {
              return((x_dim * (n-1)) / (n - x_dim) *
                       pf(q = conf_level, # switch from alpha to conf_level
                          df1 = x_dim, df2 = n - x_dim))})) %>%
          dplyr::mutate(inside_func = purrr::pmap(list(bound, mean, Sigma),
                  function(bound, mean, Sigma) {
                    check_inside_elipsoid_func(Sigma, mean, bound,
                                               suppress_warning = TRUE)}))

        xrange <- seq(min(data2d$x) - over_delta,
                      max(data2d$x) + over_delta,
                      length.out = grid_size[1])

        yrange <- seq(min(data2d$y) - over_delta,
                      max(data2d$y) + over_delta,
                      length.out = grid_size[2])

        updated_gridpoints <- get_grid_elipsoid_containment(a$inside_func,
                                                            xrange = xrange,
                                                            yrange = yrange)

        if (tidyr_new_interface()){
          update_gridpoints_mat <- tidyr::pivot_wider(updated_gridpoints,
                                                      names_from = "y",
                                                      values_from = "included"
          ) %>%
            dplyr::select(-x) %>% as.matrix
        } else {
          update_gridpoints_mat <- tidyr::spread(updated_gridpoints,
                                                 key = "y",
                                                 value = "included") %>%
            dplyr::select(-x) %>% as.matrix
        }

        cl <- grDevices::contourLines(x = xrange, y = yrange,
                                      z = update_gridpoints_mat,levels = c(1))

        lengths <- vapply(cl, function(x) length(x$x), integer(1))
        xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
        ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
        pieces <- rep(seq_along(cl), lengths)

        vis_df <- data.frame(
          x = xs,
          y = ys,
          piece = pieces,
          group = info_inner[2],
          PANEL = info_inner[1]
        ) %>%
        ggtern::xy2tlr(coord = ggtern::coord_tern()) %>%
          project_to_simplex(column_names = c("x","y","z"))

        return(vis_df)

      },
   required_aes = c("x", "y", "z", "t"))


#' stat object for use in convex hull based stat_prediction_band and
#' geom_prediction_band
#' @export
StatPredBandConvexHull <- ggplot2::ggproto("StatPredBandConvexHull",
    ggplot2::Stat,
    compute_group =
      function(data, scales, pb_type = NULL, grid_size = NULL,
               conf_level = .9, over_delta = NULL,
               dist_params = list("dist_approach"= "auto",
                               "num_steps" = "auto")){
        ## Checks
        # sim_group input
        assertthat::assert_that(!is.factor(data$sim_group),
                                msg = paste("'sim_group' cannot be a factor"))
        # distance parameters 
        dist_params <- check_dist_params(dist_params, data)
        

        info_inner <- data[, c("PANEL", "group")] %>%
          sapply(unique)

        data2d <- data %>% as.data.frame() %>%
          get_xy_coord(xyz_col = c("x", "y", "z"))

        
        if (dist_params$dist_approach == "equa_dist"){ 
          data2d_equa_dist <- data2d %>%
            dplyr::group_by(.data$sim_group) %>%
            filament_compression(data_columns = c("x","y"), 
                                 number_points = dist_params$num_steps) 
          
          data2d_list <- split(x = data2d_equa_dist, 
                               f = data2d_equa_dist$sim_group)
          xy_position <- which(names(data2d_list[[1]]) %in% c("x","y"))
          
          
          dist_mat <- dist_matrix_innersq_direction(data2d_list,
                                                    position = xy_position,
                                                    verbose = FALSE)
          
          data2d_list <- split(x = data2d, f = data2d$sim_group)
          
        } else {
          
          data2d_list <- split(x = data2d, f = data2d$sim_group)
          xy_position <- which(names(data2d_list[[1]]) %in% c("x","y"))
          
          
          dist_mat <- dist_matrix_innersq_direction(data2d_list,
                                                    position = xy_position,
                                                    verbose = FALSE)
        }
        
        data_deep_points <- depth_curves_to_points(
          data2d_list, alpha = 1 - conf_level, # switch from alpha to conf_level
          dist_mat = dist_mat) %>%
          dplyr::select(x,y)

        chull_ids <- data_deep_points %>% chull()

        chull_ci_df <- data_deep_points[c(chull_ids, chull_ids[1]),]


        chull_ci_df3 <- ggtern::xy2tlr(data = chull_ci_df,
                                     coord = ggtern::coord_tern()) %>%
          dplyr::mutate(PANEL = info_inner[1],
                        piece = info_inner[2],
                        group = info_inner[2]) %>%
          # ^this seems like an odd approach, but needed...
          project_to_simplex(column_names = c("x","y","z"))

        return(chull_ci_df3)            },
    required_aes = c("x", "y", "z", "sim_group"))


#' @export
#' @rdname geom_prediction_band
stat_prediction_band <- function(mapping = NULL, data = NULL, geom = "polygon",
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE,
                                 pb_type = c("delta_ball", "kde", 
                                             "spherical_ball",
                                             "convex_hull"),
                                 grid_size = rep(100, 2),
                                 conf_level = .9,
                                 over_delta = .1,
                                 dist_params = list("dist_approach"= "auto",
                                                 "num_steps" = "auto"),
                                 ...) {

  if (length(pb_type) > 1){
    pb_type <- pb_type[1]
  }

  assertthat::assert_that(pb_type %in% c("delta_ball", "kde", 
                                         "spherical_ball", "convex_hull"),
                          msg = paste("bc_type needs to either be 'kde' or",
                                      "'delta_ball' or 'spherical_ball' or",
                                      "'convex_hull'."))

  ggplot2::layer(
    stat = list(StatPredBandDeltaBall,
                StatPredBandKDE,
                StatPredBandSpherical,
                StatPredBandConvexHull)[
                  which(c("delta_ball", "kde", 
                          "spherical_ball", "convex_hull") == pb_type)
                  ][[1]],
    data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, conf_level = conf_level,
                  grid_size = grid_size, dist_params = dist_params, ...)
  )
}


#' The prediction_band geom/stat
#'
#'
#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. If specified and
#'   \code{inherit.aes = TRUE} (the default), it is combined with the default
#'   mapping at the top level of the plot. You must supply mapping if there is
#'   no plot mapping
#' @param data The data to be displayed in this layer. There are three options:
#'   If \code{NULL}, the default, the data is inherited from the plot data as
#'   specified in the call to \code{\link[ggplot2]{ggplot}}.
#'
#'   A \code{data.frame}, or other object, will override the plot data. All
#'   objects will be fortified to produce a data frame. See
#'   \code{\link[ggplot2]{fortify}} for which variables will be created.
#'
#'   A \code{function} will be called with a single argument, the plot data. The
#'   return value must be a \code{data.frame}, and will be used as the layer
#'   data. A function can be created from a formula (e.g. \code{~ head(.x,
#'   10)}).
#' @param geom string associated with desired geom. \code{stat} is otherwise
#'   controlled by the \code{pb_type} parameter.
#' @param stat string associated with desired stat \code{geom} is otherwise
#'   controlled by the \code{pb_type} parameter.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a
#'   warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes. It can also be a named
#'   logical vector to finely select the aesthetics to display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param pb_type String indicating which prediction band type to use. Currently
#'   only \code{"kde"} and \code{"delta_ball"} inputs are expected. See details
#'   for more information.
#' @param grid_size integer vector, length 2. Size of the grid which is going to
#'   be used to approximate prediction band (if needed). Can be reduced to
#'   speed-up computation.
#' @param conf_level confidence level for prediction band. Aka, with \code{alpha
#'   = 1-conf_level}, it creates a \code{1 - alpha} level prediction band.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}. These
#'   are often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{colour = "red"} or \code{size = 3}. They may also be parameters to
#'   the paired \code{geom}/\code{stat}.
#' @param over_delta defines small extension of box around actual points to define
#' contour.
#' @param dist_params list of parameters for distance based approaches (convex 
#' hull and delta ball). Named parameters are \code{dist_approach} with options
#' \code{c("auto", "temporal", "equa_dist")}, and \code{num_steps} as either
#' an integer or "auto". 
#'
#' @details
#'
#' This stat/geom can create 1 of 4 prediction band structures. These approaches
#' can be broken into 2 subgroups, "pointwise" and "uniform" prediction bands.
#' The rational for these splits relate to containment properties and the
#' 'original' ideas are discussed more here:
#' \href{https://arxiv.org/abs/1906.08832}{Arvix: 1906.08832}
#'
#' \strong{Pointwise}:
#'
#' \itemize{
#' \item \code{spherical_ball}: This prediction band is defined
#' relative to the time points that paths take. For each time point, we take a
#' ellipsoid defined by the prediction region that would contain
#' \code{conf_level} probability mass if the distribution of points were a
#' multivariate gaussian. We then take a union of all these ellipsoids to create
#' the full prediction band.
#' \item \code{kde}: This prediction band is defined as the kde level contour
#' for \code{conf_level} relative to all points.
#' }
#'
#' \strong{Uniform}:
#'
#' These approaches focus on containing the paths/curves/filaments in
#' uniformity. This approach uses depth (specifically a distance-based depth
#' developed by Geenens & Nieto-Reyes, 2017), to select to top
#' \code{conf_level} curves and then creates a geometric representation of
#' where the curves lie.
#'
#' \itemize{
#' \item \code{delta_ball}: relative to all the points in the top
#' \code{conf_level} curves, we find the minimum delta such all of these points
#' are contained in at least 1 ball around another point with radius delta. This
#' can be mathematically expressed as: \eqn{\delta = \max_{i} \min_{j} d(x_i,
#' x_j)}. Then we take the union of delta-balls surround all the points as the
#' prediction band.
#'
#' \item \code{convex_hull}: with to all the points in the top
#' \code{conf_level} curves we just create a convex hull and define our
#' prediction band as such.
#' }
#'
#' @section Aesthetics: \code{stat_prediction_band}/\code{geom_prediction_band}
#'  understands the following aesthetics (required aesthetics are in bold):
#'
#'   \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{z}}
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{group}
#'   \item \code{linetype}
#'   \item \code{size}}
#'
#'   For prediction band types = "kde", "delta_ball": \itemize{ \item
#'   \strong{\code{sim_group}} - note: this cannot be a factor } For prediction
#'   band type = "spherical_balls": \itemize{ \item \strong{\code{t}} - note:
#'   this cannot be a factor }
#'
#'   Learn more about setting these aesthetics in
#'   \code{vignette("ggplot2-specs")}.
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(ggtern); EpiCompare:::update_approved_layers()
#' #                ^ this doesn't generally need to be done
#'

#' # for speed purposes
#' smaller_pomp_df <- EpiCompare::pomp_df %>% filter(.id < 10)
#'
#' vis_data <- smaller_pomp_df %>%
#'  rename(x = "S", y = "I", z = "R") %>%
#'  ggplot(aes(x = x, y =y, z = z, group = .id)) +
#'  geom_path(alpha = .3) +
#'  coord_tern() +
#'  labs(title = "Actually data paths")
#'
#'vis_spherical <- smaller_pomp_df %>%
#'  rename(x = "S", y = "I", z = "R", t = "time") %>%
#'  ggplot(aes(x = x, y = y, z = z, t = t)) +
#'  geom_prediction_band(pb_type = "spherical_ball",
#'                       grid_size = rep(100,2),
#'                       conf_level = .95) +
#'  coord_tern() +
#'  labs(title = "Spherical CB")
#'
#'vis_delta_ball <- smaller_pomp_df %>%
#'  rename(x = "S", y = "I", z = "R") %>%
#'  mutate(.id = as.numeric(.id)) %>%
#'  ggplot(aes(x = x, y = y, z = z, sim_group = .id)) +
#'  geom_prediction_band(pb_type = "delta_ball",
#'                       grid_size = rep(100,2),
#'                       conf_level = .95) +
#'  coord_tern() +
#'  labs(title = "Delta-ball CB")
#'
#'vis_kde <- smaller_pomp_df %>%
#'  rename(x = "S", y = "I", z = "R") %>%
#'  mutate(.id = as.numeric(.id)) %>%
#'  ggplot(aes(x = x, y = y, z = z, sim_group = .id)) +
#'  geom_prediction_band(pb_type = "kde",
#'                       grid_size = rep(100,2),
#'                       conf_level = .95) +
#'  coord_tern() +
#'  labs(title = "KDE CB")
#'
#'vis_convex_hull <- smaller_pomp_df %>%
#'  rename(x = "S", y = "I", z = "R") %>%
#'  mutate(.id = as.numeric(.id)) %>%
#'  ggplot(aes(x = x, y = y, z = z, sim_group = .id)) +
#'  geom_prediction_band(pb_type = "convex_hull",
#'                       conf_level = .95) +
#'  coord_tern() +
#'  labs(title = "Convex hull CB")
#'
#'grid.arrange(vis_data, vis_spherical,
#'             vis_delta_ball, vis_kde,
#'             vis_convex_hull, nrow = 2)
geom_prediction_band <- function(mapping = NULL, data = NULL,
                                 stat = list("PredBandDeltaBall",
                                             "PredBandKDE",
                                             "PredBandSpherical",
                                             "PredBandConvexHull")[
                                               c("delta_ball", "kde", 
                                                 "spherical_ball",
                                                 "convex_hull") == pb_type
                                               ][[1]],
                                 position = "identity",
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 pb_type = c("delta_ball", "kde", 
                                             "spherical_ball",
                                             "convex_hull"),
                                 grid_size = rep(100, 2),
                                 conf_level = .9,
                                 over_delta = .1,
                                 dist_params = list("dist_approach"= "auto",
                                                 "num_steps" = "auto"),
                                 ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPredBand,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, grid_size = grid_size, conf_level = conf_level,
      over_delta = over_delta,
      dist_params = dist_params,
      ...
    )
  )
}

#' GeomPredBand
#' @rdname GeomPredBand
#' @format NULL
#' @usage NULL
#' @export
GeomPredBand <- ggplot2::ggproto("GeomPredBand", ggplot2::GeomPolygon,
                        default_aes = ggplot2::aes(colour = "black",
                                                   fill = "NA", size = 0.5,
                                                   linetype = 1, alpha = NA,
                                                   subgroup = NULL)
)



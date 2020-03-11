# These functions will need to be move around (but probably should think about
# if there are redundant functions now)


#' Compress filaments to filaments with the same number of points (equally
#' linearly space compared to original filament definition)
#'
#' @param grouped_df grouped_df data.frame object (assumed rows per filament are
#'   ordered) - grouped per each filament
#' @param data_columns columns (currently strings) of data.frame that relate to
#'   the filament's coordinates in euclidean space.
#' @param number_points integer number of points for each filament to be
#'   compressed to
#'
#' @return updated grouped_df with new rows so that each filament has the same
#'   number of points.
#' @export
#'
#' @examples
#' library(dplyr)
#' t13compression <- timeternR::pomp_sir %>%
#'   arrange(time) %>% # just to be safe
#'   select(-H, -cases, -time) %>%
#'   filter(.id <= 5) %>%
#'   group_by(.id) %>%
#'   filament_compression()
#'
#' t9compression <- timeternR::pomp_sir %>%
#'   filter(.id <= 5) %>%
#'   group_by(.id) %>%
#'   filament_compression(data_columns = c("S","I","R"), number_points = 9)
filament_compression <- function(grouped_df, data_columns = NULL,
                                 number_points = 13){
  group_columns <- names(attr(grouped_df, "groups"))[names(attr(grouped_df, "groups")) != ".rows"]
  if (is.null(data_columns)){ # use all columns except the grouped columns
    data_columns <- names(grouped_df)
    data_columns <- data_columns[!(data_columns %in% group_columns)]
  }

  if (tidyr_new_interface()){
    compression_df <- grouped_df %>%
      tidyr::nest() %>%
      dplyr::mutate(data =
                      purrr::map(.data$data,
                                 function(df) {equa_dist_points_direction(
                                   df[, data_columns],num_splits = number_points)})) %>%
      tidyr::unnest(cols = c(.data$data))
  } else {

    compression_df <- grouped_df %>%
      tidyr::nest() %>%
      dplyr::mutate(data =
                      purrr::map(.data$data,
                                 function(df) {equa_dist_points_direction(
                                   df[, data_columns], num_splits = number_points)})) %>%
      tidyr::unnest()

    if (length(group_columns) == 1) {
      group_sym <- dplyr::sym(group_columns)
      compression_df <- compression_df %>% dplyr::group_by(!!group_sym)
    }
    if (length(group_columns) > 1) {
      group_sym <- dplyr::syms(group_columns)
      compression_df <- compression_df %>% dplyr::group_by(!!!group_sym)
    }

  }


  return(compression_df)
}

#' Calculate filament depth (relative to distance depth)
#'
#' @param grouped_df grouped_df data.frame object (assumed rows per filament are
#'   ordered) - grouped per each filament
#' @param data_columns  columns (currently strings) of data.frame that relate to
#'   the filament's coordinates in euclidean space.
#' @param dist_list_func function to calculate distance between each filament.
#'   The default is "auto", which is selects either
#'   \code{dist_matrix_innersq_direction} if all filaments are the same length
#'   -or- preforms a 13 point compression of each filament using
#'   \code{filament_compression}.
#' @param depth_func function to distances observed between filaments, default
#'   is \code{distance_depth_function}
#'
#' @return depth_vector (of depth related to each filament)
#' @export
filament_distance_depth <- function(grouped_df,
                                    data_columns = NULL,
                                    dist_list_func = "auto",
                                    depth_func = distance_depth_function){

  if (is.null(data_columns)){ # use all columns except the grouped columns
    data_columns <- names(grouped_df)
    group_columns <- names(attr(grouped_df, "groups"))[names(attr(grouped_df, "groups")) != ".rows"]
    data_columns <- data_columns[!(data_columns %in% group_columns)]
  }

  if (dist_list_func == "auto"){
    number_of_lengths_of_filaments <- grouped_df %>%
      dplyr::summarize(count = dplyr::n()) %>% dplyr::pull(.data$count) %>%
      unique %>% length

    if (number_of_lengths_of_filaments != 1){
      message(paste("Compressing different length filaments using",
                    "'filament_compression' with 13 points, assuming",
                    "points are ordered. If not ordered, we encourage",
                    "doing dplyr::arrange(ordering_column) before passing in."))

      grouped_df <- grouped_df %>%
        filament_compression(data_columns = data_columns)
    }

    dist_list_func <-
      function(l) {dist_matrix_innersq_direction(l,
                                                 which(names(grouped_df) %in% data_columns),
                                                 verbose = FALSE)}
  }


  df_list <- grouped_df %>% dplyr::group_split()

  dist_mat <- df_list %>% dist_list_func()

  depth_vec <- depth_func(dist_mat)

  return(depth_vec)
}

#' Select a proportion of the top / most deep filaments
#'
#' similar to \code{depth_curves_to_points.grouped_df}
#' @param grouped_df grouped_df data.frame object (assumed rows per filament are
#'   ordered) - grouped per each filament
#' @param data_columns  columns (currently strings) of data.frame that relate to
#'   the filament's coordinates in euclidean space.
#' @param filament_depth_function function to calculate depth relative to the
#'   filaments in the \code{grouped_df}. Will take in \code{data_columns}
#'   parameter as well
#' @param alpha_level proportion of filaments to keep
#'
#' @return updated \code{grouped_df} with only the top depth filaments
#' @export
#'
#' @examples
#' library(dplyr)
#' top_filaments <- timeternR::pomp_df %>% group_by(.id) %>%
#'   filter(.id <= 10) %>%
#'   grab_top_depth_filaments(data_columns =c("S","I","R"),
#'                            alpha_level = .5)
#'
#' # note that the below doesn't contain all filaments - as some have 0 depth
#' all_but_extreme_filaments <- timeternR::pomp_df %>% group_by(.id) %>%
#'   filter(.id <= 10) %>%
#'   grab_top_depth_filaments(data_columns = c("S","I","R"),
#'                            alpha_level = 0)
grab_top_depth_filaments <- function(grouped_df, data_columns=NULL,
                                     filament_depth_function = filament_distance_depth,
                                     alpha_level = .95){
  depth_vector <- grouped_df %>%
    filament_depth_function(data_columns = data_columns)

  num_top <- sum(depth_vector > stats::quantile(depth_vector, probs = alpha_level))

  if (num_top == 0){
    stop("selected 'alpha_level' returns 0 filaments")
  }

  if (tidyr_new_interface()){
    updated_df <- grouped_df %>% tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(depth = depth_vector) %>%
      dplyr::filter(.data$depth >
                      stats::quantile(depth_vector, probs = alpha_level)) %>%
      dplyr::select(-.data$depth) %>%
      tidyr::unnest(cols = dplyr::everything())
  } else {
    updated_df <- grouped_df %>% tidyr::nest() %>%
      dplyr::mutate(depth = depth_vector) %>%
      dplyr::filter(.data$depth >
                      stats::quantile(depth_vector, probs = alpha_level)) %>%
      dplyr::select(-.data$depth) %>%
      tidyr::unnest()
  }
  return(updated_df)
}

# final part of pipeline

#' create the delta ball associated with points
#'
#' @param data_points points to create the delta ball structure from
#' @param data_columns  columns (currently strings) of data.frame that relate to
#'   the point's coordinates in euclidean space.
#'
#' @return a \code{delta_ball_structure} object, that is a data frame similar to
#' data_points (but with out \code{data_columns} columns) and distinct points.
#' This object also has an \code{delta} attribute which contains the delta
#' defining the delta ball.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' delta_ball_cb <- timeternR::pomp_df %>%
#'   filter(.id <= 10) %>%
#'   arrange(time) %>% # just to be safe
#'   select(-time, -H, -cases) %>%
#'   group_by(.id) %>%
#'   grab_top_depth_filaments(alpha_level = .9) %>%
#'   create_delta_ball_structure()
create_delta_ball_structure <- function(data_points, data_columns=NULL){
  if (!is.null(data_columns)){
    delta_ball_info <- data_points %>%
      dplyr::select(dplyr::one_of(data_columns))
  }

  delta_ball_info <- data_points %>% dplyr::distinct()

  a <- get_delta(data = delta_ball_info)
  delta <- a[[2]]

  attr(delta_ball_info, "delta") <- delta

  class(delta_ball_info) <- c("delta_ball_structure", class(delta_ball_info))

  return(delta_ball_info)
}

#' create the convex hull associated with points
#'
#' I believe this will error unless one projects from the simplex to the lower
#' dimensional space
#'
#' @param data_points points to create the convex hull structure from
#' @param data_columns columns (currently strings) of data.frame that relate to
#'   the point's coordinates in euclidean space.
#'
#' @return a \code{convex_hull_structure} object, that is a data frame similar
#'   to data_points (but with out \code{data_columns} columns) and only points
#'   that define the convex hull. This object also has an \code{normals}
#'   attribute which contains a matrix with the first p columns defining "A"
#'   and the last defining "b", such that -Ax >= b defines the convex hull.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' # actually needs to reduce dimension first :/
#' if (FALSE){
#' convex_hull_cb <- timeternR::pomp_df %>%
#'   filter(.id <= 10) %>%
#'   arrange(time) %>% # just to be safe
#'   select(-time, -H, -cases) %>%
#'   group_by(.id) %>%
#'   grab_top_depth_filaments(alpha_level = .9) %>%
#'   create_convex_hull_structure()
#'   }
create_convex_hull_structure <- function(data_points, data_columns = NULL){
  # return object of data_points[chull_idx, data_columns]
  if (is.null(data_columns)){
    data_columns = names(data_points)
  }

  inner_data <- data_points %>%
    dplyr::select(dplyr::one_of(data_columns)) %>% dplyr::distinct()

  chulln <- geometry::convhulln(inner_data,
                                output.options = "n")
  points_on_hull <- data_points[unique(matrix(chulln$hull)),]

  class(points_on_hull) <- c("convex_hull_structure", class(points_on_hull))

  attr(points_on_hull, "normals") <- chulln$normals

  return(points_on_hull)
}

#' Calculates the Hausdorff distance between 2 sets
#'
#' Note for delta-ball structure, this could over estimate by the
#' maximum of the 2 defining delta radi, whereas the convex hull structures'
#' hausdorff distance is exact.
#'
#' @param a assumed to either \code{convex_hull_structure} or
#'   \code{delta_ball_structure} objects
#' @param b same class as a
#'
#' @return hausdorff distance between 2 sets
#' @export
hausdorff_dist <- function(a,b){
  # could also use pracma::hausdorff_dist

  if (!((inherits(a, "delta_ball_structure") &
         inherits(b, "delta_ball_structure")) |
        (inherits(a, "convex_hull_structure") &
         inherits(b, "convex_hull_structure")))){
    stop(paste("both a and b must be of the same structure type for now."))
  }

  d_1 <- d_2 <- 0

  if (inherits(a, "delta_ball_structure")){
    d_1 <- attr(a, "delta")
  }
  if (inherits(b, "delta_ball_structure")){
    d_2 <- attr(b, "delta")
  }



  dist_mat <- rdist::cdist(a, b, metric = "euclidean")


  hausdorff_dist <- max(c((dist_mat %>% apply(1, min) %>% max) - d_1,
                          (dist_mat %>% apply(2, min) %>% max) - d_2,
                          0))

  return(hausdorff_dist)
}

#' Method to check if a filament is completely contained in a set (relative to
#' discrete representation)
#'
#' uniform containment
#'
#' @param x assumed to either \code{convex_hull_structure} or
#'   \code{delta_ball_structure} objects
#' @param y data.frame with filament represented in it
#'
#' @return boolean if y is contained in x
#' @export
#'
contained <- function(x, y){
  UseMethod("contained")
}

#' @rdname contained
contained.delta_ball_structure <- function(x, y){
  # uniform containment
  dist_mat <- rdist::cdist(x, y, metric = "euclidean")
  delta <- attr(x, "delta")

  interior_dist <- dist_mat %>% apply(2, min) %>% max

  return(interior_dist <= delta)
}

#' @rdname contained
contained.convex_hull_structure <- function(x, y){
  normals <- attr(x, "normals")
  A <- normals[, -ncol(normals)]
  b <- normals[, ncol(normals)]

  contained_vec <- rep(F, nrow(y))
  for (y_idx in 1:nrow(y)){
    contained_vec[y_idx] <- all(-A %*% t(y[y_idx,]) >= b)
  }
  return(all(contained_vec))
}


#' Create linear map to move simplex with p vertices to p-1 dimensional space
#'
#' This approach comes for
#' \url{https://en.wikipedia.org/wiki/Simplex#Cartesian_coordinates_for_regular_n-dimensional_simplex_in_Rn}
#' and centers the projected points at 0.
#'
#' @param p number of vertices
#'
#' @return A matrix that transforms each vertices (column) to (p-1) dimensional
#'   space
#' @export
#'
#' @examples
#' A4 <- simplex_project_mat(4)
#' # expected for dim 4 (for examples on wikipedia)
#' A4_expected <- matrix(c(1,0,0,
#'                         -1/3, sqrt(8)/3,0,
#'                         -1/3,-sqrt(2)/3, sqrt(2/3),
#'                         -1/3, -sqrt(2)/3, -sqrt(2/3)),
#'                       ncol = 4)
#' A4 == A4_expected
simplex_project_mat <- function(p){
  # https://en.wikipedia.org/wiki/Simplex#Cartesian_coordinates_for_regular_n-dimensional_simplex_in_Rn
  # Iteratively create:
  # For a regular simplex, the distances of its vertices to its center are equal (in both spaces, in the projected space - center = 0).
  # The angle subtended by any two vertices of an n-dimensional simplex through its center is \arccos \left({\tfrac {-1}{n))\right)
  # aka <a,b> = -1/n (where n is the dimension of the new space -- also defined as p-1)
  #
  # In terms of coding this really means:
  # 1. Initial setup
  # A = (p-1) x p matrix
  # A[,1] = e_1
  # A[lower_diag] = 0
  #
  # Iterative process:
  # J:
  # J.0: A[(j+1):(p-1), j] = 0 # technically not needed if set A = 0 initially
  # J.1: Solve: t(x[1:(j-1)]) %*% A[1:(j-1), 1:(j-1)] = (-1/(p-1)) * rep(1, j-1)
  # Set A[1:(j-1),j] = x
  # J.2: a_j_star = ||x||^2
  # Set A[j,j] = sqrt(1-a_j_star)
  #
  # For J = p
  # only do J.1

  assertthat::assert_that(p >= 3 & floor(p) == p,
                          msg = "p should be an integer 3 or higher")

  # initial set up
  A <- matrix(0, nrow = p - 1, ncol = p)
  # first column:
  A[,1] <- c(1, rep(0, p-2))

  # j = 2:(p-1)
  for (j in 2:(p-1)) {
    A_pre_j <- A[1:(j-1), 1:(j-1)]
    diff_constant <- (-1/(p-1)) * rep(1, j-1)

    x <- solve(t(A_pre_j), diff_constant)
    A[1:(j-1),j] <- x
    A[j,j] <- sqrt(1-sum(x^2))
  }

  # for j = p
  j = p
  A_pre_j <- A[1:(j-1), 1:(j-1)]
  diff_constant <- (-1/(p-1)) * rep(1, j-1)
  A[,j] <- solve(t(A_pre_j), diff_constant)


  return(A)
}



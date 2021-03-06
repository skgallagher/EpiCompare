# These functions will need to be move around (but probably should think about
# if there are redundant functions now)


#' Compress filaments to filaments with the same number of points (equally
#' linearly space compared to original filament definition)
#'
#' @param grouped_df grouped_df data.frame object (assumed rows per filament are
#'   ordered) - grouped per each filament
#' @param data_columns columns of data.frame that relate to
#'   the filament's coordinates in euclidean space. The input should look like
#' something like \code{c(S,I,R)} or \code{c("S", "I", "R")}. If the input is
#' \code{NULL} this function will treat this like all non-group columns.
#' @param number_points integer number of points for each filament to be
#'   compressed to
#'
#' @return updated grouped_df with new rows so that each filament has the same
#'   number of points.
#' @export
#'
#' @examples
#' library(dplyr)
#' t13compression <- EpiCompare::pomp_sir %>%
#'   arrange(time) %>% # just to be safe
#'   select(-H, -cases, -time) %>%
#'   filter(.id <= 5) %>%
#'   group_by(.id) %>%
#'   filament_compression()
#'
#' t9compression <- EpiCompare::pomp_sir %>%
#'   filter(.id <= 5) %>%
#'   group_by(.id) %>%
#'   filament_compression(data_columns = c("S","I","R"), number_points = 9)
filament_compression <- function(grouped_df, data_columns = NULL,
                                 number_points = 13){
  #quos
  data_columns_q <- dplyr::enquos(data_columns)
  data_columns <- unname(tidyselect::vars_select(dplyr::tbl_vars(grouped_df),
                                                 !!!data_columns_q))
  group_columns <- names(attr(grouped_df, "groups"))[names(attr(grouped_df, "groups")) != ".rows"]
  if (length(data_columns) == 0){ # use all columns except the grouped columns
    data_columns <- names(grouped_df)
    data_columns <- data_columns[!(data_columns %in% group_columns)]
  }

  compression_df <- grouped_df %>%
    tidyr::nest() %>%
    dplyr::mutate(data =
                    purrr::map(.data$data,
                               function(df) {equa_dist_points_direction(
                                 df[, data_columns],num_splits = number_points)})) %>%
    tidyr::unnest(cols = c(.data$data))

  return(compression_df)
}

#' Calculate filament depth (relative to distance depth)
#'
#' @param grouped_df grouped_df data.frame object (assumed rows per filament are
#'   ordered) - grouped per each filament
#' @param data_columns columns of data.frame that relate to
#'   the filament's coordinates in euclidean space. The input should look like
#' something like \code{c(S,I,R)} or \code{c("S", "I", "R")}. If the input is
#' \code{NULL} this function will treat this like all non-group columns.
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

  #quos
  data_columns_q <- dplyr::enquos(data_columns)
  data_columns <- unname(tidyselect::vars_select(dplyr::tbl_vars(grouped_df),
                                                 !!!data_columns_q))

  if (length(data_columns) == 0){ # use all columns except the grouped columns
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
#'
#' @param grouped_df grouped_df data.frame object (assumed rows per filament are
#'   ordered) - grouped per each filament
#' @param data_columns columns of data.frame that relate to
#'   the filament's coordinates in euclidean space. The input should look like
#' something like \code{c(S,I,R)} or \code{c("S", "I", "R")}. If the input is
#' \code{NULL} this function will treat this like all non-group columns.
#' @param filament_depth_function function to calculate depth relative to the
#'   filaments in the \code{grouped_df}. Will take in \code{data_columns}
#'   parameter as well
#' @param conf_level proportion of filaments to keep
#' @param .remove_group boolean (default true). Should the output keep the
#' grouping columns or just havd data points?
#'
#' @return updated \code{grouped_df} with only the top depth filaments
#' @export
#'
#' @examples
#' library(dplyr)
#' top_filaments <- EpiCompare::pomp_df %>% group_by(.id) %>%
#'   filter(.id <= 10) %>%
#'   grab_top_depth_filaments(data_columns =c("S","I","R"),
#'                            conf_level = .5)
#'
#' # note that the below doesn't contain all filaments - as some have 0 depth
#' all_but_extreme_filaments <- EpiCompare::pomp_df %>% group_by(.id) %>%
#'   filter(.id <= 10) %>%
#'   grab_top_depth_filaments(data_columns = c("S","I","R"),
#'                            conf_level = 1)
grab_top_depth_filaments <- function(grouped_df, data_columns = NULL,
                                     filament_depth_function = filament_distance_depth,
                                     conf_level = .95,
                                     .remove_group = TRUE){
  #quos
  data_columns_q <- dplyr::enquos(data_columns)
  data_columns <- unname(tidyselect::vars_select(dplyr::tbl_vars(grouped_df),
                                                 !!!data_columns_q))
  if (length(data_columns) == 0){
    data_columns <- NULL
  }
  depth_vector <- grouped_df %>%
    filament_depth_function(data_columns = data_columns)

  num_top <- sum(depth_vector > stats::quantile(depth_vector,
                                                probs = 1 - conf_level))
  # ^conf_level = 1 - alpha

  if (num_top == 0){
    stop("selected 'conf_level' returns 0 filaments")
  }


  updated_df <- grouped_df %>% tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(depth = depth_vector) %>%
    dplyr::filter(.data$depth >
                    stats::quantile(depth_vector, probs = 1 - conf_level))
  # ^conf_level = 1 - alpha


  if (.remove_group){
    updated_df <- updated_df %>%
      dplyr::select(.data$data) %>%
      tidyr::unnest(cols = dplyr::everything())
  } else {
    updated_df <- updated_df %>%
      dplyr::select(-.data$depth) %>%
      tidyr::unnest(cols = dplyr::everything())
  }
  
  return(updated_df)
}

# final part of pipeline

#' create the delta ball associated with points
#'
#' @param data_points points to create the delta ball structure from
#' @param data_columns columns of data.frame that relate to the point's
#' coordinates in euclidean space. This should be at least 3 columns (else it
#' doesn't really make sense to use this function). The input should look like
#' something like \code{c(S,I,R)} or \code{c("S", "I", "R")}. If the input is
#' \code{NULL} this function will treat this like
#' \code{\link[dplyr:everything]{dplyr::everything()}}.
#' @param .lower_simplex_project boolean, if data points should be projected to a simplex
#'   and then to the lower dimensional simplex (for this package, this should
#'   always be done)
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
#' delta_ball_cb <- EpiCompare::pomp_df %>%
#'   filter(.id <= 10) %>%
#'   arrange(time) %>% # just to be safe
#'   select(-time, -H, -cases) %>%
#'   group_by(.id) %>%
#'   grab_top_depth_filaments(conf_level = .9) %>%
#'   create_delta_ball_structure() #data_columns = c(S,I,R)
create_delta_ball_structure <- function(data_points, data_columns = NULL,
                                        .lower_simplex_project = TRUE){
  #quos
  data_columns_q <- dplyr::enquos(data_columns)
  data_columns <- unname(tidyselect::vars_select(dplyr::tbl_vars(data_points),
                                           !!!data_columns_q))

  if (!is.null(data_columns)){
    delta_ball_info <- data_points %>%
      dplyr::select(dplyr::one_of(data_columns))
  }

  delta_ball_info <- data_points %>% dplyr::distinct()

  if (.lower_simplex_project){
    # projecting to lower structure to simplex
    A <- simplex_project_mat(ncol(delta_ball_info))
    delta_ball_info <- to_lower_simplex(delta_ball_info, A)
  }

  # delta_ball approach
  delta_info <- get_delta_nn(data = delta_ball_info)
  delta <- delta_info

  # class info
  class(delta_ball_info) <- c("delta_ball_structure", class(delta_ball_info))
  attr(delta_ball_info, "delta") <- delta
  if (.lower_simplex_project){
    attr(delta_ball_info, "A") <- A
  } else {
    attr(delta_ball_info, "A") <- diag(ncol(data_points))
  }

  return(delta_ball_info)
}

#' create the convex hull associated with points
#'
#' I believe this will error unless one projects from the simplex to the lower
#' dimensional space
#'
#' @param data_points points to create the convex hull structure from
#' @param data_columns columns of data.frame that relate to the point's
#' coordinates in euclidean space. This should be at least 3 columns (else it
#' doesn't really make sense to use this function). The input should look like
#' something like \code{c(S,I,R)} or \code{c("S", "I", "R")}. If the input is
#' \code{NULL} this function will treat this like
#' \code{\link[dplyr:everything]{dplyr::everything()}}.
#' @param .lower_simplex_project boolean, if data points should be projected to a simplex
#'   and then to the lower dimensional simplex (for this package, this should
#'   always be done)
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
#' # this will look like a 2d data frame (but this is becuase pomp_df info is
#' # on the 3d simplex)
#' convex_hull_cb <- EpiCompare::pomp_df %>%
#'   filter(.id <= 10) %>%
#'   arrange(time) %>% # just to be safe
#'   select(-time, -H, -cases) %>%
#'   group_by(.id) %>%
#'   grab_top_depth_filaments(conf_level = .9) %>%
#'   create_convex_hull_structure() #data_columns = c(S,I,R)
create_convex_hull_structure <- function(data_points, data_columns = NULL,
                                         .lower_simplex_project = TRUE){
  #quos
  data_columns_q <- dplyr::enquos(data_columns)
  data_columns <- unname(tidyselect::vars_select(dplyr::tbl_vars(data_points),
                                                 !!!data_columns_q))

  if (length(data_columns) == 0){
    data_columns = names(data_points)
  }

  inner_data <- data_points %>%
    dplyr::select(dplyr::one_of(data_columns)) %>% dplyr::distinct()
  if (.lower_simplex_project){
    # projecting to lower structure to simplex
    A <- simplex_project_mat(ncol(inner_data))
    inner_data <- to_lower_simplex(inner_data, A)
  }
  # convex hull
  chulln <- geometry::convhulln(inner_data,
                                output.options = "n")
  points_on_hull <- inner_data[unique(matrix(chulln$hull)),]


  # class structure
  class(points_on_hull) <- c("convex_hull_structure", class(points_on_hull))
  attr(points_on_hull, "normals") <- chulln$normals

  if (.lower_simplex_project){
    attr(points_on_hull, "A") <- A
  } else {
    attr(points_on_hull, "A") <- diag(ncol(data_points))
  }


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
#' Completely contained should be thought of as "uniform" containment.
#'
#'
#' @param x assumed to either \code{convex_hull_structure} or
#'   \code{delta_ball_structure} objects
#' @param y data.frame with filament represented in it
#' @param .lower_simplex_project boolean, if y should be projected to a simplex
#'   and then to the lower dimensional simplex (that x probably was). For this
#'   package, this should always be done.
#'
#' @return boolean if y is contained in x
#' @export
#'
contained <- function(x, y, .lower_simplex_project = TRUE){
  UseMethod("contained")
}


#' @rdname contained
contained.delta_ball_structure <- function(x, y,
                                           .lower_simplex_project = TRUE){
  if (.lower_simplex_project){
    y_inner <- to_lower_simplex(y, attr(x, "A"))
  } else {
    y_inner <- y
  }

  dist_mat <- rdist::cdist(x, y_inner, metric = "euclidean")
  delta <- attr(x, "delta")

  interior_dist <- dist_mat %>% apply(2, min) %>% max

  return(interior_dist <= delta)
}
if (r_new_interface()){
  .S3method("contained", "delta_ball_structure")
}

#' @rdname contained
contained.convex_hull_structure <- function(x, y,
                                            .lower_simplex_project = TRUE){
  if (.lower_simplex_project){
    y_inner <- to_lower_simplex(y, attr(x, "A"))
  } else {
    y_inner <- y
  }
  normals <- attr(x, "normals")
  A <- normals[, -ncol(normals)]
  b <- normals[, ncol(normals)]

  contained_vec <- rep(F, nrow(y))
  for (y_idx in 1:nrow(y_inner)){
    contained_vec[y_idx] <- all(-A %*% t(y_inner[y_idx,]) >= b)
  }
  return(all(contained_vec))
}
if (r_new_interface()){
  .S3method("contained", "convex_hull_structure")
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
#' all.equal(A4, A4_expected) # minor numerical inconsistencies
#'
#' library(dplyr)
#'
#' if (interactive()){
#'   # visualizing this 4d projection
#'   s4in3 <- simplex_project_mat(4) %>% t %>% data.frame %>%
#'     rbind(., data.frame(X1 = 0, X2 = 0, X3 = 0)) %>%
#'     mutate(color = factor(c(rep(1,4), 2)))#center
#'
#'
#'   library(plotly)
#'   plot_ly(s4in3,
#'           type = "scatter3d",
#'           mode = "markers", x = ~X1, y = ~X2, z = ~X3,
#'           color = ~color)
#' }
#' # visualizing a 3d projection
#' s3in2 <- simplex_project_mat(3) %>% t %>% data.frame %>%
#'  rbind(., data.frame(X1 = 0, X2 = 0)) %>%
#'  mutate(color = factor(c(rep(1,3), 2))) #center
#'
#' library(ggplot2)
#' ggplot(s3in2) +
#'   geom_point(aes(x = X1, y = X2, color = color))
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


#' move df to simplex representation
#'
#' This function takes a data.frame, moves it to unit simplex representation and
#' then projects the points onto a lower representation space (using \code{A}).
#'
#' @param df an n x p data.frame, all scalar columns
#' @param A default is \code{NULL}, if so, we create A from
#'   \code{simplex_project_mat}
#'
#' @return updated dat.frame (n x k), where k should be p-1, but that's assuming
#'   A is made from \code{simplex_project_mat}
#' @export
#'
#' @examples
#' center <- data.frame(x = 1, y = 1, z = 1)
#' to_lower_simplex(center) # 0,0 # center
#'
#' side <- data.frame(x = 1, y = 0, z =0)
#' to_lower_simplex(side) #(1,0)
to_lower_simplex <- function(df, A = NULL){
  # first get to simplex
  assertthat::assert_that(all(df >= 0),
                          msg = paste("data values should all be greater or",
                                      "equal to 0 to meet expectations of",
                                      "'simplex' potential data"))
  row_sum <- rowSums(df)
  df_simplex <- as.matrix(df / row_sum)

  if (is.null(A)){
    A <- simplex_project_mat(ncol(df))
  }
  df_lower <- as.data.frame(df_simplex %*% t(A))

  return(df_lower)
}

print.convex_hull_structure <- function(x){
  A <- attr(x, "A")
  p <- nrow(A)
  q <- ncol(A)
  n <- nrow(x)
  cat(sprintf(paste("Convex hull structure object,",
                    "for data projected from %i to %i dimensional space,",
                    "defined by %i vertices.",
                    "Below are the vertices in the projected space.\n"),
              q, p, n))
  print.data.frame(x)
}

print.delta_ball_structure <- function(x){
  A <- attr(x, "A")
  p <- nrow(A)
  q <- ncol(A)
  n <- nrow(x)
  cat(sprintf(paste("Delta ball structure object,",
                    "for data projected from %i to %i dimensional space,",
                    "defined by %i points.",
                    "Below are the points in the projected space.\n"),
              q, p, n))
  print.data.frame(x)
}






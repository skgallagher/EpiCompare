#' Global Distance Depth

#' pulling out data points from curves that are the most deep
#'
#' @param x list or grouped_df containing curves, with index ordering associated
#'   with the dist_mat's row/column ordering
#' @param alpha the proportion of curves to be removed before presenting all the
#'   points together. Takes value in (0, 1.0)
#' @param dist_mat distance matrix
#' @param dist_func function to calculate depth via the distance_matrix
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(1)
#' random_data_list <- lapply(1:5, function(x){data.frame(matrix(rnorm(10),
#'                                                               ncol = 2))})
#'
#' dist_mat <- dist_matrix_innersq(random_data_list,
#'                                 position = 1:2,
#'                                 verbose = FALSE)
#'
#' combined_points_list <- depth_curves_to_points(random_data_list,
#'                                                alpha = .2,
#'                                                dist_mat)
#'
#' random_data_grouped <- random_data_list %>%
#'   do.call(rbind, .) %>%
#'   mutate(id = rep(1:5, each = 5)) %>%
#'   group_by(id)
#'
#' combined_points_grouped <- depth_curves_to_points(random_data_grouped,
#'                                                   alpha = .2,
#'                                                   dist_mat)
depth_curves_to_points <- function(x, alpha, dist_mat,
                                   dist_func = distance_depth_function){
  UseMethod("depth_curves_to_points")
}

#' @rdname depth_curves_to_points
#' @export
depth_curves_to_points.list <- function(x, alpha, dist_mat,
                                        dist_func = distance_depth_function){
  depth_vector <- dist_func(dist_mat)
  deep_idx <- which(depth_vector > stats::quantile(depth_vector, probs = alpha))

  updated_df <- x[deep_idx] %>% do.call(rbind, .)

  return(updated_df)
}


#' @rdname depth_curves_to_points
#' @export
depth_curves_to_points.grouped_df <- function(x, alpha, dist_mat,
                                          dist_func = distance_depth_function){
  depth_vector <- dist_func(dist_mat)

  if (tidyr_new_interface()){
    updated_df <- x %>% tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(depth = depth_vector) %>%
      dplyr::filter(depth > stats::quantile(depth_vector, probs = alpha)) %>%
      dplyr::select(-depth) %>%
      tidyr::unnest(cols = everything())
  } else {
    updated_df <- x %>% tidyr::nest() %>%
      dplyr::mutate(depth = depth_vector) %>%
      dplyr::filter(depth > stats::quantile(depth_vector, probs = alpha)) %>%
      dplyr::select(-depth) %>%
      tidyr::unnest()
  }

  return(updated_df)
}

#' Geenens & Nieto-Reyes functional distance-based depth
#'
#' Calculates a global distance-based depth vector using a distance matrix.
#' Specifically we use Geenens & Nieto-Reyes's global distance-based depth
#' defined as:
#'
#' \eqn{DD(x, \hat{P}) = 1/(n choose 2) * sum_{i!=j} I(d(X_1, X_2) > max(d(X_1,x),
#' d(X_2,x)))}
#'
#' @param dist_mat a n x n square positive symmetric matrix
#'
#' @return depth vector length n with depth values associated with indices in
#'   \code{dist_mat}
#' @export
#'
#' @examples
#'
#' dist_mat <- matrix(c(0,   1, 1.5,
#'                      1,   0, 2,
#'                      1.5, 2, 0   ),
#'                    nrow = 3,
#'                    byrow = TRUE)
#'
#' dd_vec <- distance_depth_function(dist_mat) # c(1,0,0)
distance_depth_function <- function(dist_mat){
  # This is a cleaned up version of the \code{depth_function} from
  # \pkg{TCpredictionbands} available on github. One of the authors of this
  # package wrote both versions (and given \pkg{TCpredictionbands} is not on
  # CRAN...).

  if (nrow(dist_mat) != ncol(dist_mat) |
      any(t(dist_mat) != dist_mat) |
      any(dist_mat < 0)) {
    stop("your dist_mat is not a positive symmetric square matrix")
  }

  N <- nrow(dist_mat)
  depth <- rep(0, N)

  for (obs_index in 1:N) {
    sub_matrix <- dist_mat[-obs_index, -obs_index]
    obs_column <- dist_mat[-obs_index, obs_index]
    obs_row <- dist_mat[obs_index, -obs_index]

    obs_combo_array <- array(0, dim = c(N - 1, N - 1, 2))
    obs_combo_array[, ,1] <- matrix(rep(obs_column, N - 1),
                                    nrow = N - 1)
    obs_combo_array[, ,2] <- matrix(rep(obs_row, N - 1),
                                     nrow = N - 1, byrow = T)

    max_matrix <- sapply(1:(N - 1), function(row_i) {
      sapply(1:(N - 1), function(col_i) {
        max(obs_combo_array[row_i, col_i, 1:2])
      })
      }) %>% t

    depth[obs_index] <- mean((sub_matrix > max_matrix)[
                                row(sub_matrix)!=col(sub_matrix)
                                #^ignoring the diagonal values
                                ])
  }

  return(depth)
}

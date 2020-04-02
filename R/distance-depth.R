#' Global Distance Depth

#' pulling out data points from curves that are the most deep
#'
#' @details This function for lists (renamed as \code{depth_curves_to_points})
#' is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param x list or grouped_df containing curves, with index ordering associated
#'   with the dist_mat's row/column ordering
#' @param alpha the proportion of curves to be removed before presenting all the
#'   points together. Takes value in (0, 1.0)
#' @param dist_mat distance matrix
#' @param dist_func function to calculate depth via the distance_matrix
#'
#' @return data frame from curves of correct depth.
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(1)
#' random_data_list <- lapply(1:5, function(x){data.frame(matrix(rnorm(10),
#'                                                               ncol = 2))})
#'
#' dist_mat <- dist_matrix_innersq_direction(random_data_list,
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

  updated_df <- do.call(rbind, x[deep_idx])

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
      dplyr::filter(.data$depth >
                      stats::quantile(depth_vector, probs = alpha)) %>%
      dplyr::select(-.data$depth) %>%
      tidyr::unnest(cols = dplyr::everything())
  } else {
    updated_df <- x %>% tidyr::nest() %>%
      dplyr::mutate(depth = depth_vector) %>%
      dplyr::filter(.data$depth > stats::quantile(depth_vector, probs = alpha)) %>%
      dplyr::select(-.data$depth) %>%
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
#' \eqn{DD(x, \hat{P}) = 1/(n choose 2) \cdot \sum_{i!=j} I(d(X_i, X_j) >
#' max(d(X_i,x), d(X_j,x)))}
#'
#' @details
#' This function (renamed as \code{depth_function}) is shared with
#' \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param dist_mat a n x n square positive symmetric matrix
#'
#' @return depth vector length n with depth values associated with indices in
#'   \code{dist_mat}
#' @export
#'
#' @examples
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



#' Localized version of Geenens & Nieto-Reyes functional distance-based depth
#'
#' Calculates a localized distance-based depth vector using a distance matrix.
#' Specifically we alter Geenens & Nieto-Reyes's global distance-based depth to
#' a local depth similar to Agostinelli & Romanazzi (2011) localized approach,
#' and define our local distance-based depth as:
#' 
#' \eqn{LDD(x, \hat{P}, \tau) = 1/(|S| choose 2) \cdot \sum_{i!=j, i,j \in S}
#' I(d(X_i, X_j) > max(d(X_i,x), d(X_j,x)))}
#'
#' where \eqn{S = {i : D(X_i,x) < \tau}}.
#'
#' Note that if \eqn{|S| = 1}, then we say that \code{LDD(x) = 0}.
#'
#' @details
#' This function (renamed as \code{depth_function}) is shared with
#' \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#' 
#' @param dist_mat a n x n square positive symmetric matrix
#' @param tau localizing parameter (default is Inf)
#'
#' @return depth vector length n with depth values associated with indices in
#'   \code{dist_mat}
#' @export
#'
#' @examples
#' dist_mat <- matrix(c(0,   1, 1.5,
#'                      1,   0, 2,
#'                      1.5, 2, 0   ),
#'                    nrow = 3,
#'                    byrow = TRUE)
#'
#' dd_vec <- local_distance_depth_function(dist_mat) # c(1,0,0)
#' 
#' ldd_vec1 <- local_distance_depth_function(dist_mat, tau = 2) # c(1,0,0)
#' ldd_vec2 <- local_distance_depth_function(dist_mat, tau = 1.5) # c(1,0,0)
#' ldd_vec3 <- local_distance_depth_function(dist_mat, tau = 1) # c(0,0,0)
#' ldd_vec <- local_distance_depth_function(dist_mat, tau = .1) # c(0,0,0)
local_distance_depth_function <- function(dist_mat, tau = Inf){
  
  if (nrow(dist_mat) != ncol(dist_mat) |
      any(t(dist_mat) != dist_mat) |
      any(dist_mat < 0)) {
    stop("your dist_mat is not a positive symmetric square matrix")
  }
  
  N <- nrow(dist_mat)
  depth <- rep(0, N)
  
  for (obs_index in 1:N) {
    # effected by tau
    rm_idx <- c(1:N)[-obs_index]
    
    dist_to_obs = dist_mat[obs_index, -obs_index]
    keep_idx <- rm_idx[dist_to_obs <= tau]
    N_keep <- length(keep_idx)
    
    if (N_keep <= 1) { # if only 1 or 0 points are in the S
      depth[obs_index] <- 0
    } else {
    sub_matrix <- dist_mat[keep_idx,keep_idx]
    obs_column <- dist_mat[keep_idx, obs_index]
    obs_row <- dist_mat[obs_index, keep_idx]
    
    obs_combo_array <- array(0, dim = c(N_keep, N_keep, 2))
    obs_combo_array[, ,1] <- matrix(rep(obs_column, N_keep),
                                    nrow = N_keep)
    obs_combo_array[, ,2] <- matrix(rep(obs_row, N_keep),
                                    nrow = N_keep, byrow = T)
    
    # below seems complicated
    max_matrix <- apply(obs_combo_array, 1:2, max)

    depth[obs_index] <- mean((sub_matrix > max_matrix)[
      row(sub_matrix)!=col(sub_matrix)
      #^ignoring the diagonal values
      ])
    }
  }
  
  return(depth)
}

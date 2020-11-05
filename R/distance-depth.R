#' Pull top (1-alpha)\% quantile percent of curves points.
#'
#' pulling out data points from curves that are ranked highest based on quantile
#' function.
#'
#' @details This function for lists (renamed as \code{depth_curves_to_points})
#'   is shared with \pkg{TCpredictionbands} on github:
#'   \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param x list or grouped_df containing curves, with index ordering associated
#'   with the dist_mat's row ordering
#' @param alpha the proportion of curves to be removed before presenting all the
#'   points together. Takes value in [0, 1.0].
#' @param dist_mat distance matrix
#' @param dist_func function to calculate quantiles via the distance_matrix
#' @param ... additional parameters to be passed to the dist_func
#'
#' @return data frame from curves of the top values associated with the
#'   \code{dist_func}
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
#' combined_points_list <- quantile_curves_to_points(random_data_list,
#'                                                alpha = .2,
#'                                                dist_mat)
#'
#' random_data_grouped <- random_data_list %>%
#'   do.call(rbind, .) %>%
#'   mutate(id = rep(1:5, each = 5)) %>%
#'   group_by(id)
#'
#' combined_points_grouped <- quantile_curves_to_points(random_data_grouped,
#'                                                   alpha = .2,
#'                                                   dist_mat)
quantile_curves_to_points <- function(x, alpha, dist_mat,
                                   dist_func = distance_depth_function,
                                   ...){
  #.Deprecated("top_curves_to_points")
  UseMethod("quantile_curves_to_points")
}

#' @rdname quantile_curves_to_points
#' @export
quantile_curves_to_points.list <- function(x, alpha, dist_mat,
                                           dist_func = distance_depth_function,
                                           ...){
  depth_vector <- dist_func(dist_mat, ...)
  deep_idx <- which(depth_vector > stats::quantile(depth_vector, probs = alpha))

  updated_df <- do.call(rbind, x[deep_idx])

  return(updated_df)
}


#' @rdname quantile_curves_to_points
#' @export
quantile_curves_to_points.grouped_df <- function(x, alpha, dist_mat,
                                          dist_func = distance_depth_function,
                                          ...){
  depth_vector <- dist_func(dist_mat, ...)

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



#' Pull the points from the top (1-alpha)\% percent of curves.
#'
#' pulling out data points from curves that are ranked highest based on quantile
#' function.
#'
#' @param x list or grouped_df containing curves, with index ordering associated
#'   with the \code{x_names_df} and \code{rownames} of \code{tidy_dm}
#' @param alpha the proportion of curves to be removed before presenting all the
#'   points together. Takes value in [0, 1.0].
#' @param tidy_dm a \code{tidy_dist_mat} distance matrix
#' @param quantile_func function to calculate quantiles via the distance_matrix,
#'   we now expect this function to handle \code{tidy_dist_mat} objects and have
#'   a parameter called \code{df_out} which we can set as true. See
#'   \code{distance_depth_function.tidy_dist_mat} for an example.
#' @param x_names_df Only used when x is a list. Group structure associated with
#'   the ordering of the items in the list \code{x}. Assume the naming structure
#'   for \code{tidy_dm} is the same as this data frame (can have different
#'   ordering).
#' @param ... additional parameters to be passed to the \code{quantile_func}.
#'   Please also see details for more information.
#'
#' @return data frame from curves of the top values associated with the
#'   \code{quantile_func}
#'
#' @details  See \code{\link{top_curves_to_points.list}} and
#'   \code{\link{top_curves_to_points.grouped_df}} for more details and
#'   commentary on expected parameters. \code{\link{top_curves_to_points.list}}
#'   requires an additional parameter - \code{x_names_df}.
#'
#' @export
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
#' combined_points_list <- quantile_curves_to_points(random_data_list,
#'                                                alpha = .2,
#'                                                dist_mat)
#'
#' random_data_grouped <- random_data_list %>%
#'   do.call(rbind, .) %>%
#'   mutate(id = rep(1:5, each = 5)) %>%
#'   group_by(id)
#'
#' combined_points_grouped <- quantile_curves_to_points(random_data_grouped,
#'                                                   alpha = .2,
#'                                                   dist_mat)
top_curves_to_points <- function(x, alpha,
                                 tidy_dm,
                                 quantile_func = distance_depth_function,
                                 x_names_df = NULL,
                                 ...){
  UseMethod("top_curves_to_points")
}

#' Pull the points from the top (1-alpha)\% percent of curves (LIST)
#'
#' pulling out data points from curves that are ranked highest based on quantile
#' function.
#'
#' @param x list containing curves, with index ordering associated
#'   with the \code{x_names_df} and \code{rownames} of \code{tidy_dm}
#' @param alpha the proportion of curves to be removed before presenting all the
#'   points together. Takes value in [0, 1.0].
#' @param tidy_dm a \code{tidy_dist_mat} distance matrix
#' @param quantile_func function to calculate quantiles via the distance_matrix,
#'   we now expect this function to handle \code{tidy_dist_mat} objects and have
#'   a parameter called \code{df_out} which we can set as true. See
#'   \code{distance_depth_function.tidy_dist_mat} for an example.
#' @param x_names_df group structure associated with the ordering of the items
#'   in the list \code{x}. Assume the naming structure for \code{tidy_dm} is
#'   the same as this data frame (can have different ordering).
#' @param ... additional parameters to be passed to the \code{quantile_func}.
#'   Please also see details for more information.

#'   
#' @return data frame from curves of the top values associated with the
#'   \code{quantile_func}
#' @export
top_curves_to_points.list <- function(x, alpha,
                                 tidy_dm,
                                 quantile_func = distance_depth_function,
                                 x_names_df,
                                 ...){
  
  if(missing(x_names_df) | is.null(x_names_df)) {
    stop("please provide ans x_names_df.")
  }
  
  quantile_df <- quantile_func(tidy_dm, ..., df_out = T) %>% as.data.frame()
  
  quantile_name <- names(quantile_df)[!(names(quantile_df) %in% names(rownames(tidy_dm)))]
  
  lower_bound <- stats::quantile(quantile_df[[quantile_name]], probs = alpha)
  
  quantile_df["tctp_indicator"] <- quantile_df[[quantile_name]] > lower_bound
  
  x_names_df_i <- x_names_df %>% 
    dplyr::mutate(tctp_index = 1:nrow(x_names_df))

  combined <- x_names_df_i %>% 
    dplyr::left_join(quantile_df, by = names(rownames(tidy_dm)))
  
  deep_idx <- combined[combined[["tctp_indicator"]], "tctp_index"]
  
  updated_df <- do.call(rbind, x[deep_idx])
  
  return(updated_df)
}

#' Pull the points from the top (1-alpha)\% percent of curves (LIST)
#'
#' pulling out data points from curves that are ranked highest based on quantile
#' function.
#'
#' @param x grouped data.frame containing curves, with grouping associated with
#'   the \code{rownames} of \code{tidy_dm}.
#' @param alpha the proportion of curves to be removed before presenting all the
#'   points together. Takes value in [0, 1.0].
#' @param tidy_dm a \code{tidy_dist_mat} distance matrix
#' @param quantile_func function to calculate quantiles via the distance_matrix,
#'   we now expect this function to handle \code{tidy_dist_mat} objects and have
#'   a parameter called \code{df_out} which we can set as true. See
#'   \code{distance_depth_function.tidy_dist_mat} for an example.
#' @param x_names_df - pointless parameter - is not used.
#' @param ... additional parameters to be passed to the \code{quantile_func}.
#'   Please also see details for more information.
#'
#' @return data frame from curves of the top values associated with the
#'   \code{quantile_func}
#' @export
top_curves_to_points.grouped_df <- function(x, alpha,
                                        tidy_dm,
                                        quantile_func = distance_depth_function,
                                        x_names_df = NULL,
                                        ...){
  quantile_df <- quantile_func(tidy_dm, ..., df_out = T) %>% as.data.frame()
  
  quantile_name <- names(quantile_df)[
    !(names(quantile_df) %in% names(rownames(tidy_dm)))
    ]
  
  lower_bound <- stats::quantile(quantile_df[[quantile_name]], probs = alpha)
  
  quantile_df["tctp_indicator"] <- quantile_df[[quantile_name]] > lower_bound
  
  combine_df <- x %>% 
    dplyr::left_join(quantile_df, by = names(rownames(tidy_dm))) 
  
  updated_df <- combine_df[combine_df[["tctp_indicator"]], ] %>%
    dplyr::ungroup() %>% 
    dplyr::select(-dplyr::one_of(c(quantile_name, "tctp_indicator",
                                   names(rownames(tidy_dm)))))
  
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
#' @param dist_mat a n x n square positive symmetric matrix or a tidy_dist_mat
#' @param df_out indicates if one should return a data.frame our a vector,
#' by default returns data.frame if dist_mat is a tidy_dist_mat, and a vector
#' if dist_mat is a matrix.
#' @return depth vector length n with depth values associated with indices in
#'   \code{dist_mat} or a data.frame with a column called \code{depth}
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
#' 
#' tidy_dm <- tidy_dist_mat(dist_mat)
#' dd_df <- distance_depth_function(tidy_dm) # depth = c(1,0,0)
distance_depth_function <- function(dist_mat, df_out = "auto"){
  UseMethod("distance_depth_function")
}

#' @rdname distance_depth_function
#' @export
distance_depth_function.matrix <- function(dist_mat, df_out = F){
  # This is a cleaned up version of the \code{depth_function} from
  # \pkg{TCpredictionbands} available on github. One of the authors of this
  # package wrote both versions (and given \pkg{TCpredictionbands} is not on
  # CRAN...).

  if (df_out == "auto"){
    df_out <- FALSE
  }
  
  if (nrow(dist_mat) != ncol(dist_mat) |
      any(t(dist_mat) != dist_mat) |
      any(dist_mat < 0)) {
    stop("your dist_mat is not a positive symmetric square matrix")
  }

  N <- nrow(dist_mat)
  rnames <- rownames(dist_mat)
  
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

  if (df_out) {
    if (is.null(rnames)){
      rnames <- 1:length(depth)
    }
    depth_out <- data.frame(names = rnames,
                             depth = depth)
  } else {
    names(depth) <- rnames
    depth_out <- depth
  }
  return(depth_out)
}

#' @rdname distance_depth_function
#' @export
distance_depth_function.tidy_dist_mat <- function(dist_mat, df_out = T){
  if (df_out == "auto"){
    df_out <- TRUE
  }
  
  if (nrow(dist_mat) != ncol(dist_mat) |
      any(t(dist_mat) != dist_mat) |
      any(dist_mat < 0)) {
    stop("your dist_mat is not a positive symmetric square matrix")
  }
  
  N <- nrow(dist_mat)
  rnames <- rownames(dist_mat) # data.frame
  
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
  
  
  if (df_out) {
    depth_out <- rnames %>% dplyr::mutate(depth = depth)
    
  } else {
    rnames <- rnames %>% tidyr::unite(col = "names",
                                      dplyr::everything(), sep = "|") %>% 
      dplyr::pull(names)
    
    names(depth) <- rnames
    depth_out <- depth
  }
  
  return(depth_out)
  
}

if (r_new_interface()){
  .S3method(generic = "distance_depth_function", class = "tidy_dist_mat") 
  .S3method(generic = "distance_depth_function", class = "matrix") 
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
#' This matrix function (renamed as \code{depth_function}) is shared with
#' \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#' 
#' @param dist_mat a n x n square positive symmetric matrix or a tidy_dist_mat
#' @param tau localizing parameter (default is Inf)
#' @param df_out indicates if one should return a data.frame our a vector,
#' by default returns data.frame if dist_mat is a tidy_dist_mat, and a vector
#' if dist_mat is a matrix.
#' 
#' @return depth vector length n with depth values associated with indices in
#'   \code{dist_mat} or a data.frame with a column called \code{local_depth}
#' @return depth vector length n with depth values associated with indices in
#'   \code{dist_mat}
#' @export
#'
#' @examples
#' ## matrix-only examples 
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
local_distance_depth_function <- function(dist_mat,tau = Inf, df_out = "auto"){
  UseMethod("local_distance_depth_function")
}

#' @rdname local_distance_depth_function
#' @export
local_distance_depth_function.matrix <- function(dist_mat, tau = Inf, 
                                                 df_out = F){
  if (df_out == "auto"){
    df_out <- FALSE
  }
  
  if (nrow(dist_mat) != ncol(dist_mat) |
      any(t(dist_mat) != dist_mat) |
      any(dist_mat < 0)) {
    stop("your dist_mat is not a positive symmetric square matrix")
  }
  
  N <- nrow(dist_mat)
  rnames <- rownames(dist_mat)
  
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
  
  if (df_out) {
    if (is.null(rnames)){
      rnames <- 1:length(depth)
    }
    depth_out <- data.frame(names = rnames,
                            local_depth = depth)
  } else {
    names(depth) <- rnames
    depth_out <- depth
  }
  
  return(depth_out)
}


#' @rdname local_distance_depth_function
#' @export
local_distance_depth_function.tidy_dist_mat <- function(dist_mat, tau = Inf, 
                                                        df_out = F){
  if (df_out == "auto"){
    df_out <- FALSE
  }
  
  if (nrow(dist_mat) != ncol(dist_mat) |
      any(t(dist_mat) != dist_mat) |
      any(dist_mat < 0)) {
    stop("your dist_mat is not a positive symmetric square matrix")
  }
  
  N <- nrow(dist_mat)
  rnames <- rownames(dist_mat) # data.frame
  
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
  
  if (df_out) {
    depth_out <- rnames %>% dplyr::mutate(local_depth = depth)
    
  } else {
    rnames <- rnames %>% tidyr::unite(col = "names",
                                      dplyr::everything(), sep = "|") %>% 
      dplyr::pull(names)
    
    names(depth) <- rnames
    depth_out <- depth
  }
  
  return(depth_out)
}


if (r_new_interface()){
  .S3method(generic = "local_distance_depth_function", class = "tidy_dist_mat") 
  .S3method(generic = "local_distance_depth_function", class = "matrix") 
}
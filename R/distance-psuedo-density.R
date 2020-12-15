#' psuedo-density for objects with a distance between them
#'
#' DESCRIBE MORE
#'
#' @param x a n x n square positive symmetric matrix or a \code{tidy_dist_mat}
#' @param x_new a n_new x n matrix or tidy_dist_mat where the rows correspond to
#' new observations, the columns correspond to points in \code{x} (if \code{x} 
#' and \code{x_new} are matrices then they need to be corrected ordered). If 
#' this value is not \code{NULL} (default is \code{NULL}) then the 
#' psuedo-density vector will be calculated for these observations relative to 
#' observations defined with \code{x} and \code{x_new}'s columns.
#' @param sigma scaling parameter. Can either by a standard numerical value or a
#' string as a percentage (e.g. "20\%")
#' @param df_out indicates if one should return a data.frame our a vector,
#' by default returns data.frame if dist_mat is a tidy_dist_mat, and a vector
#' if dist_mat is a matrix.
#' 
#' @return depth vector length n with depth values associated with indices in
#'   \code{x} or a data.frame with a column called \code{psuedo_density}
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
distance_psuedo_density_function <- function(x, x_new = NULL,
                                             sigma = 1, df_out = "auto"){
  UseMethod("distance_psuedo_density_function")
}





#' @rdname distance_psuedo_density_function
#' @export
distance_psuedo_density_function.matrix <- function(x, x_new = NULL,
                                                    sigma = 1, df_out = F){
  if (df_out == "auto"){
    df_out <- FALSE
  }
  
  if (inherits(sigma, "character")){
    percentage <- check_character_percent(sigma, "sigma")
    sigma <- stats::quantile(x, percentage)
  }

  if(is.null(x_new)){
    rnames <- rownames(x)
    
    kernel_dist_mat <- stats::dnorm(x/sigma)
    psuedo_density <- apply(kernel_dist_mat, MARGIN = 1, mean)
    
    if (df_out) {
      if (is.null(rnames)){
        rnames <- 1:length(psuedo_density)
      }
      psuedo_density_out <- data.frame(names = rnames,
                              psuedo_density = psuedo_density)
    } else {
      names(psuedo_density) <- rnames
      psuedo_density_out <- psuedo_density
    }
    
    return(psuedo_density_out)
  } else {
    rnames <- rownames(x_new)
    
    kernel_dist_mat <- stats::dnorm(x_new/sigma)
    psuedo_density <- apply(kernel_dist_mat, MARGIN = 1, mean)
    
    if (df_out) {
      if (is.null(rnames)){
        rnames <- 1:length(psuedo_density)
      }
      psuedo_density_out <- data.frame(names = rnames,
                                       psuedo_density = psuedo_density)
    } else {
      names(psuedo_density) <- rnames
      psuedo_density_out <- psuedo_density
    }
    
    return(psuedo_density_out)
  }
}

#' @rdname distance_psuedo_density_function
#' @export
distance_psuedo_density_function.tidy_dist_mat <- function(x, x_new = NULL,
                                                           sigma = 1, 
                                                           df_out = T){
  if (df_out == "auto"){
    df_out <- TRUE
  }
  
  if (inherits(sigma, "character")){
    percentage <- check_character_percent(sigma, "sigma")
    sigma <- stats::quantile(as.matrix(x), percentage)
  }
  
  if (is.null(x_new)){
    rnames <- rownames(x) # data.frame
    
    kernel_dist_mat <- stats::dnorm(as.matrix(x)/sigma)
    psuedo_density <- apply(kernel_dist_mat, MARGIN = 1, mean)
    
    
    if (df_out) {
      psuedo_density_out <- rnames %>% dplyr::ungroup() %>% 
        dplyr::mutate(psuedo_density = psuedo_density)
    } else {
      rnames <- rnames %>% dplyr::ungroup() %>%
        tidyr::unite(col = "names",
                     dplyr::everything(), sep = "|") %>% 
        dplyr::pull(names)
      
      names(psuedo_density) <- rnames
      psuedo_density_out <- psuedo_density
    }
    
    return(psuedo_density_out)
  } else {
    rnames <- rownames(x_new) # data.frame
    
    kernel_dist_mat <- stats::dnorm(as.matrix(x_new)/sigma)
    psuedo_density <- apply(kernel_dist_mat, MARGIN = 1, mean)
    
    
    if (df_out) {
      psuedo_density_out <- rnames %>% dplyr::ungroup() %>% 
        dplyr::mutate(psuedo_density = psuedo_density)
    } else {
      rnames <- rnames %>% dplyr::ungroup() %>%
        tidyr::unite(col = "names",
                     dplyr::everything(), sep = "|") %>% 
        dplyr::pull(names)
      
      names(psuedo_density) <- rnames
      psuedo_density_out <- psuedo_density
    }
    
    return(psuedo_density_out)
  }
}

if (r_new_interface()){
  .S3method(generic = "distance_psuedo_density_function", 
            class = "tidy_dist_mat") 
  .S3method(generic = "distance_psuedo_density_function", class = "matrix") 
}



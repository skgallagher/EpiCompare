#' psuedo-density for objects with a distance between them
#'
#' DESCRIBE MORE
#'
#' @param dist_mat a n x n square positive symmetric matrix or a tidy_dist_mat
#' @param sigma scaling parameter
#' @param df_out indicates if one should return a data.frame our a vector,
#' by default returns data.frame if dist_mat is a tidy_dist_mat, and a vector
#' if dist_mat is a matrix.
#' 
#' @return depth vector length n with depth values associated with indices in
#'   \code{dist_mat} or a data.frame with a column called \code{psuedo_density}
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
distance_psuedo_density_function <- function(x, sigma = 1, df_out = "auto"){
  UseMethod("distance_psuedo_density_function")
}

#' @rdname distance_psuedo_density_function
#' @export
distance_psuedo_density_function.matrix <- function(x, sigma = 1, df_out = F){
  if (df_out == "auto"){
    df_out <- FALSE
  }
  
  rnames <- rownames(x)
  
  kernel_dist_mat <- dnorm(x/sigma)
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

#' @rdname distance_psuedo_density_function
#' @export
distance_psuedo_density_function.tidy_dist_mat <- function(x, sigma = 1, 
                                                           df_out = T){
  if (df_out == "auto"){
    df_out <- TRUE
  }
  rnames <- rownames(x) # data.frame
  
  kernel_dist_mat <- dnorm(x/sigma)
  psuedo_density <- apply(unclass(kernel_dist_mat), MARGIN = 1, mean)
  
  
  if (df_out) {
    psuedo_density_out <- rnames %>% 
      dplyr::mutate(psuedo_density = psuedo_density)
  } else {
    rnames <- rnames %>% tidyr::unite(col = "names",
                                      dplyr::everything(), sep = "|") %>% 
      dplyr::pull(names)
    
    names(psuedo_density) <- rnames
    psuedo_density_out <- psuedo_density
  }
  
  return(psuedo_density_out)
}

if (r_new_interface()){
  .S3method(generic = "distance_psuedo_density_function", 
            class = "tidy_dist_mat") 
  .S3method(generic = "distance_psuedo_density_function", class = "matrix") 
}




group_identify <- function(dist_mat, threshold = quantile(dist_mat, .2)){
  
  rnames <- rownames(dist_mat)
  
  binary_mat <- 1* dist_mat <= threshold
  g <- igraph::graph.adjacency(binary_mat) 
  clu <- igraph::components(g)
  
  membership <- clu$membership
  names(membership) <- rnames
  
  return(membership)
} 


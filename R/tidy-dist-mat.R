#' creates tidy_dist_mat objects
#'
#' @param dist_mat distance matrix 
#' @param rownames_df data.frame with row identifying information
#' @param colnames_df data.frame with column identifying information
#'
#' @return
#' @export
tidy_dist_mat <- function(dist_mat, rownames_df = NULL, colnames_df = NULL){
  object <- dist_mat
  
  dimnames(object) <- NULL
  
  if (is.null(rownames_df)){
    rowname_df <- data.frame(id = 1:nrow(object))
  }
  if (is.null(colnames_df)){
    rowname_df <- data.frame(id = 1:ncol(object))
  }
  
  check_tidy_dist_mat_dimensions(object, rownames_df, colnames_df)
  check_tidy_dist_names_distinct(rownames_df, colnames_df)
  
  attr(object, which = "rownames_df") <- rownames_df
  attr(object, which = "colnames_df") <- colnames_df
  class(object) <- c("tidy_dist_mat", "matrix")
  
  return(object)
}

#' check_tidy_dist_mat_dimensions
#' 
#' internal function that makes sure to check the creation structure of the 
#' tidy_dist_mat object
#'
#' @param dist_mat distance matrix 
#' @param rownames_df data.frame with row identifying information
#' @param colnames_df data.frame with column identifying information
#'
#' @return either a error or \code{TRUE}
check_tidy_dist_mat_dimensions <- function(dist_mat, rownames_df, colnames_df){
  dim_dmat <- dim(dist_mat)
  
  assertthat::assert_that(dim_dmat[1] == nrow(rownames_df),
                        msg = paste("number of rows of distance matrix and",
                                    "rows of rownames_df do not match"))
  assertthat::assert_that(dim_dmat[2] == nrow(colnames_df),
                        msg = paste("number of columns of distance matrix and",
                                    "rows of colnames_df do not match"))  
  
  assertthat::assert_that(inherits(rownames_df, "data.frame"))
  assertthat::assert_that(inherits(colnames_df, "data.frame"))
  
  TRUE
}

#' check_tidy_dist_names_distinct
#' 
#' internal function that makes sure to names are distinct
#'
#' @param rownames_df data.frame with row identifying information
#' @param colnames_df data.frame with column identifying information
#'
#' @return either a error or \code{TRUE}
check_tidy_dist_names_distinct <- function(rownames_df, colnames_df){
  rn_df_distinct <- dplyr::distinct(rownames_df)
  assertthat::assert_that(identical(rownames_df, rn_df_distinct),
                          msg = paste("rownames_df is not distinct",
                                      "(see dplyr::distinct) for help"))
  cn_df_distinct <- dplyr::distinct(colnames_df)
  assertthat::assert_that(identical(colnames_df, cn_df_distinct),
                          msg = paste("colnames_df is not distinct",
                                      "(see dplyr::distinct) for help"))
  
  TRUE
}


#' checks if object is a tidy_dist_mat
#'
#' @param x any old object
#'
#' @return
#' @export
is.tidy_dist_mat <- function(x){
  inherits(x, "tidy_dist_mat")
}

#' @export
dimnames.tidy_dist_mat <- function(x){
  dimnames <- list("rownames" = attr(x, "rownames_df"),
                   "colnames" = attr(x, "colnames_df"))
}

#' @export
`dimnames<-.tidy_dist_mat` <- function(x, value){
  
  if (!is.list(value) || length(value) != 2L) {
    stop("invalid 'dimnames' given for data frame")
  }
  
  check_tidy_dist_mat_dimensions(x, value[[1]], value[[2]])
  
  attr(x, "rownames_df") <- value[[1]]
  attr(x, "colnames_df") <- value[[2]]
  x
}

#' @export
`rownames<-.tidy_dist_mat` <- function(x, value){
  
  if (!is.list(value) || length(value) != 2L) {
    stop("invalid 'dimnames' given for data frame")
  }
  
  check_tidy_dist_mat_dimensions(x, value, colnames(x))
  
  attr(x, "rownames_df") <- value
  
  x
}

#' @export
`colnames<-.tidy_dist_mat` <- function(x, value){
  
  if (!is.list(value) || length(value) != 2L) {
    stop("invalid 'dimnames' given for data frame")
  }
  
  check_tidy_dist_mat_dimensions(x, rownames(x),value)
  
  attr(x, "colnames_df") <- value
  
  x
}

#' @export
format.tidy_dist_mat <- function(x, digits = 6){
  dim_x <- dim(x)
  dim_colnames_x <- dim(colnames(x))
  dim_rownames_x <- dim(rownames(x))
  

  top_present <- cbind(matrix("",nrow = dim_colnames_x[2],
                          ncol = dim_rownames_x[2]),
                       matrix("|", nrow = dim_colnames_x[2],
                              ncol = 1),
                       t(as.matrix(colnames(x)))) 
  between_present <- matrix(c(rep("-", dim_rownames_x[2]), 
                              "+", 
                              rep(paste(rep("-", digits + 1),
                                        collapse = ""), 
                              dim_x[2])), nrow = 1)

  bottom_present <- rbind(cbind(as.matrix(rownames(x)), 
                                matrix("|", nrow = dim_rownames_x[1],
                                       ncol = 1),
                                signif(x, digits)))
  
  colnames(top_present) <- NULL
  dimnames(between_present) <- NULL
  rownames(bottom_present) <- NULL
  
  m <- rbind(top_present, between_present, bottom_present) 
  
  noquote(m)
}

#' print tidy_dist_mat objects
#'
#' @param x tidy_dist_mat object
#' @param digits number of significant digits to display (uses \code{signif})
#'
#' @return
#' @export
#'
#' @examples
#' inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
#' my_dist_mat <- as.matrix(dist(inner_data)) 
#' rownames_df <- data.frame(id = 1:3)
#' colnames_df <- data.frame(id = c(1,2,1), id2 = c("f", "f", "s"))
#' 
#' my_tidy_dm <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
#' print(my_tidy_dm)
print.tidy_dist_mat <- function(x, digits = 6){
  print(format(x))
}

if (r_new_interface()){
  .S3method(generic = "dimnames", class = "tidy_dist_mat") 
  .S3method(generic = "dimnames<-", class = "tidy_dist_mat") 
  .S3method(generic = "rownames<-", class = "tidy_dist_mat") 
  .S3method(generic = "colnames<-", class = "tidy_dist_mat") 
  
  .S3method(generic = "format", class = "tidy_dist_mat") 
  .S3method(generic = "print", class = "tidy_dist_mat") 
  

}


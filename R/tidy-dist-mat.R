#' tidy_dist_mat objects
#' 
#' This function allows the user to transform a distance \code{matrix} into a
#' \code{tidy_dist_mat} matrix with information about the rows and columns in 
#' the \code{tidyverse} grouping style.
#'
#' @param dist_mat distance matrix 
#' @param rownames_df data.frame with row identifying information
#' @param colnames_df data.frame with column identifying information
#'
#' @return \code{tidy_dist_mat} object
#' @export
#' 
#' @examples
#' # data creation
#' inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
#' my_dist_mat <- as.matrix(dist(inner_data))
#' 
#' rownames_df <- data.frame(id = 1:3)
#' colnames_df <- data.frame(id = c(1,2,1), id2 = c("f", "f", "s"))
#' 
#' my_tidy_dm <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
#' 
#' # visualizing the structure
#' print(my_tidy_dm)
#' 
#' # accessing structure 
#' rownames(my_tidy_dm)
#' colnames(my_tidy_dm)
#' 
#' # updating structure
#' rownames(my_tidy_dm) <- colnames_df
#' my_tidy_dm
tidy_dist_mat <- function(dist_mat, rownames_df = NULL, colnames_df = NULL){
  object <- dist_mat
  
  dimnames(object) <- NULL
  
  if (is.null(rownames_df)){
    rownames_df <- data.frame(id = 1:nrow(object))
  }
  if (is.null(colnames_df)){
    colnames_df <- data.frame(id = 1:ncol(object))
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
  assertthat::assert_that(all(rownames_df == rn_df_distinct),
                          msg = paste("rownames_df is not distinct",
                                      "(see dplyr::distinct) for help"))
  cn_df_distinct <- dplyr::distinct(colnames_df)
  assertthat::assert_that(all(colnames_df == cn_df_distinct),
                          msg = paste("colnames_df is not distinct",
                                      "(see dplyr::distinct) for help"))
  
  TRUE
}


#' checks if object is a tidy_dist_mat
#'
#' @param x any old object
#'
#' @return boolean logic if object is a \code{tidy_dist_mat} object
#' @export
is.tidy_dist_mat <- function(x){
  inherits(x, "tidy_dist_mat")
}


#' return dimnames for tidy_dist_mat
#' 
#' @param x tidy_dist_mat object
#' @return list of rowname and colnames data.frames
#'
#' @export
dimnames.tidy_dist_mat <- function(x){
  dimnames <- list("rownames" = attr(x, "rownames_df"),
                   "colnames" = attr(x, "colnames_df"))
  return(dimnames)
}

#' assign dimnames for tidy_dist_mat
#' 
#' @param x tidy_dist_mat
#' @param value list of rowname and colnames data.frame to override the 
#' \code{dimnames} of x with.
#' 
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

#' assign rownames for tidy_dist_mat
#'
#' @param x tidy_dist_mat
#' @param value updated rownames
#' 
#' @usage \method{rownames}{tidy_dist_mat}(x) <- value
#' 
#' @export
`rownames<-.tidy_dist_mat` <- function(x, value){
  
  if (!is.list(value) || length(value) != 2L) {
    stop("invalid 'dimnames' given for data frame")
  }
  
  check_tidy_dist_mat_dimensions(x, value, colnames(x))
  
  attr(x, "rownames_df") <- value
  
  x
}

#' assign colnames
#' 
#' @param x tidy_dist_mat
#' @param value updated rownames
#' 
#' @usage \method{colnames}{tidy_dist_mat}(x) <- value
#' 
#' @export
`colnames<-.tidy_dist_mat` <- function(x, value){
  
  if (!is.list(value) || length(value) != 2L) {
    stop("invalid 'dimnames' given for data frame")
  }
  
  check_tidy_dist_mat_dimensions(x, rownames(x),value)
  
  attr(x, "colnames_df") <- value
  
  x
}


#' Format tidy_dist_mat for printing
#'
#' @param x tidy_dist_mat
#' @param ... additional parameters. Currently we allow / use:
#' \itemize{
#'   \item{\code{digit}: }{integer, tells us the number of significant digits to 
#'   present for the distances}
#'   \item{\code{more_rows:} }{boolean, tells us if more rows exist than 
#'   presented}
#'   \item{\code{more_cols:} }{boolean, tells us if more columns exist than 
#'   presented}
#' }
#'
#' @return format ready information
#' @export
format.tidy_dist_mat <- function(x, ...){
  
  additional_params <- list(...)
  if ("digits" %in% names(additional_params)) {
    digits <- additional_params[["digits"]]
  } else {
    digits = 6
  }
  if ("more_rows" %in% names(additional_params)){
    more_rows <- additional_params[["more_rows"]]
  } else {
    more_rows <- F
  }
  if ("more_cols" %in% names(additional_params)){
    more_cols <- additional_params[["more_cols"]]
  } else {
    more_cols <- F
  }
  
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
  
  
  if (more_rows){
    m <- rbind(m, rep("...", ncol(m)))
  }
  
  if (more_cols) {
    m <- cbind(m, rep("...", nrow(m)))
    
  }
  
  
  noquote(m)
}

#' print tidy_dist_mat objects
#'
#' @param x tidy_dist_mat object
#' @param ... (like \code{digits = 6}) number of significant digits to display
#'   (uses \code{signif})
#' @param n maximum number of rows or columns of x to print
#'
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
print.tidy_dist_mat <- function(x, ..., n = NULL){
  if (!is.null(n)){
    n_row <- min(nrow(x), n)
    n_col <- min(ncol(x), n)
    more_rows <- n_row < nrow(x)
    more_cols <- n_col < ncol(x)
    
    print(format(x[1:n_row, 1:n_col], ..., 
                 more_cols = more_cols, more_rows = more_rows))
    
  } else {
    print(format(x,...))
  }
}


#' convert \code{tidy_dist_mat} to \code{matrix}
#'
#' @param x \code{tidy_dist_mat} object
#' @param ... additional arguments to be passed to or from methods. (currently
#' doesn't do anything for this.)
#'
#' @return matrix representation (without column information)
#' @export
#'
as.matrix.tidy_dist_mat <- function(x, ...){
  inner <- unclass(x)
  attr(inner, "rownames_df") <- NULL
  attr(inner, "colnames_df") <- NULL
  
  return(inner)
}

#' @rdname as.matrix.tidy_dist_mat
#' @export
as.array.tidy_dist_mat <- as.matrix.tidy_dist_mat



#' internal function to get the indices of the \code{tidy_dist_mat} related to a data frame.
#'
#' @param x \code{tidy_dist_mat} object
#' @param index data.frame style index related to \code{x}
#' @param margin scalar representing if \code{index} is related to rows or 
#' columns of \code{x}. 1 = row, 2 = column.
#'
#' @return index vector
process_df_index <- function(x, index, margin = 1){
  out <- which_index.tidy_dist_mat(x, index, margin = margin)
  return(out)
}


#' Extract a part of a \code{tidy_dist_mat} object
#'
#' @param x \code{tidy_dist_mat} object
#' @param i indices specifying elements to extract or replace. Indices are 
#' numeric or data frames related to \code{x}'s row information. 
#' @param j indices specifying elements to extract or replace. Indices are 
#' numeric or data frames related to \code{x}'s column information. If no 
#' \code{j} is provided, then \code{j=i} and a square matrix is returned (if 
#' \code{x}'s structure follows correct assumptions).
#'
#' @return a new \code{tidy_dist_mat} object
#' @export
`[.tidy_dist_mat` <- function(x, i, j = i){
  
  # checking and processing i
  assertthat::assert_that(is.data.frame(i) || all(is.wholenumber(i)) ,
                          msg = paste("i needs to be a numeric (integer)",
                                      "vector or a data.frame of the same",
                                      "style as x's rownames"))
  if(is.data.frame(i)){
    i <- process_df_index(x, i, margin = 1)
  }
  assertthat::assert_that(is.data.frame(j) || all(is.wholenumber(j)),
                          msg = paste("j needs to be a numeric (integer)",
                                      "vector or a data.frame of the same",
                                      "style as x's colnames"))
  if(is.data.frame(j)){
    j <- process_df_index(x, j, margin = 2)
  }
  
  x_mat <- as.matrix(x)[i,j,drop = F]
  
  perserve_rownames_class <- class(rownames(x))
  perserve_colnames_class <- class(colnames(x))
  
  rownames_new <- tibble::tibble(rownames(x))[i,]
  class(rownames_new) <- perserve_rownames_class
  colnames_new <- tibble::tibble(colnames(x))[j,] 
  class(colnames_new) <- perserve_colnames_class
  
  
  tidy_dist_mat(x_mat, 
                rownames_df = rownames_new,
                colnames_df = colnames_new)
}

#' characterizes that we will subset by the indices *not* in said data.frame
#'
#' @param x data.frame like object with index structure associated with a 
#' \code{tidy_dist_mat}'s rownames or colnames.
#'
#' @return a new \code{not_df} object
#' @export
not <- function(x){
  UseMethod("not")
}

#' @rdname not
#' @export
not.data.frame <- function(x){
  df2 <- x
  class(df2) <- c("not_df", class(x))
  return(df2)
}

#' removes the \code{not}() operation (that makes a df a \code{not_df})
#'
#' @param x \code{not_df} class object
#'
#' @return x without the \code{not_df} class associated with it
#' @export
reverse_not_df <- function(x){
  UseMethod("reverse_not_df")
}

#' @rdname reverse_not_df
#' @export
reverse_not_df.not_df <- function(x){
  df2 <- x
  class(df2) <- class(x)[class(x) != "not_df"]
  return(df2)
}


#' @rdname not
#' @export
is.not_df <- function(x){
  inherits(x, "not_df")
}

#' find the index of a \code{tidy_dist_mat} relative to a data.frame 
#'
#' probably more of an internal function...
#'
#' @param x \code{tidy_dist_mat} object
#' @param index data.frame index associated with \code{x}'s colnames or
#' rownames (related to \code{margin}). If \code{which_not_index} then this 
#' is which associated information *not* to include.
#' @param margin scalar representing if \code{index} is related to rows or 
#' columns of \code{x}. 1 = row, 2 = column.
#'
#' @return index values associated with rows or columns of \code{x}
#' @export
which_index <- function(x, index, margin = 1){
  UseMethod("which_index")
}

#' @rdname which_index
#' @export
which_index.tidy_dist_mat <- function(x, index,  margin = 1){
  if (is.not_df(index)){
    index <- reverse_not_df(index)
    return(which_not_index(x, index, margin = margin))
  }
  
  
  if (margin == 1){
    info_names <- rownames(x)
    index_name <- "i"
    margin_name <- "rownames"
  } else if (margin == 2) {
    info_names <- colnames(x)
    index_name <- "j"
    margin_name <- "colnames"
    
  } else {
    stop("margin needs to be either 1 or 2.")
  }
  
  assertthat::assert_that(all(names(info_names) == names(index)),
                          msg =  sprintf(
                            paste("%s needs to be a numeric (integer)",
                                  "vector or a data.frame of the same",
                                  "style as x's %s"), 
                            index_name, margin_name))
  
  rnames2 <- info_names %>% dplyr::mutate(`CAPTURE CORRECT` = TRUE)
  index_check <- index %>% dplyr::left_join(rnames2, by = names(index))
  
  assertthat::assert_that(!any(is.null(index_check["CAPTURE CORRECT"])),
                          msg = sprintf(paste("some of %s's rows don't",
                                              "appear in %s(x)"),
                                        index_name,
                                        margin_name))
  
  index2 <- index %>% dplyr::mutate(`CAPTURE CORRECT` = TRUE)
  which_rname <- info_names %>% dplyr::left_join(index2, names(info_names))
  index <- which(!is.na(which_rname["CAPTURE CORRECT"]))
  return(index)
}

#' @rdname which_index
#' @export
which_not_index <- function(x, index, margin = 1){
  UseMethod("which_not_index")
}

#' @rdname which_index
#' @export
which_not_index.tidy_dist_mat <- function(x, index, margin = 1){
  if (is.not_df(index)){
    index <- reverse_not_df(index)
    return(which_index(x, index, margin = margin))
  }
  
  
  
  if (margin == 1){
    info_names <- rownames(x)
    index_name <- "i"
    margin_name <- "rownames"
  } else if (margin == 2) {
    info_names <- colnames(x)
    index_name <- "j"
    margin_name <- "colnames"
    
  } else {
    stop("margin needs to be either 1 or 2.")
  }
  
  n_info <- nrow(info_names)
  
  assertthat::assert_that(all(names(info_names) == names(index)),
                          msg =  sprintf(
                            paste("%s needs to be a numeric (integer)",
                                  "vector or a data.frame of the same",
                                  "style as x's %s"), 
                            index_name, margin_name))
  
  rnames2 <- info_names %>% dplyr::mutate(`CAPTURE CORRECT` = TRUE)
  index_check <- index %>% dplyr::left_join(rnames2, by = names(index))
  
  assertthat::assert_that(!any(is.null(index_check["CAPTURE CORRECT"])),
                          msg = sprintf(paste("some of %s's rows don't",
                                              "appear in %s(x)"),
                                        index_name,
                                        margin_name))
  
  index2 <- index %>% dplyr::mutate(`CAPTURE CORRECT` = TRUE)
  which_rname <- info_names %>% dplyr::left_join(index2, names(info_names))
  index <- which(is.na(which_rname["CAPTURE CORRECT"]))
  return(index)
  
}





#' Return the Last Parts of a \code{tidy_dist_mat} (symmetric grab) 
#'
#' @param x symmetric (in shape) \code{tidy_dist_mat} object
#' @param n an integer of value bounded by the maximum rows \code{x}.
#' @param ... arguments to be passed to or from other methods (currently none
#' expected / used).
#'
#' @return \code{x[1:n]} or \code{x[(nrow(x)-n):nrow(x)]}
#' @export
#' @importFrom utils head
head.tidy_dist_mat <- function(x, n = 6L, ...){
  assertthat::assert_that(ncol(x) == nrow(x),
                          msg = paste("currently only able to to produce the",
                                      "head of a tidy_dist_mat if it has the",
                                      "same number of rows and columns"))
  n_row <- min(nrow(x), n)
  
  x[1:n_row]
}


#' Return the Last Parts of a \code{tidy_dist_mat} (symmetric grab) 
#'
#' @param x symmetric (in shape) \code{tidy_dist_mat} object
#' @param n an integer of value bounded by the maximum rows \code{x}.
#' @param ... arguments to be passed to or from other methods (currently none
#' expected / used).
#'
#' @return \code{x[(nrow(x)-n):nrow(x)]}
#' @export
#' @importFrom utils tail
tail.tidy_dist_mat <- function(x, n = 6L, ...){
  assertthat::assert_that(ncol(x) == nrow(x),
                          msg = paste("currently only able to to produce the",
                                      "tail of a tidy_dist_mat if it has the",
                                      "same number of rows and columns"))
  nr <- nrow(x)
  n_row <- min(nrow(x), n)
  if (n_row < nr){
    x[(nr-n):nr]
  } else {
    x
  }
}


if (r_new_interface()){
  .S3method(generic = "dimnames", class = "tidy_dist_mat") 
  .S3method(generic = "dimnames<-", class = "tidy_dist_mat") 
  .S3method(generic = "rownames<-", class = "tidy_dist_mat") 
  .S3method(generic = "colnames<-", class = "tidy_dist_mat") 
  
  .S3method(generic = "format", class = "tidy_dist_mat") 
  .S3method(generic = "print", class = "tidy_dist_mat") 
  
  .S3method(generic = "as.matrix", class = "tidy_dist_mat")
  .S3method(generic = "as.array", class = "tidy_dist_mat")
  .S3method(generic = "[", class = "tidy_dist_mat")
  .S3method(generic = "which_index", class = "tidy_dist_mat")
  .S3method(generic = "which_not_index", class = "tidy_dist_mat")
  .S3method(generic = "head", class = "tidy_dist_mat")
  .S3method(generic = "tail", class = "tidy_dist_mat")
  
  .S3method(generic = "not", class = "data.frame")
  
  .S3method(generic = "reverse_not_df", class = "not_df")
}

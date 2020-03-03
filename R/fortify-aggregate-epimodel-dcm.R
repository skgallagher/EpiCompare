
#'  Take external aggregate data and put it in a format used in this package
#'
#' @param ext_data output from external source package.  See details
#' @param states names of states we want aggregate totals of at each time
#' @param package_source optional argument to include the package from which the output is derived from, which helps with the fortify function when outputs are of generic classes such as list or data.frame
#' @return a data frame with the following columns
#' \describe{
#' \item{t}{time}
#' \item{Xk}{columns X0, ..., X_K. which are numeric}
#' }
#' @details This function converts external data sources (we currently support output from the EpiModel and pomp R packages), which is already aggregated and puts it in a format that can be used by our exploring functions.
#' @export
#' @examples
#' ## For dcm
#' sir1 <- fortify_EpiModel(EpiModel_det)
#' head(sir1)
#' class(sir1)
#' @export
fortify_aggregate_ext.dcm <- function(ext_data,
                                  states = NULL,
                                  package_source = NULL){

    out <- fortify_aggregate_ext.icm(ext_data)
    return(out)

}



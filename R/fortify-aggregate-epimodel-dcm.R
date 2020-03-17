#' Take external aggregate data and put it in a format used in this package
#'
#' @param data output from external source package.  See details
#' @param states names of states we want aggregate totals of at each time
#' @param package_source optional argument to include the package from which the
#'   output is derived from, which helps with the fortify function when outputs
#'   are of generic classes such as list or data.frame
#' @return a data frame with the following columns
#' \describe{
#' \item{t}{time}
#' \item{Xk}{columns X0, ..., X_K. which are numeric}
#' }
#' @details This function converts external data sources (we currently support
#'   output from the EpiModel and pomp R packages), which is already aggregated
#'   and puts it in a format that can be used by our exploring functions.
#' @export
#' @examples
#' ## For dcm
#' out <- fortify_aggregate(EpiModel_det)
#' head(out)
#' @export
fortify_aggregate.dcm <- function(data,
                                  states = NULL,
                                  package_source = NULL){

    out <- fortify_aggregate.icm(data, states)
    return(out)

}



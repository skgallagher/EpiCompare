




#' Takes in data from the R pomp package  where the output is a data frame and puts it in SIR format for timeternR
#'
#' @param data Output from a pomp simulation where the output is a data frame, \code{pomp::simulate()}
#' @param states vector of state names
#' @param package_source optional package name 
#' @details The default variables that are retained are SIR, but can be modified with the \code{states} argument.  If code{states = NULL}, we will attempt to find all single letter names in POMP and output those.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' \item{Xk}{where k = 0, ..., K}
#' }
#' @export
fortify_aggregate.list <- function(data,
                                           states = NULL,
                                           package_source = NULL){

    if(!is.null(package_source) & package_source == "pomp"){
        class(data) <- c("pomp_list", class(data))
        out <- fortify_aggregate.pomp_list(data,
                                               states = states)
        return(out)
    }
    stop("We currently do not support fortifying an object of class 'list' if 'package_source' is not 'pomp'")
 
}


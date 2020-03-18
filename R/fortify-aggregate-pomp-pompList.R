#' Takes in data from the R pomp package  where the output is a data frame and
#' puts it in SIR format for EpiCompare
#'
#' @param data Output from a pomp simulation where the output is a data frame,
#'   \code{pomp::simulate()}
#' @param states vector of state names
#' @param package_source optional package name
#' @details The default variables that are retained are SIR, but can be modified
#'   with the \code{states} argument.  If code{states = NULL}, we will attempt
#'   to find all single letter names in POMP and output those.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' \item{Xk}{where k = 0, ..., K}
#' }
#' @export
#' @examples
#' out <- fortify_aggregate(pomp_arr, package_source = "pomp",
#' states = c("S", "I", "R"))
#' head(out)
#' unique(rowSums(out[, 3:5]))
fortify_aggregate.pompList <- function(data,
                                           states = NULL,
                                           package_source = NULL){
    pomp_output <- data
    df <- as.data.frame(pomp_output)
    out <- fortify_aggregate.pomp_df(df, states = states)


}


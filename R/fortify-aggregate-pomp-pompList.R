
#' Generic method that takes in data from the R pomp package and puts it in SIR format
#'
#' @param data Output from a pomp simulation, \code{pomp::simulate()}
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
#' @examples
#' data(pomp_df)
#' fortified_df <- fortify_pomp(pomp_df)
#' data(pomp_pomp)
#' fortified_pomp <- fortify_pomp(pomp_pomp)
#' fortified_arr <- fortify_pomp(pomp_arr)
#' assertthat::are_equal(fortified_df, fortified_pomp)
#' assertthat::are_equal(fortified_pomp, fortified_arr)
#' head(fortified_df)
#' class(fortified_df)
#' @export
fortify_pomp <- function(data){

    UseMethod("fortify_pomp")

}





#' Takes in data from the R pomp package  where the output is a data frame and puts it in SIR format for timeternR
#'
#' @param ext_data Output from a pomp simulation where the output is a data frame, \code{pomp::simulate()}
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

fortify_aggregate_ext.pompList <- function(ext_data,
                                           states = NULL,
                                           package_source = NULL){
    pomp_output <- ext_data
    df <- as.data.frame(pomp_output)
    class(df) <- c("pomp_df", class(df)) 
    out <- fortify_aggregate_ext.pomp_df(df, states = states)

 
}


#' Takes in data from the R pomp package  where the output is a data frame and
#' puts it in SIR format for EpiCompare
#'
#' @param data Output from a pomp simulation where the output is a data frame,
#'   \code{pomp::simulate()}, and we have added the name pomp_df to the class
#'   names.
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
#' out <- fortify_aggregate(pomp_df, package_source = "pomp",
#' states = c("S", "I", "R"))
#' head(out)
#' unique(rowSums(out[, 3:5]))
fortify_aggregate.pomp_df <- function(data,
                                           states = c("S", "I", "R"),
                                          package_source = NULL){

    browser()
    ## pull out state names
    if(class(states!= "quosure")){
        state_cols <- dplyr::enquos(states)
    }
    states <- unname(tidyselect::vars_select(colnames(data),
                                             !!!state_cols))


    if(length(states) == 0){
        states <- grep("^[A-Z]{1}$", colnames(data),
                       value = TRUE)
    }

    pomp_output <- data
    out <- pomp_output %>%
        dplyr::rename(t = "time", sim = ".id") %>%
        dplyr::select(dplyr::one_of(c("t", "sim", states))) %>%
        dplyr::mutate(sim = factor(.data$sim, ordered = FALSE)) %>%
        dplyr::arrange(dplyr::desc(-as.numeric(.data$sim)))

    colnames(out)[-c(1:2)] <- paste0("X", 0:(ncol(out)-3))


    ## #TODO: How do we handle non integer t?

    return(out)



}

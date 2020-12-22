#' Takes in data from the R pomp package  where the output is a data frame and
#' puts it in SIR format for EpiCompare.
#'
#' @param data Output from a pomp simulation where the output is a data frame,
#'   \code{pomp::simulate()}
#' @param states vector of state names.  These are strings.
#' @param package_source optional package name
#' @details The default variables that are retained are SIR, but can be modified
#'   with the \code{states} argument.  If code{states = NULL}, we will attempt
#'   to find all single letter names in POMP and output those.  This is an internal function.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' \item{Xk}{where k = 0, ..., K}
#' }
fortify_aggregate.pomp_list <- function(data,
                                           states = c("S", "I", "R"),
                                           package_source = NULL){

    nms <- dimnames(data$states)
    nm_vars <- nms$variable

    states <- unname(tidyselect::vars_select(nm_vars,
                                             !!states))

    if(length(states) == 0){

        states <- grep("^[A-Z]{1}$", nms$variable,
                       value = TRUE)
    }

    pomp_output <- data
    arr <- pomp_output[[1]]

    out <- arr %>%
        as.data.frame.table() %>%
        dplyr::mutate(t = as.numeric(.data$time) - 1,
                      sim = as.numeric(.data$rep)) %>%
        tidyr::pivot_wider(values_from = .data$Freq,
                           names_from = .data$variable) %>%
        as.data.frame() %>%
        dplyr::select(dplyr::one_of(c("t", "sim", states))) %>%
        dplyr::arrange(dplyr::desc(-.data$sim))
    
    out$sim <- factor(out$sim)
    colnames(out)[-c(1:2)] <- paste0("X", 0:(ncol(out)-3))


    return(out)
}


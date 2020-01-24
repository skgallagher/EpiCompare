
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
#' @param data Output from a pomp simulation where the output is a data frame, \code{pomp::simulate()}
#' @details We require that the variables "S", "I", and "R" must be states in the pomp output.  Moreover, we will assume that these are the only relevant variables in the SIR calculation.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
#' @export
fortify_pomp.data.frame <- function(data){

    pomp_output <- data
    out <- pomp_output %>%
        dplyr::rename(t = "time", sim = ".id") %>%
        dplyr::select(.data$t, .data$S, .data$I, .data$R, .data$sim) %>%
        dplyr::mutate(sim = factor(.data$sim, ordered = FALSE)) %>%
        dplyr::arrange(dplyr::desc(-as.numeric(.data$sim)))
    class(out) <- c("aggregate", "fortified_df", class(out))
    attr(out, "source") <- "pomp"
#    class(out$sim) <- "factor"

    ## #TODO: How do we handle non integer t?
    return(out)
}


#' Takes in data from the R pomp package  where the output is pomp and puts it in SIR format for timeternR
#'
#' @param data Output from a pomp simulation where the output is 'pomp', \code{pomp::simulate()}
#' @details We require that the variables "S", "I", and "R" must be states in the pomp output.  Moreover, we will assume that these are the only relevant variables in the SIR calculation.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
#' @export
fortify_pomp.pompList <- function(data){
    pomp_output <- data
    df <- as.data.frame(pomp_output)
    out <- fortify_pomp.data.frame(df)
    return(out)
}


#' Takes in data from the R pomp package  where the output is array and puts it in SIR format for timeternR
#'
#' @param data Output from a pomp simulation where the output is 'array', \code{pomp::simulate()}
#' @param ... additional arguments
#' @details We require that the variables "S", "I", and "R" must be states in the pomp output.  Moreover, we will assume that these are the only relevant variables in the SIR calculation.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
#' @export
fortify_pomp.list <- function(data){
    pomp_output <- data
    arr <- pomp_output[[1]]
    if(tidyr_new_interface()){
        out <- arr %>%
            as.data.frame.table() %>%
            dplyr::mutate(t = as.numeric(.data$time) - 1,
                          sim = as.numeric(.data$rep)) %>%
            tidyr::pivot_wider(values_from = .data$Freq,
                               names_from = .data$variable) %>%
            as.data.frame() %>%
            dplyr::select(.data$t, .data$S, .data$I, .data$R, .data$sim) %>%
            dplyr::arrange(dplyr::desc(-.data$sim))
    } else{

        out <- arr %>%
            as.data.frame.table() %>%
            dplyr::mutate(t = as.numeric(.data$time) - 1,
                          sim = as.numeric(.data$rep)) %>%
            tidyr::spread(.data$variable,
                               .data$Freq) %>%
            as.data.frame() %>%
            dplyr::select(.data$t, .data$S, .data$I, .data$R, .data$sim) %>%
            dplyr::arrange(dplyr::desc(-.data$sim))
    }
    out$sim <- factor(out$sim)
    class(out) <- c("aggregate", "fortified_df", class(out))
    attr(out, "source") <- "pomp"
    return(out)
 }


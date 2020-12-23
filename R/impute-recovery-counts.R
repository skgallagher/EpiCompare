## SKG
## April 16, 2020
## Imputing R (# recovered) from cumulative case counts




#' Impute Recovered counts for the SIR model
#'
#' @param data data frame or grouped data frame with the following columns
#' \describe{
#' \item{t}{time}
#' \item{confirmed}{number of cumulative confirmed cases at time t in the group}
#' \item{N}{population size, which is used as the number of susceptible (minus the initial infections)}
#' }
#' @param par named vector of parameters
#' @param method Currently default is "chain-binomial".  See details.  More methods to come.
#' @return the input data with the additional columns
#' \describe{
#' \item{X0}{number of susceptible}
#' \item{X1}{number of infectious}
#' \item{X2}{number of recovered}
#' }
#' @details For the method "chain-binomial". 
#'  Let the cumulative case counts at time \eqn{t} be \eqn{J_t}. 
#'   Then the number of susceptibles is simply \eqn{S_t = N - J_t}. 
#'    The number of infectious and recovered is imputed iteratively
#'     using random draws from a chain binomial based on the
#'      state sizes at the previous time step. 
#'   Specifically, we assume \eqn{I_{t_0} = J_{t_0}} and 
#'  \eqn{R_{t_0} = 0}, that is the initial number of recovered
#'   individuals is zero.  Then for each 
#'   \eqn{ t \in \{ t_0 + 1, t_0 + 2, \dots, T\}} 
#'   \eqn{R_t = R_{t-1} +} Binomial\eqn{(I_{t-1}, \gamma)} 
#'   and \eqn{I_t = J_t - R_t}. 
#'    Here \eqn{(X0, X1, X2) = (S, I, R)}.
#' @export
#' @examples
#'   df <- data.frame(t = 0:4,
#'                     confirmed = c(0, 1, 3, 9, 9),
#'                     N = 10)
#'    out <- cases_to_SIR(data = df,
#'                        par = 1)
cases_to_SIR <- function(data, par,
                     method = "chain-binomial"){
    stopifnot(par[1] >= 0 & par[1] <= 1)
    stopifnot(all(c("t", "confirmed", "N") %in% colnames(data)))

    UseMethod("cases_to_SIR")
    

}




#' Impute Recovered counts for the SIR model
#'
#' @param data data frame with the following columns
#' \describe{
#' \item{t}{time}
#' \item{confirmed}{number of cumulative confirmed cases at time t in the group}
#' \item{N}{population size, which is used as the number of susceptible (minus the initial infections)}
#' }
#' @param par named vector of parameters
#' @param method Currently default is "chain-binomial".  See details.  More methods to come.
#' @return the input data with the additional columns
#' \describe{
#' \item{X0}{number of susceptible}
#' \item{X1}{number of infectious}
#' \item{X2}{number of recovered}
#' }
#' @export
#' @examples
#'   df <- data.frame(t = 0:4,
#'                     confirmed = c(0, 1, 3, 9, 9),
#'                     N = 10)
#'    out <- cases_to_SIR(data = df,
#'                        par = 1)
cases_to_SIR.data.frame <- function(data, par,
                     method = "chain-binomial"){
    new_data <- data %>% dplyr::mutate(X0 = .data$N - .data$confirmed,
                                       X1 = .data$confirmed,
                                       X2 = 0) 

    ## Iteratively choose recoveries based on past infection size
    for(ii in 2:nrow(new_data)){
        new_recoveries <- stats::rbinom(n = 1, size = new_data$X1[ii-1],
                                 prob = as.numeric(par[1]))
        new_data$X2[ii] <- new_data$X2[ii-1] + new_recoveries
        new_data$X1[ii] <- new_data$confirmed[ii] - new_data$X2[ii]
    }
    return(new_data)


}




#' Impute Recovered counts for the SIR model
#'
#' @param data data frame or grouped data frame with the following columns
#' \describe{
#' \item{t}{time}
#' \item{confirmed}{number of cumulative confirmed cases at time t in the group}
#' \item{N}{population size, which is used as the number of susceptible (minus the initial infections)}
#' }
#' @param par named vector of parameters
#' @param method Currently default is "chain-binomial".  See details.  More methods to come.
#' @return the input data with the additional columns
#' \describe{
#' \item{X0}{number of susceptible}
#' \item{X1}{number of infectious}
#' \item{X2}{number of recovered}
#' }
#' @export
#' @examples
#'   df <- data.frame(t = 0:4,
#'                     confirmed = c(0, 1, 3, 9, 9),
#'                     N = 10)
#'    out <- cases_to_SIR(data = df,
#'                        par = 1)
cases_to_SIR.grouped_df <- function(data, par,
                     method = "chain-binomial"){
  out <- data %>%
    tidyr::nest() %>%
    dplyr::mutate(update = purrr::map(.data$data, cases_to_SIR,
                                      par = par)) %>%
    dplyr::select(-.data$data) %>%
    tidyr::unnest(cols = c(.data$update)) # only change
  
  return(out)
    
}

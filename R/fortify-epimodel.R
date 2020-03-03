
#' Generic method that takes in data from the \code{EpiModel} package and puts it in aggregate, SIR format
#' 
#' @param data Output from a EpiModel simulation of class \code{icm} or \code{dcm}
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{Xk}{where k = 0, ..., K.}

#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
#' @examples
#' ## For icm
#' sir <- fortify_EpiModel(EpiModel_icm)
#' head(sir)
#' class(sir)
#' @export
fortify_epimodel <- function(data){

    UseMethod("fortify_EpiModel")

}



#' Takes in output from the \code{R} \code{EpiModel} package in \code{icm} format and puts it in SIR format
#'
#' @param data output from  \code{EpiModel::icm}
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
#' @details Take the output from \code{EpiModel::icm} and turn it into an SIR data.frame for plotting.
#' @examples
#' ## For icm
#' sir <- fortify_EpiModel(EpiModel_icm)
#' head(sir)
#' class(sir)
#' @export
fortify_EpiModel.icm <- function(data){
  if(!all(c("s.num", "i.num", "r.num" ) %in% names(data$epi))){
    stop("This is not in SIR format")
  } ## Don't have extra states
  if(sum(grepl(".num", names(data$epi))) > 3){
    warning("There is at least one extra compartment we are ignoring")
  }
  EpiModel_output <- data

  ## Actual formatting

  n_sim <- EpiModel_output$control$nsims
  S_mat <- EpiModel_output$epi$s.num
  I_mat <- EpiModel_output$epi$i.num
  R_mat <- EpiModel_output$epi$r.num
  if(tidyr_new_interface()){
      S_df <- tidyr::pivot_longer(as.data.frame(S_mat), cols = tidyr::everything(),
                                  names_to = "sim",
                                  values_to = "S")
      I_df <- tidyr::pivot_longer(as.data.frame(I_mat), cols = tidyr::everything(),
                                  names_to = "sim",
                                  values_to = "I")
      R_df <- tidyr::pivot_longer(as.data.frame(R_mat), cols = tidyr::everything(),
                                  names_to = "sim",
                                  values_to = "R")
  } else{
      S_df <- tidyr::gather(as.data.frame(S_mat), key = "sim", value = "S")
      I_df <- tidyr::gather(as.data.frame(I_mat), key = "sim", value = "I")
      R_df <- tidyr::gather(as.data.frame(R_mat), key = "sim", value = "R")
  }
  t <- rep(1:EpiModel_output$control$nsteps, ncol(S_mat))
  SIR_df <- data.frame(t = t, S = S_df$S, I = I_df$I,
                       R = R_df$R, sim = S_df$sim)

  ## Reformulate to proper form
  N <- sum(SIR_df[1, c("S", "I", "R")])
  Ns <- rowSums(SIR_df[, c("S", "I", "R")])
  if(!assertthat::are_equal(Ns, rep(N, length(Ns)))){
    warning("The number of agents is not constant over time")
  }

  class(SIR_df) <- c("aggregate", "fortified_df",  class(SIR_df))
  attr(SIR_df, "source") <- "EpiModel"

  return(SIR_df)
}


#' Takes in output from the \code{R} \code{EpiModel} package in the \code{dcm} class and puts it in SIR format
#'
#' @param data output from \code{EpiModel::dcm}
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
#' @details Take the output from \code{EpiModel::dcm} and turn it into an SIR data.frame for plotting.
#' @examples
#' ## For dcm
#' sir1 <- fortify_EpiModel(EpiModel_det)
#' head(sir1)
#' class(sir1)
#' @export
fortify_EpiModel.dcm <- function(data){

    EpiModel_output <- data
    ## Some checks
  if(!all(c("s.num", "i.num", "r.num" ) %in% names(EpiModel_output$epi))){
    stop("This is not in SIR format")
  } ## Don't have extra states
  if(sum(grepl(".num", names(EpiModel_output$epi))) > 3){
    warning("There is at least one extra compartment we are ignoring")
  }

    ## Actual formatting

    t <- EpiModel_output$control$timesteps
    S <- EpiModel_output$epi$s.num
    names(S) <- NULL
    I <- EpiModel_output$epi$i.num
    names(I) <- NULL
    R <- EpiModel_output$epi$r.num
    names(R) <- NULL
    SIR_df <- data.frame(t = t, S = S, I = I, R = R)


    N <- sum(SIR_df[1, c("S", "I", "R")])
    Ns <- rowSums(SIR_df[, c("S", "I", "R")])
    if(!assertthat::are_equal(Ns, rep(N, length(Ns)))){
        warning("The number of agents is not constant over time")
    }

    class(SIR_df) <- c("aggregate", "fortified_df", class(SIR_df))
    attr(SIR_df, "source") <- "EpiModel"

  return(SIR_df)
}



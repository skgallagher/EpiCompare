## Fortify functions




#' Takes in data from the R pomp package and puts it in SIR format
#'
#' @param pomp_output Output from a pomp simulation, \code{pomp::simulate()}
#' @return data frame with the following columns
#' @details \describe{
#' \item{t}{}
#' }
fortify_pomp <- function(pomp_output){
  if(!(class(pomp_output) %in% c("data.frame", "pomp", "array"))){
    stop("Pomp output must be from pomp::simulate and of one of a 'data.frame', 'pomp' or 'array' output")
  }


}


#' Takes in output from the \code{R} \code{EpiModel} package and puts it in SIR format
#'
#' @param EpiModel_output output from either \code{EpiModel::dcm} or \code{EpiModel::icm}
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
#'
#' ## For icm
#' sir2 <- fortify_EpiModel(EpiModel_icm)
#' head(sir2)
#' @export
fortify_EpiModel <- function(EpiModel_output){
  object_class <- class(EpiModel_output)
  ## Some basic checks
  ## Correct Class
  if(!(object_class %in% c("icm", "dcm"))){
    stop("EpiModel_output needs to be of class 'icm' or 'dcm'")
  } ## Have s, i, and r states
  if(!all(c("s.num", "i.num", "r.num" ) %in% names(EpiModel_output$epi))){
    stop("This is not in SIR format")
  } ## Don't have extra states
  if(sum(grepl(".num", names(EpiModel_output$epi))) > 3){
    warning("There is at least one extra compartment we are ignoring")
  }

  ## Actual formatting

  if(object_class == "icm"){
    n_sim <- EpiModel_icm$control$nsims
    S_mat <- EpiModel_icm$epi$s.num
    I_mat <- EpiModel_icm$epi$i.num
    R_mat <- EpiModel_icm$epi$r.num
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
    t <- rep(1:EpiModel_icm$control$nsteps, ncol(S_mat))
    SIR_df <- data.frame(t = t, S = S_df$S, I = I_df$I,
                         R = R_df$R, sim = S_df$sim)



  } else if(object_class == "dcm"){

    t <- EpiModel_det$control$timesteps
    S <- EpiModel_det$epi$s.num
    names(S) <- NULL
    I <- EpiModel_det$epi$i.num
    names(I) <- NULL
    R <- EpiModel_det$epi$r.num
    names(R) <- NULL
    SIR_df <- data.frame(t = t, S = S, I = I, R = R)
  }

  N <- sum(SIR_df[1, c("S", "I", "R")])
  Ns <- rowSums(SIR_df[, c("S", "I", "R")])
  if(!assertthat::are_equal(Ns, rep(N, length(Ns)))){
    stop("The number of agents is not constant over time")
  }

  return(SIR_df)
}

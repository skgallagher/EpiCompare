## Fortify functions




#' Takes in data from the R pomp package and puts it in SIR format
#'
#' @param pomp_output Output from a pomp simulation, \code{pomp::simulate()}
#' @return data frame with the following columns
#' @details \describe{
#' \item{t}{}
#' }
fortify_pomp <- function(pomp_output){
  return(0)
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
#' \item{sim}{simulation number (optional column)}
#' }
#' @details
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
  if(any(rowSums(SIR_df[, c("S", "I", "R")]) != N)){
    stop("The number of agents is not constant over time")
  }

  return(SIR_df)
}

## Fortify functions




#' Takes in data from the R pomp package and puts it in SIR format
#'
#' @param pomp_output Output from a pomp simulation, \code{pomp::simulate()}
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
#' head(pomp_df)
#' @export
fortify_pomp <- function(pomp_output){
    pomp_class <- class(pomp_output)
    if(!(pomp_class %in% c("data.frame", "pompList", "list"))){
        stop("Pomp output must be from pomp::simulate and of one of a 'data.frame', 'pompList' or 'array' output")
    }
    if(pomp_class == "data.frame"){
        df_f <- fortify_pomp.df(pomp_output)
    } else if(pomp_class == "pompList"){
        df_f <- fortify_pomp.pomp(pomp_output)
    } else if(pomp_class == "list"){
        df_f <- fortify_pomp.arr(pomp_output)
    }


    return(df_f)
}

#' Takes in data from the R pomp package  where the output is a data frame and puts it in SIR format for timeternR
#'
#' @param pomp_output Output from a pomp simulation where the output is a data frame, \code{pomp::simulate()}
#' @details We require that the variables "S", "I", and "R" must be states in the pomp output.  Moreover, we will assume that these are the only relevant variables in the SIR calculation.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
fortify_pomp.df <- function(pomp_output){
    out <- pomp_output %>%
        dplyr::rename(t = "time", sim = ".id") %>%
        dplyr::select(t, S, I, R, sim) %>%
        dplyr::mutate(sim = factor(sim))

    ## #TODO: How do we handle non integer t?
    return(out)
}

#' Takes in data from the R pomp package  where the output is array and puts it in SIR format for timeternR
#'
#' @param pomp_output Output from a pomp simulation where the output is 'array', \code{pomp::simulate()}
#' @details We require that the variables "S", "I", and "R" must be states in the pomp output.  Moreover, we will assume that these are the only relevant variables in the SIR calculation.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
fortify_pomp.arr <- function(pomp_output){
    arr <- pomp_output[[1]]
    out <- arr %>%
        as.data.frame.table() %>%
        dplyr::mutate(t = as.numeric(time) - 1,
                      sim = as.numeric(rep)) %>%
        tidyr::pivot_wider(values_from = Freq,
                           names_from = variable) %>%
        as.data.frame() %>%
        dplyr::select(t, S, I, R, sim)
    return(out)
 }


#' Takes in data from the R pomp package  where the output is pomp and puts it in SIR format for timeternR
#'
#' @param pomp_output Output from a pomp simulation where the output is 'pomp', \code{pomp::simulate()}
#' @details We require that the variables "S", "I", and "R" must be states in the pomp output.  Moreover, we will assume that these are the only relevant variables in the SIR calculation.
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
fortify_pomp.pomp <- function(pomp_output){
    df <- as.data.frame(pomp_output)
    out <- fortify_pomp.df(df)
    return(out)
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

## Fortify functions




#' Fortify agent data frame with columns when individual stops being susceptible
#' and stops being infected (as well as initial state).
#'
#' @param data data frame, agent based data frame, class ("individuals_df")
#' @param time_col length 2 string vector, column names recording when
#' individual is infected and when they enter the recovery stage
#' @param max_time int, maximum time for infection process
#' @return \code{fortified_df} data frame, the \code{raw_df} plus three
#' additional columns:
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was susceptible (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#'
#'
#' @export
#'
#' @examples
#' fortify_df <- fortify_agents(timeternR::hagelloch_raw,
#'                              time_col = c("tI","tR"),
#'                              max_time = 95)
#' assertthat::are_equal(fortify_df[,c("init_state",
#' "max_time_S", "max_time_I")],
#'                       timeternR::hagelloch_agents)
fortify_agents <- function(data, time_col = c("tI","tR"),
                           max_time = floor(
                               max(raw_df[,time_col], na.rm = TRUE)) + 1){
    raw_df <- data

  assertthat::assert_that(inherits(time_col, "character") &&
                          length(time_col) == 2 &&
                          all(time_col %in% names(raw_df)),
                          msg = paste("time_col should be a string vector of",
                                      "length 2 that has column names relative",
                                      "to raw_df."))
  N <- nrow(raw_df)

  # initial state (was the individual the original one infected?)
  A0 <- rep(0, N)
  initial_inf <- intersect(which(raw_df[,time_col[1]] < 0),
                           which(raw_df[,time_col[2]] >= 0))
  initial_rec <- intersect(which(raw_df[,time_col[1]] < 0),
                           which(raw_df[,time_col[2]] < 0))
  A0[initial_inf] <- 1
  A0[initial_rec] <- 2



  ## round I and R time - going to use floor
  SMax <- ceiling(raw_df[,time_col[1]])
  SMax <- ifelse(SMax > max_time-1, max_time-1, SMax)
  IMax <- ceiling(raw_df[,time_col[2]])
  IMax <- ifelse(IMax > max_time-1, max_time-1, IMax)
  agents <- data.frame(init_state = factor(A0),
                  max_time_S = SMax,
                  max_time_I = IMax)
  # dealing with initially infected
  agents[union(initial_inf, initial_rec),"max_time_S"] <- NA
  agents[initial_rec,"max_time_I"] <- NA


  inner_na_agents <- is.na(agents[,c("max_time_S", "max_time_I")])

  if (sum(inner_na_agents) > 0){
    # check NAs are logical
    assertthat::assert_that(all(inner_na_agents[-c(initial_inf, initial_rec),1] <=
                                  inner_na_agents[-c(initial_inf, initial_rec),2]),
                            msg = paste("Please manually correct the fact that",
                                        "an individual has a NA for the time",
                                        "they reached the Infected stage, but",
                                        "a time for when they reached the",
                                        "Recovered stage."))


    ### standard clean up of NAs
    # update agents
    agents[agents$init_state == 0,
      c("max_time_S", "max_time_I")][inner_na_agents[agents$init_state == 0,]] <- max_time
    agents[agents$init_state == 1,
      "max_time_I"][inner_na_agents[agents$init_state == 1,2]] <- max_time

  }


  fortified_df <- cbind(raw_df, agents)
  class(fortified_df) <- c("agents_df", class(fortified_df))

  return(fortified_df)
}




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
    out <- arr %>%
        as.data.frame.table() %>%
        dplyr::mutate(t = as.numeric(.data$time) - 1,
                      sim = as.numeric(.data$rep)) %>%
        tidyr::pivot_wider(values_from = .data$Freq,
                           names_from = .data$variable) %>%
        as.data.frame() %>%
        dplyr::select(.data$t, .data$S, .data$I, .data$R, .data$sim) %>%
        dplyr::arrange(dplyr::desc(-.data$sim))
    out$sim <- factor(out$sim)
    class(out) <- c("aggregate", "fortified_df", class(out))
    attr(out, "source") <- "pomp"
    return(out)
 }






#' Generic method that takes in data from the \code{EpiModel} package and puts it in aggregate, SIR format
#' 
#' @param data Output from a EpiModel simulation of class \code{icm} or \code{dcm}
#' @return data frame with the following columns
#' \describe{
#' \item{t}{the time}
#' \item{S}{number of Susceptibles}
#' \item{I}{number of Infectious}
#' \item{R}{number of Recovered}
#' \item{sim}{simulation number (factor variable) (optional column)}
#' }
#' @examples
#' ## For icm
#' sir <- fortify_EpiModel(EpiModel_icm)
#' head(sir)
#' class(sir)
#' @export
fortify_EpiModel <- function(data){

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



#' Transform the array of simulations to a data frame
#'
#' @param data n_sims x 3 x n_agents where entry (i,j,k) is the ith
#' simulation, the jth statistic and the kth agent.
#' @return data.frame with. The output is
#' a data.frame with columns agent_id, init_state, I_max, R_max, sim_num.  The
#' size is (n_agents x n_sims) x 5.
#' @export
#' @examples
#' sims_array <- array(c(1, 0, 1, 0, 1, 1), dim = c(1, 3, 2))
#' fortify_sims(sims_array)
fortify_sims <- function(data){
  sims_data <- data
  array_dim <- dim(sims_data)
  n_sims <- array_dim[1]
  n_agents <- array_dim[3]
  stopifnot(array_dim[2] == 3)
  dimnames(sims_data) <- list(sim = 1:n_sims,
                              agents_stat = c("init_state", "max_time_S",
                                         "max_time_I"),
                              agent_id = 1:n_agents)
  df <- as.data.frame.table(sims_data)


  if (tidyr_new_interface()){
    df_spread <- df %>% tidyr::pivot_wider(names_from = .data$agents_stat,
                                           values_from = .data$Freq) %>%
      dplyr::select(dplyr::one_of("init_state", "max_time_S",
                                  "max_time_I", "sim", "agent_id"))
  } else {
    df_spread <- df %>% tidyr::spread(key = .data$agents_stat,
                                      value = .data$Freq) %>%
      dplyr::select(dplyr::one_of("init_state", "max_time_S",
                                  "max_time_I", "sim", "agent_id"))
  }
  class(df_spread) <- c("agents", "fortified_df", class(df_spread))
  attr(df_spread, "source") <- "sims"
  return(df_spread)

}


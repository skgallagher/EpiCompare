## SKG
## Aug 21, 2021
## How are we stil working on this project
##
## Estimate best likelihood parameters for the three group SIR case, 7 total compartments
## ## S1, S2, S3, I1, I2, I3, R (where 1 = prek, 2 = 1st class, 3 = second class)
## Nine infection parameters: where betaij is rate of infection
## from group i infectious to group j susceptible
## beta11, beta12, beta13
## beta21, beta22, beta23
## beta31, beta32, beta33
##
## recovery parameter gamma -- constant across all classes
##
## Likelihood is going to be and individual agent level along prob of infection data frame
## Need initial state, time of infection, time of recovery
##


#' Get the full likelihood for SIR for K groups
#'
#' @param par vector of parameters (beta_vec, gamma).  Must be named
#' @param agents_df data frame of agents with columns agent_id, .
#' init_state, tI, tR, infector_id
#' @param grouping_var name of grouping variable
#' @param agg_df data frame with cumulative number in each state at each time for each group
#' columns t, S1, ..., SK, I1, ..., IK, ..., R1, ...RK
#' @return loglike for groups
#' @details See likelihood calc
#' TODO: make pretty so betas are generalizable for K groups.  Want to turn beta_vec into a matrix
#'
loglike_groups_internal <- function(par,
                                    agents_df, grouping_var,
                           agg_df){
  


  agents_df$infector_group <- get_infector_group(agents_df)


  ## returns long df with t, pI, pR, grouping_var, infector_group
  group_prob_df <- get_prob_of_inf(par, agg_df,
                                   grouping_var,
                                   )

  joined_df <- dplyr::left_join(agents_df, group_prob_df,
                                by = c(grouping_var, "infector_group"))

  trimmed_df <- joined_df %>%
    group_by(agent_id) %>%
    filter(t <= tR)  ## Don't need items after recovery


  agent_log
  like <- trimmmed_df %>%
    group_by(agent_id) %>%
    summarize(loglike = loglike_agent(.data$t,
                                      .data$pI,
                                      .data$pR,
                                      .data$grouping_var,
                                      .data$infector_group
                                      ))

  loglike <- sum(agent_loglike$loglike)
  return(loglike)






}



#' Get the full likelihood for SIR for 3 groups
#'
#' @param par vector of parameters
#' (beta11, ..beta13, ..., beta31, ..., beta33,
#' gamma).  Must be in correct order
#' @param agents_df data frame of agents with columns agent_id,
#'  tI (integer), tR (integer), infector_id
#' @param grouping_var name of grouping variable
#' @param agg_df data frame with cumulative number in each state at each time for each group
#' columns t, group, S, I, R
#' @param return_neg logical indicating whether we should return neg. log like
#' @return loglike for groups
#' @details Ugly version.
loglike_3_groups_internal <- function(par,
                                    agents_df,
                                    agg_df,
                                    return_neg = TRUE){

  agents_df <- agents_df %>%
    dplyr::mutate(tI = ceiling(.data$tI),
                  tR = ceiling(.data$tR),
                  group = group + 1)
  ## The +1 is a hack (above) to get the groupings to match, generally unsafe
  agents_df$infector_group <- get_infector_group(agents_df) 


  ## returns long df with t, pI, pR, group, infector_group
  group_prob_df <- get_prob_of_inf_3groups(par, agg_df) 

  joined_df <- dplyr::left_join(agents_df, group_prob_df,
                                by = c("group", "infector_group")) %>%
    arrange(.data$t)

  trimmed_df <- joined_df %>%
    group_by(.data$agent_id) %>%
    filter(.data$t <= .data$tR)  ## Don't need items after recovery


  agent_loglike <- trimmed_df %>%
    group_by(agent_id) %>%
    summarize(loglike = loglike_agent(.data$t,
                                      .data$tI,
                                      .data$tR,
                                      .data$pI,
                                      .data$pR,
                                      .data$prob_pooled)
    )

  loglike <- sum(agent_loglike$loglike)
  if(return_neg) loglike <- -loglike
  return(loglike)



}


#' Get the corresponding infector group
#'
#' @param agents_df data frame with columns agent_id, group, infector_id
#' @return agents_df with additional column of infector_group
#' @details TODO: need to allow more flexible grouping variable name
get_infector_group <- function(agents_df){
  sub_df <- agents_df %>% dplyr::select(.data$agent_id,
                                 .data$group)
  colnames(sub_df) <- c("agent_id", "infector_group")

  joined_df <- agents_df %>%
    dplyr::left_join(sub_df, 
                     by = c("infector_id" = "agent_id"))

  return(joined_df$infector_group)


}

#' Get probabilities at each time step for each group
#' 
#' @param par (beta11, ..., beta13, ..., beta31, ...beta33, gamma)
#' @param agg_df  data frame with cumulative number in each state at each time for each group
#' columns t, group, X0, X1, X2
#' @return long df with t, pI, pR, group, infector_group
get_prob_of_inf_3groups <- function(par, agg_df){
 # browser()
  agg_df <- agg_df %>%
    dplyr::select(.data$t, dplyr::everything()) %>%
    dplyr::rename(S = X0,
                  I = X1,
                  R = X2)
  t0_df <- agg_df %>%
    dplyr::filter(.data$t == 0) %>%
    dplyr::summarize(S = sum(.data$S),
                     I = sum(.data$I), 
                     R = sum(.data$R))
  #browser()
  N_vec <- t0_df$S + t0_df$I + t0_df$R
  N <- sum(N_vec)
 # N1 <- N_vec[1]
 # N2 <- N_vec[2]
 # N3 <- N_vec[3]
  N1 <- N2 <- N3 <- N
  #browser()
  agg_df <- tidyr::pivot_wider(agg_df %>%
                                 dplyr::mutate(group = 
                                                 as.numeric(as.character(.data$group)) + 
                                                              1),
                               id_cols = t,
                               names_from = group,
                               values_from = c(S, I, R),
                               names_sep = "")
  ## Hard coding this for now
  ## Format is betaij * Ij 
  pI11 <- with(agg_df, par["beta11"] * I1 / N1)
  pI12 <- with(agg_df, par["beta12"] * I2 / N2)
  pI13 <- with(agg_df, par["beta13"] * I3 / N3)
  pI21 <- with(agg_df, par["beta21"]  * I1 / N1)
  pI22 <- with(agg_df, par["beta22"]  * I2 / N2)
  pI23 <- with(agg_df, par["beta23"] * I3 / N3)
  pI31 <- with(agg_df, par["beta31"]  * I1 / N1)
  pI32 <- with(agg_df, par["beta32"]  * I2 / N2)
  pI33 <- with(agg_df, par["beta33"] * I3 / N3)
  probs <- c(pI11, pI12, pI13,
             pI21, pI22, pI23,
             pI31, pI32, pI33)
  prob_pooled <- rowMeans(cbind(pI11, pI12, pI13,
                                pI21, pI22, pI23,
                                pI31, pI32, pI33))
  names(probs) <- NULL
  pR <- par["gamma"]
  names(pR) <- NULL
  df <- data.frame(t = rep(agg_df$t, 9),
                   pI = probs,
                   prob_pooled = rep(prob_pooled, 9),
                   group = rep(c(1, 1, 1,
                                 2, 2, 2,
                                 3, 3, 3), nrow(agg_df)),
                   infector_group = rep(c(1, 2, 3,
                                          1, 2, 3,
                                          1, 2, 3), nrow(agg_df)),
                   pR = pR) 
  ## If
  return(df)
}


#' Calculate individual log like for each agent
#' 
#' @param t vector of time steps starting with 0
#' @param tI time when individual became infectious (integer)
#' @param tR time when individual recovered (integer)
#' @param pI probability of infection from time i to i+1
#' @param pR probability of recovery from time i to i+1
#' @param prob_pooled pooled prob of infection
#' @return log likelihood for agent
#' @details Like = sum(t as S) log(1 - pI) + log(pI) * Indicator(max t <= tI) +
#' sum(t as I) log(1 - pR) + log(pR) * Indicator(max t <= tR)
#' above is if starts in S.  If pI from time before inf to inf is 0
#' we substitute the pooled prob
loglike_agent <- function(t,
              tI,
              tR,
              pI,
              pR,
              prob_pooled){
  ## A little hacky, but protecting against INF
  tI <- tI[1]
  tR <- tR[1]
  max_t <- max(t)
  
 # browser()
  
  ## STARTING OUT INFECTIOUS
  if(tI <= 0 & tR > 0 ){
    if(tR == 1){  ## If you immediately recover, no failed recovs
      times_with_failed_recov <- NA
    } else{
      times_with_failed_recov <- 0:(tR-2)
    }
    time_before_recov <- tR-1
    loglike_vec <- na.omit(log(c(1 - pR[times_with_failed_recov + 1],
                                 pR[time_before_recov + 1][1])))
    if(any(is.infinite(loglike_vec))){
     # browser()
      stop("Infinite loglike, this shouldn't be possible")
    }
    good_inds <- 1:min(max_t , length(loglike_vec))
    return(sum(loglike_vec[good_inds], na.rm = TRUE))
    
    ## STARTING OUT RECOVERED
  } else if(tR <= 0){  ##  Nothing to do here
    return(0)
    
    ## STARTING OUT AS SUSCEPTIBLE
  } else{ 
      if(tI == 1){  ## if you immediately are infected, no failure of inf
        times_with_failed_infection<- NA
      } else {
        times_with_failed_infection <- 0:(tI - 2) # became infected on tI
      }
      time_before_infection <- tI - 1
      prob_before_I <- ifelse(pI[time_before_infection + 1] == 0,
                              prob_pooled[time_before_infection + 1],
                              pI[time_before_infection + 1])
      if(tR == tI + 1){  ## if you immediately recover, no failed recovs.
        times_with_failed_recov <- NA
      } else{
        times_with_failed_recov <- tI:(tR-2)
      }
    #  browser()
      time_before_recov <- tR-1
      ## ADD + 1 because indexing sucks as times starting with 0 but R vector does not
      loglike_vec <- na.omit(log(c(1 - pI[times_with_failed_infection + 1],
                           prob_before_I,
                           1 - pR[times_with_failed_recov + 1],
                           pR[time_before_recov + 1][1])))
      if(any(is.infinite(loglike_vec))) {
       # browser()
        stop("Infinite loglike, this shouldn't be possible")
      }
      ## USE ONLY STUFF IN TIME RANGE
      good_inds <- 1:min(max_t, length(loglike_vec))
      return(sum(loglike_vec[good_inds], na.rm = TRUE))
      
  }
  
  
  
}

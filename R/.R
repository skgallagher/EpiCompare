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
#' init_state, tI, tR, infector_id
#' @param grouping_var name of grouping variable
#' @param agg_df data frame with cumulative number in each state at each time for each group
#' columns t, S1, ..., S3, I1, ..., I3, ..., R1, ...R3
#' @return loglike for groups
#' @details Ugly version.
#'
loglike_3_groups_internal <- function(par,
                                    agents_df,
                                    agg_df){

  agents_df$infector_group <- get_infector_group(agents_df)


  ## returns long df with t, pI, pR, group, infector_group
  group_prob_df <- get_prob_of_inf_3groups(par, agg_df)

  joined_df <- dplyr::left_join(agents_df, group_prob_df,
                                by = c("group", "infector_group"))

  trimmed_df <- joined_df %>%
    group_by(agent_id) %>%
    filter(t <= tR)  ## Don't need items after recovery


  agent_log
  like <- trimmmed_df %>%
    group_by(agent_id) %>%
    summarize(loglike = loglike_agent(.data$t,
                                      .data$pI,
                                      .data$pR)
    )

  loglike <- sum(agent_loglike$loglike)
  return(loglike)



}


#' Get the corresponding infector group
#'
#' @param agents_df data frame with columns agent_id, group, infector_id
#' @return agents_df with additional column of infector_group
#' @details TODO: need to allow more flexible grouping variable name
get_infector_group <- function(agents_df){
  sub_df <- agents_df %>% select(.data$agent_id,
                                 .data$group)
  colnames(sub_df) <- c("agent_id", "infector_group")

  joined_df <- agents_df %>%
    dplyr::left_join(sub_df, by = c("infector_id" = "agent_id"))

  return(joined_df)


}


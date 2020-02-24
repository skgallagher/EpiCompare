## Functions to generate SIR data to visualize


#' Simulate SIR data according to many, independent chain Binomials
#'
#' @param n_sims number of times to run simulation.
#' @param n_time_steps number of total time steps (will use 0 to n_time_steps
#' -1 inclusive).  If size 1 use n_sims for all.  Otherwise should be size K.
#' @param par_mat a K x 2 matrix where entry k1 is the beta value for group k
#' and k2 is the gamma value for group k.
#' @param init_mat K x 3 matrix where entry k1 is the initial S0 for group k,
#' k2 is the initial I0, and k3 is the initial R0.
#' @details If output_format = "array" then the output will be a list of K
#' arrays.  Each array is a n_sims x 3 x n_agents array where entry (i,j,k) is
#' the ith simulation, the jth statistic and the kth agent.  If
#' \code{output_format = "data.frame"} then the output is a data.frame with
#' columns agent_id, init_state, I_max, R_max, sim_num, group.  The size is
#' (n_agents x n_sims) x 6.
#' @details For each simulation \eqn{i} and each group \eqn{k}, agent
#' \eqn{A_{t,j}} (the jth agent at time t) will update according to the
#' following where the states are denoted \eqn{S=0,I=1,R=2}.  The update
#' follows a Bernoulli draw based on the agent's current state.  Specifically,
#' \deqn{A_{t,j}| S_{t-1,k}, I_{t-1,k} \sim \left \{\begin{array}{ll}
#' \textnormal{Bernoulli} \left ( p_{t-1,k}\right ) & \textnormal{ if }
#' A_{t-1,j} = 0 \\ 1 + Bernoulli(\gamma_k) & \textnormal{ if } A_{t-1,j}
#' = 1 \\2 & \textnormal{ otherwise}  \end{array} \right . }
#'
#' The 3 pieces that make up the agents statistic are (init_state (0/1/2), max time
#' susceptible, max time infectious.)  If the agent never became infectious or
#' was infectious at time \eqn{t=0} then \eqn{s_max = n_time_steps -1}.
#' Similarly, if the agent never recovers or is recovered from time 0 on then
#' \eqn{i_max = n_time_steps -1}.  This function is a wrapper for
#' simulate_SIR_agents() to generate many independent simulations with
#' different initial parameters and disease parameters.
#' @export
#' @examples
#' par_mat <- matrix(c(.5, .25,
#'                      .8, .2), byrow = TRUE, ncol = 2)
#' init_mat <- matrix(c(90, 10, 0,
#'                      45, 5, 0), byrow = TRUE, ncol = 3)
#' sims_data <- simulate_SIR_agents_groups(n_sims = 5,
#'                                         n_time_steps = 10,
#'                                         par_mat = par_mat,
#'                                         init_mat = init_mat)
#' dim(sims_data)
simulate_SIR_agents_groups <- function(n_sims,
                                       n_time_steps,
                                       par_mat,
                                       init_mat){
  K <- nrow(par_mat)
  stopifnot(nrow(init_mat) == K)
  sim_data <- vector(mode = "list", length = K)
  for(kk in 1:K){
    beta <- par_mat[kk, 1]
    gamma <- par_mat[kk, 2]
    init_SIR <- init_mat[kk,]
    if(length(n_time_steps) > 1){
      n_time_steps_k <- n_time_steps[kk]
    } else{
      n_time_steps_k <- n_time_steps
    }

    df <- simulate_SIR_agents(n_sims = n_sims,
                                          n_time_steps = n_time_steps_k,
                                          beta = beta, gamma = gamma,
                                          init_SIR = init_SIR,
                                          output_format = "data.frame")

    df$group <- kk  ## add a grouping column
    sim_data[[kk]] <- df
  }
  sim_data <- do.call('rbind', sim_data)
  class(sim_data)
  return(sim_data)

}

#' Simulate SIR data according to a chain Binomial
#'
#' @param n_sims number of times to run simulation
#' @param n_time_steps number of total time steps (will use 0 to n_time_steps
#' -1 inclusive)
#' @param beta infection parameter for SIR chain Binomial.  See details
#' @param gamma recovery paraemter for SIR chain Binomial.  See details
#' @param init_SIR vector of (S0, I0, R0) the number of agents initially in the
#' Susceptible, Infected, and Recovered state, respectively.  The sum of this
#' will be used as the number of agents
#' @param output_format one of either "array" or "data.frame"
#' @return The the output is a data.frame with
#' columns agent_id, init_state, I_max, R_max, sim_num.  The size is (n_agents
#' x n_sims) x 5.
#' @details For each simulation \eqn{i}, agent \eqn{A_{t,k}} (the kth agent at
#' time t) will update according to the following where the states are denoted
#' \eqn{S=0,I=1,R=2}.  The update follows a Bernoulli draw based on the agent's
#'  current state.  Specifically,
#' \deqn{A_{t,k}| S_{t-1}, I_{t-1} \sim \left \{\begin{array}{ll}
#' \textnormal{Bernoulli} \left ( p_{t-1}\right ) & \textnormal{ if }
#' A_{t-1,k} = 0 \\ 1 + Bernoulli(\gamma) & \textnormal{ if } A_{t-1,k} = 1
#' \\2 & \textnormal{ otherwise}  \end{array} \right . }
#'
#' The 3 pieces that make up the agents statistic are (init_state (0/1/2), max time
#' susceptible, max time infectious.)  If the agent never became infectious or
#' was infectious at time \eqn{t=0} then \eqn{s_max = n_time_steps -1}.
#' Similarly, if the agent never recovers or is recovered from time 0 on then
#' \eqn{i_max = n_time_steps -1}.
#'
#' @export
#' @examples
#' sims_data <- simulate_SIR_agents(n_sims = 2, n_time_steps = 5,
#'                                  beta = .5, gamma = .1, init_SIR = c(9,1,0),
#' output_format = "data.frame")
#' dim(sims_data)
simulate_SIR_agents <- function(n_sims,
                                n_time_steps,
                                beta, gamma,
                                init_SIR,
                                output_format = "data.frame"){
  n_agents <- sum(init_SIR)
  sim_data <- array(NA, dim = c(n_sims, 3, n_agents))
  # ^3 is for the agents stat

  ## Fill in initial states
  init_states <- c(rep(0, init_SIR[1]),
                   rep(1, init_SIR[2]),
                   rep(2, init_SIR[3]))
  sim_data[, 1, ] <- rep(init_states, each = n_sims)

  ## Simulate


  for(sim in 1:n_sims){
    SIR_count <- init_SIR
    current_states <- init_states
    for(tt in 0:(n_time_steps -2)){ # Don't update on last known state

      new_states_list <- update_agents(current_states,
                                  SIR_count,
                                  beta, gamma)
      new_states <- new_states_list$states
      new_inf_inds <- state_change_inds(new_states,
                                        current_states,
                                        type = "inf")
      new_rec_inds <- state_change_inds(new_states,
                                        current_states,
                                        type = "rec")
      if(length(new_inf_inds) > 0){
        sim_data[sim, 2, new_inf_inds] <- tt
      }
      if(length(new_rec_inds) > 0){
        sim_data[sim, 3, new_rec_inds] <- tt
      }

      ## Set new states to be next current states
      current_states <- new_states
      SIR_count <- new_states_list$SIR_count
    }

    sim_data[sim, 2, ] <- ifelse(sim_data[sim, 3, ] < sim_data[sim, 2,],
                                 sim_data[sim, 3, ],
                                 sim_data[sim, 2, ])

  }
  class(sim_data) <- c("sims_array", class(sim_data))
 # browser()
 # ensure max_time_S <= max_time_i
  dimnames(sim_data) <- list(sim = 1:n_sims,
                        agents_stat = c("init_state", "max_time_S",
                                        "max_time_I"),
                        agent_id = paste0("id_", 1:n_agents))


  if(output_format == "array"){
    return(sim_data)
  } else if(output_format == "data.frame"){
    sim_data <- fortify_sims(sim_data)
    return(sim_data)
  } else {
    stop("output_format should either be 'array' or 'data.frame'")
  }
}

#' Update agents based on Bernoulli draws
#'
#' @param current_states vector of 0/1/2 of length n_agents.  Gives the current
#' state for each agent
#' @param SIR_count total number of S, I, R currently in each state
#' @param beta infection parameter between 0 and 1
#' @param gamma infection parameter between 0 and 1
#' @return list with
#' \itemize{
#' \item states vector of size n_agents of new states
#' \item SIR_count new total number of S, I, R currently in each state
#' }
update_agents <- function(current_states, SIR_count,
                          beta, gamma){
  n_agents <- sum(SIR_count)
  stopifnot(sum(SIR_count) == length(current_states))
  inf_prob <- SIR_count[2] / n_agents * beta
  current_sus_inds <- which(current_states == 0)
  current_inf_inds <- which(current_states == 1)
  new_states <- current_states
  if(length(current_sus_inds) > 0){
    new_states[current_sus_inds] <- stats::rbinom(n = SIR_count[1],
                                                  size = 1,
                                                  prob = inf_prob)
  }
  if(length(current_inf_inds) > 0){
    new_states[current_inf_inds] <- 1 + stats::rbinom(n = SIR_count[2],
                                                      size = 1,
                                                      prob = gamma)
  }
  # get new count
  SIR_count[1] <- sum(new_states == 0)
  SIR_count[2] <- sum(new_states == 1)
  SIR_count[3] <- n_agents - SIR_count[1] - SIR_count[2]

  return(list(states = new_states, SIR_count = SIR_count))

}


#' Determine which agents changed states
#'
#' @param new_states vector of size n_agents of 0/1/2
#' @param current_states vector of size n_agents of 0/1/2
#' @param type either "inf" for new infectious or "rec" for new recovered
#'
#' @return indices of which agents who were in state type are now in the next
#' state
#'
state_change_inds <- function(new_states,
                  current_states,
                  type = "inf"){
  if(type == "inf"){
    inds <- which(current_states == 0 & new_states == 1)
  } else if(type == "rec"){
    inds <- which(current_states == 1 & new_states == 2)
  } else{
    stop("type should be 'inf' or 'rec'")
  }
  return(inds)
}







## SKG
## May 13, 2020
## A general agent simulator that follows basic CM rules

#' Simulate an agent-based model for given state-to-state transfers
#'
#' @param trans_mat TBD matrix of size KxK where K is the total number of states (including death and excluding birth)
#' @param init_vals vector of length K corresponding to the initial values in each state.  This will be over-ridden by birth_dates and birth_states if those are not NULL.
#' @param par_vals vector of named parameters and their values
#' @param max_T total number of discrete time steps
#' @param n_sims number of simulations to run
#' @param birth_dates vector of size N, the maximum number of agents where each entry is the 'birth' of the agent into the system
#' @param birth_states which state an agent begins when born into the system
#' @return a data frame with the following columns
#' \describe{
#' \item{agent_id}{unique ID of agent that can be linked through simulations}
#' \item{time}{time between 0 and max_T}
#' \item{state}{integer of the state the agent goes to at that time}
#' \item{state_name}{name of state corresponding to the integer}
#' \item{sim}{simulation number}
#' }
#' @param verbose logical to print progress
#' @param out_format Format of the output data frame.  
#' Wide corresponds to one row to each agent and "long" corresponds to one row being a state change.
#' Generally, 'wide' is more readable and slightly easier to use with other EpiCompare functions.  However,
#' With the 'wide' format, there is a problem when agents can enter a state more than once.
#' This will trigger an error.
#' @details A (fairly) generic, simple agent-based model based on multinomial/categorical draws to transfer from state to state.  
#'  Although the function can support a non-constant population,
#'   it does not support random births, they must be pre-specified. 
#'   Random deaths may be supported by adding a compartment for them.
#'  Please see the 'basic-abm' vignette for more details on usage.
#' @export
#' @examples 
#' ## SI example
#' ## In this example, agents start out susceptible and then become infected
#' 
#' trans_mat <- matrix(c("X0 * (1 - X1 * par1/N)", "X0 * X1 * par1 / N",
#' "0", "X1"), byrow = TRUE, nrow = 2)
#' rownames(trans_mat) <- c("S", "I")
#' init_vals <- c(999, 1)
#' par_vals <- c(par1 = .01)
#' max_T <- 100
#' n_sims <- 5
#' 
#' abm <- simulate_agents(trans_mat,
#' init_vals,
#' par_vals,
#' max_T,n_sims,
#' verbose = FALSE)
#' 
#' head(abm)
#' table(abm$I)
#' library(ggplot2)
#' library(dplyr)
#' abm %>% dplyr::group_by(sim) %>%
#' agents_to_aggregate(states = I) %>%
#' ggplot(aes( x= t, y = X1, group = sim, col = factor(sim))) +
#' geom_line()
simulate_agents <- function(trans_mat,
                            init_vals,
                            par_vals,
                            max_T,
                            n_sims,
                            birth_dates = NULL,
                            birth_states = NULL,
                            verbose = TRUE,
                            out_format = "wide"){

    ## Translate the matrix to a function
    state_names <- get_state_names(trans_mat)
    n_states <- nrow(trans_mat)
    n_pars <- length(par_vals)
    D_list_fxns <- trans_mat_to_D_fxn(trans_mat, n_states, n_pars)

    
    ## Set up agent array of simulations x n_agents x time
    agent_array <- initialize_agent_array(init_vals,
                                          max_T,
                                          n_sims,
                                          birth_dates,
                                          birth_states)

    ## Possibly slow now but better capability to make Cpp loops
    ## Loop over simulations then time, updating all agents at once based on previous counts
    if(verbose){
        print("Simulation")
    }
    for(ii in 1:n_sims){
        if(verbose){
            print(sprintf("%d / %d ..", ii, n_sims))
        }
        for(tt in 2:max_T){

            agent_states <- agent_array[ii,, tt-1] # vector of length n_agents
            next_states <- agent_array[ii,, tt] # to check births
            state_counts <- get_previous_counts(agent_states, n_states) # vector of length K
            is_born <- ((agent_states < 0) & (agent_array[ii,, tt] >=0))
                              
            N <- sum(state_counts) # single number
            draw_matrix <- multinomial_updater(state_counts, 
                                               par_vals,
                                               D_list_fxns,
                                               time = tt-1)
            new_states <- draws_to_states(agent_states,
                                          draw_matrix) #vector of length n_agents
            agent_array[ii,, tt] <- ifelse(is_born, next_states, new_states)
        }
    }
    df <- agent_array_to_df(agent_array)
    df$state_name <- factor(df$state, levels = 0:(n_states-1),
                                            labels = state_names)
    

    if(out_format == "wide"){
        
        ## Check for duplicate rows in the long df
        ## indicating someone enters a state more than once in a single simulation
        temp_df <- df %>% dplyr::select(-.data$time) %>% dplyr::distinct()
        if(nrow(temp_df) != nrow(df)){
            stop(paste0("The simulation cannot output a wide format",
                "because the transition matrix allows for ",
                "re-entry into a state.  Either fix ",
                "D or change out_format to `long`"))
        }
        

        
        if(tidyr_new_interface()){
            df <- df %>% dplyr::select(-.data$state) %>%
                tidyr::pivot_wider(id_cols = c(.data$sim, 
                                               .data$agent_id),
                    names_from = .data$state_name,
                                            values_from = .data$time)
        } else {
            df <- df %>% dplyr::select(-.data$state) %>%
                tidyr::spread(.data$state_name, .data$time)
        }
    }
    return(df)
    
}


#' Draw the transfer matrix
#'
#' @param agent_states vector of length agent_states where each has a value of -1, 0, ..., K-1.
#' @param draw_matrix KxK matrix
#' @return vector of length agent_states of new states
draws_to_states <- function(agent_states,
                            draw_matrix){
    next_states <- agent_states
    K <- nrow(draw_matrix)
    for(kk in 0:(K-1)){
        kk_inds <- which(agent_states == kk)
        new_states <- rep(0:(K-1), times = draw_matrix[kk + 1,])
        if(length(new_states) == 1){
           perm_new_states <- new_states 
        } else {
           perm_new_states <- sample(new_states)
        }
        if(length(kk_inds) > 0){
            next_states[kk_inds] <- perm_new_states
        }
        
    }
    return(next_states)

}


#' Count number in each state
#'
#' @param agent_states vector of length agent_states where each has a value of -1, 0, ..., K-1.
#' @param n_states number of total states since we will use zeros
#' @return   We return a vector of length K of counts, the number in state 0 through K-S
get_previous_counts <- function(agent_states, n_states){
    sapply(0:(n_states-1), function(ii) sum(agent_states == ii))
}
                            

#' Initialize array for agent simulations
#'
#' @param init_vals vector of length K corresponding to the initial values in each state.  This will be over-ridden by birth_dates and birth_states if those are not NULL.
#' @param max_T total number of discrete time steps
#' @param n_sims number of simulations to run
#' @param birth_dates vector of size N, the maximum number of agents where each entry is the 'birth' of the agent into the system
#' @param birth_states which state an agent begins when born into the system
#' @return n_sims x n_agents x (max_T + 1) array where entry ijt is the state of agent j in simulation i at time t-1.  The states they can take are integers between -1 (not born yet) and K-1 where 0 to K-1 correspond to different states.
initialize_agent_array <- function(init_vals, max_T,
                       n_sims,
                       birth_dates = NULL,
                       birth_states = NULL){
    names(init_vals) <- NULL

    if(is.null(birth_dates) & !is.null(birth_states)){
        stop("Please provide birth states in addition to birth dates")
    } else if(is.null(birth_dates) & is.null(birth_states)){
        stopifnot(length(birth_dates) == length(birth_states))
        agent_array <- array(-1, dim = c(n_sims, sum(init_vals), max_T + 1))
        agent_array[,,1] <- rep(rep(0:(length(init_vals)-1), times = init_vals),
                                each = n_sims)
    } else {
        n_agents <- length(birth_dates)
        birth_states_rep <- rep(birth_states, times = n_sims)
        sim_inds <- rep(1:n_sims, each = n_agents)
        agent_inds <- rep(1:n_agents, times = n_sims)
        time_inds <-  rep(birth_dates + 1, times = n_sims)
        agent_array <- array(-1, dim = c(n_sims, n_agents, max_T + 1))
        for(ii in 1:length(sim_inds)){
            agent_array[sim_inds[ii], agent_inds[ii],
                        time_inds[ii]] <- birth_states_rep[ii]
        }
    }
    

    return(agent_array)
}


#' Get the state names fro the transition matrix
#'
#' @param trans_mat KxK transition matrix
#' @return names of states
get_state_names <- function(trans_mat){
    if(is.null(rownames(trans_mat))){
        nms <- paste0("X", 0:(nrow(trans_mat)-1))
    } else {
        nms <- rownames(trans_mat)
    }
    return(nms)
}


## SKG
## March 4, 2020
## Converting from SIR simulation array to data frame with columns
##
## agent_id | sim | tI | tR
## if agent is initially infected then tI < 0
## If agent never reaches a state, then tI or tR = NA


#' Convert simulation output from simulate_SIR_agents to data frame format
#'
#' @param arr array of dimension \eqn{\# agents \times \# sims \times 3} where
#'   agent ijk is the ith agent in simulation j, and looking at the kth column
#'   where 1 is the initial state, 1 is the time of infection, and 2 is the time
#'   of recovery
#' @return data.frame of dimension (n_agents x n_sims) x 4 where the columns are
#' \describe{
#' \item{agent_id}{unique ID}
#' \item{sim}{simulation number}
#' \item{tI}{time of infection}
#' \item{tR}{time of recovery}
#' }
sim_arr_to_df <- function(arr){
    array_dim <- dim(arr)
    n_sims <- array_dim[1]
    n_agents <- array_dim[3]
    stopifnot(array_dim[2] == 3)
    dimnames(arr) <- list(sim = 1:n_sims,
                                agents_stat = c("init_state", "tI",
                                                "tR"),
                                agent_id = 1:n_agents)
    df <- as.data.frame.table(arr)


    if (tidyr_new_interface()){
        df_spread <- df %>% tidyr::pivot_wider(names_from = .data$agents_stat,
                                               values_from = .data$Freq) %>%
            dplyr::select(dplyr::one_of("init_state", "tI",
                                        "tR", "sim", "agent_id"))
    } else {
        df_spread <- df %>% tidyr::spread(key = .data$agents_stat,
                                          value = .data$Freq) %>%
            dplyr::select(dplyr::one_of("init_state", "tI",
                                        "tR", "sim", "agent_id"))
    }
    df <- df_spread %>% dplyr::mutate(tI = ifelse(.data$init_state != 0,
                                                  0, .data$tI),
                                      tR = ifelse(.data$init_state == 2,
                                                  0, .data$tR),
                                      sim = factor(.data$sim)) %>%
        dplyr::select(.data$agent_id, .data$sim,
                      .data$tI,
                      .data$tR)

    return(df)

}



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


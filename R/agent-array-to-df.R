## SKG
## May 14, 2020
## Agent array to DF

#' Put agent_array into a data frame format
#'
#' @param agent_array array of agents (n_sims x agent_id x time)
#' @return data frame version of this
agent_array_to_df <- function(agent_array){
    agent_mat <- agent_array_to_mat(agent_array)
    df <- as.data.frame(agent_mat)
    colnames(df) <- c("sim", "agent_id", "time", "state")
    return(df)

}


#' Put agent_array into a matrix format
#'
#' @param agent_array n_sims x n_agents x time array
#' @return matrix with the following columns
#' \describe{
#' \item{sim}{simulation id}
#' \item{agent_id}{agent id}
#' \item{time}{time}
#' \item{state}{number between 0 and K-1}
#' }
#' @details loop through array and make into a long matrix.  Only record when there is a change in state.  Do not record -1.  Made so to eventually replace with a Rcpp function
agent_array_to_mat <- function(agent_array){
    ## initialize matrix
    max_length <- prod(dim(agent_array))
    agent_mat <- matrix(-1, ncol = 4, nrow = max_length)
    dims <- dim(agent_array)
    mat_index <- 1
    for(sim in 1:dims[1]){
        for(ii in 1:dims[2]){
            for(tt in 1:dims[3]){
                if(tt == 1){
                    old_state <- agent_array[sim, ii, 1]
                    if(old_state > -1){
                        agent_mat[mat_index, ] <- c(sim, ii, tt, old_state)
                        mat_index <- mat_index + 1
                    }
                } else {
                    new_state <- agent_array[sim, ii, tt]
                    if((new_state > -1) & (new_state != old_state)){
                        agent_mat[mat_index, ] <- c(sim, ii, tt, new_state)
                    }
                    mat_index <- mat_index + 1
                    old_state <- new_state
                }
                
            }
        }
    }


    return(agent_mat[agent_mat[,1] > -1,])
                       
                       

}
                  

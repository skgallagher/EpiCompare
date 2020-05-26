## SKG
## May 15, 2020
## Getting the actual multinomial udpates



#' Multinomial draws to update states depending on past count
#'
#' @param state_counts vector of length K of counts in each state
#' @param par_vals parameter vector values
#' @param D_list_fxns output from \code{\link{trans_mat_to_D_fxn}}
#' @param time current time, possibly used in the udpate function
#' @return KxK matrix with new number in each state for agents who were in state i
multinomial_updater <- function(state_counts,
                                par_vals,
                                D_list_fxns,
                                time = NA){
    K <- length(state_counts)
    n_pars <- length(par_vals)
    N <- sum(state_counts)
    t <- time
    draws <- matrix(0, nrow = K, ncol = K)
    ## Set up argument list
    arg_list <- c(as.list(state_counts), as.list(par_vals), N, t)
    names(arg_list) <- c(paste0("X", 0:(K-1)),
                         paste0("par", 1:n_pars),
                         "N", "t")
    for(ii in 1:K){
        D_vec <- numeric(K)
        p_vec <- numeric(K)
        if(state_counts[ii] > 0){
            for(jj in 1:K){
                f <- D_list_fxns[[K * (ii-1) + jj]]
                D_vec[jj] <- do.call(f, arg_list)
            }
            D_vec <- ifelse(D_vec < 0, 0, D_vec)
            p_vec <- D_vec / sum(D_vec)
            
            if(any(is.na(p_vec))) stop()
            draws[ii,] <- stats::rmultinom(n = 1,
                                    size = state_counts[ii],
                                    prob = p_vec)
        } else{
            draws[ii,] <- 0 
        }
    }
    return(draws)
     


}

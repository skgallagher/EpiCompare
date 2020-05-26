## SKG
## May 14, 2020
## Take a formula for a transition matrix  and turn it into a function
## LOL good luck

#' Take a character formula transition matrix and turn it into a function
#'
#' @param D_mat a KxK matrix of character transitions.  See details
#' @param K number of states
#' @param n_pars number of parameters
#' @return a list of length \eqn{K^2} where entry K(i-1) + j for i, j <= K is a function.  See details.
#' @details This function dynamically creates a list of functions that turn the
#'  character transition formulas provided by the user in D_mat into \eqn{K^2}
#'   functions which take in arguments X0, ..., X{K-1)}, par1, par2, ..., par{n_pars},
#'  N, and t and output the numeric transition rate of an individual
#'   currently in state \eqn{i} to move to state \eqn{j}.  
trans_mat_to_D_fxn <- function(D_mat, K,
                               n_pars){
    sym_list <- vector(mode = "list", length = K^2)
    my_args <- vector(mode = "list", length = K + n_pars + 2)
    names(my_args) <- c(paste0("X", 0:(K-1)),
                        paste0("par", 1:n_pars),
                        "N", "t")
    D_vec <- as.vector(t(D_mat))
    ## Go across then down
    for(ii in 1:K^2){
  
        sym_list[[ii]] <- rlang::new_function(
                                     args = my_args,
                                     rlang::parse_expr(D_vec[ii]),
                                     rlang::caller_env())
                                     
    }
    return(sym_list)

}

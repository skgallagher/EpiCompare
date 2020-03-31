## SKG
## March 26, 2020
##
## Rewriting enquosure things to have outer functions that accept symbols/null/strings and inner functions which only take strings
## This is an *internal* function only


#' Take EpiModel Data and format it
#'
#' @param data output from external source package.  See details
#' @param states names of states we want aggregate totals of at each time.  This is a \code{string}.
#' @param package_source optional argument to include the package from which the
#'   output is derived from, which helps with the fortify function when outputs
#'   are of generic classes such as list or data.frame
#' @return a data frame with the following columns
#' \describe{
#' \item{t}{time}
#' \item{Xk}{columns X0, ..., X_K. which are numeric}
#' }
#' @details This function converts epimodel data sources and converts
#'   and puts it in a format that can be used by our exploring functions.
fortify_aggregate.epimodel_inner <- function(data,
                                             states,
                                             package_source = NULL){

    states <- unname(tidyselect::vars_select(names(data$epi),
                                             !!states))

    ## Grab the state names if not specified
    if(length(states) == 0){
        states <- get_epimodel_icm_states(data)
    }

    ## Check if all the specified names are there
    if(!all(c(states) %in% names(data$epi))){
        stop("The state names are not all in the names of data$epi")
    } ## Don't have extra states

    ## Actual formatting
    n_sim <- data$control$nsims
    n_states <- length(states)
    state_list <- vector(mode = "list", length = n_states)
    for(ii in 1:n_states){
        nm <- states[ii]
        state_list[[ii]] <- extract_icm_cols(nm, ii-1, data$epi)
    }

    if(("dt" %in% names(data$control)) &&
       (data$control$dt != 1)){
        sprintf("Warning!  dt != 1 so the times",
                "in the output data frame will be",
                "rescaled to evenly spaced, integer values")
    }

    ## Extract time steps
    mat <- data$epi[[1]]
    if("timesteps" %in% names(data$control)){
        t <- rep(1:length(data$control$timesteps), ncol(mat))
    } else{
        t <- rep(1:data$control$nsteps, ncol(mat))
    }


    ## Put it all together
    agg_df <-  state_list %>%
        dplyr::bind_cols() %>%
        dplyr::select(.data$sim, dplyr::starts_with("X")) %>%
        dplyr::mutate(sim = factor(.data$sim))
    agg_df$t <- t
    agg_df <- agg_df %>%
        dplyr::select(.data$t, dplyr::everything())

    return(agg_df)

}

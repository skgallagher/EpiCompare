#' Comments for Shannon:
#' # not sure about how I'm dealing with the start (t_min -1 vs t_min) in example
#' (difference between the below examples. - this may correspond to comment:
#' timeternR::agents_to_aggregate_SIR.data.frame statement "## Ben, I think this
#' is part of the problem...") additionally, this doesn't cost us anything in
#' the filament visualization as the data points are the same.
#'
#' # example code:
#' library(tidyverse)
#'
#' agents <- timeternR::hagelloch_raw
#' states = c("tI", "tR")
#' death <- "tDEAD"
#' min_max_time <- c(0, NA)
#'
#' b <- raw_agents_to_aggregate(agents, states, death, birth = birth)
#' a <- timeternR::hagelloch_raw %>%
#' timeternR::fortify_agents() %>%
#' timeternR::agents_to_aggregate_SIR() %>%
#' as.matrix
#'
#' #look at:
#' b[1:(nrow(b)-2),] - a[2:nrow(a),]
#'
#' timeternR::hagelloch_raw %>% filter(!is.na(tDEAD)) %>% pull(tDEAD) %>%
#' ceiling() %>% sort
#'
#' ### example 2 of code:
#' agents <- timeternR::hagelloch_raw
#' # making babies
#' set.seed(5)
#' babies <- sample(nrow(agents),size = 5)
#' agents$tBIRTH <- NA
#' agents$tBIRTH[babies] <- agents$tI[babies] - 5
#' states = c("tI", "tR")
#' death <- NULL
#' birth <- "tBIRTH"
#' min_max_time <- c(0, NA)
#'
#' b <- raw_agents_to_aggregate(agents, states, death, birth = birth)
#' a <- timeternR::hagelloch_raw %>%
#' timeternR::fortify_agents() %>%
#' timeternR::agents_to_aggregate_SIR() %>%
#' as.matrix
#'
#' b[1:(nrow(b)-2),] - a[2:nrow(a),]
#'
#' agents %>% filter(!is.na(tBIRTH)) %>% pull(tBIRTH) %>% ceiling() %>% sort




#' min_max_time vector check
#'
#' this is an internal function
#'
#' @param min_max_time min_max_time
#'
#' @return error or \code{TRUE} if min_max_time vector meets assumptions
#'
check_min_max_time <- function(min_max_time){
  assertthat::assert_that(length(min_max_time) == 2 & is.vector(min_max_time),
                          msg = "min_max_time should be a vector of length 2")
  assertthat::assert_that(is.na(min_max_time[2]) ||
                            min_max_time[1] < min_max_time[2],
                          msg = paste("min_max_time must have assending values",
                                      "(NA is acceptable for max value)"))

  non_na <- min_max_time[!is.na(min_max_time)]
  assertthat::assert_that(all(ceiling(non_na) == non_na),
                          msg = "min_max_time (non NA values) must be integer")
}

#' checks if states within data frame are ordered as inputted (<=)
#'
#' @details This function returns either 1) TRUE, if the states are ordered, 2)
#' an error is observed (when assert_error = TRUE), or 3) a list of 2 data
#' frames, the first containing information for each individual (their observed
#' ordering and if they broke the provided ordering), and the second containing
#' summarization on the number of individuals in each of the ordering "classes".
#'
#' @param df data frame with individual agent information
#' @param states time entered state, in expected order
#' @param assert_error boolean if we should raise error if ordering assumption
#' is violated.
#'
#' @return depends, see details
#' @export
check_ordered <- function(df, states, assert_error = TRUE){
  df_select <- df[, states]
  K <- length(states)

  if(sum(is.na(df_select[, states])) > 0 & K > 1){

    info_only_int_na <- df_select[, states]

    for (i in (K):2) {
      if (sum(is.na(info_only_int_na[,i-1])) > 0) {
        info_only_int_na[,i-1][
          is.na(info_only_int_na[,i-1])
          ] <- info_only_int_na[,i][is.na(info_only_int_na[,i-1])]
      }
    }

    df_select[, states] <- info_only_int_na
  }

  logic_out <- matrix(FALSE, nrow = nrow(df_select), ncol = ncol(df_select) - 1)
  for (i in 1:(K-1)){
    logic_out[,i] <- df_select[,i] > df_select[,i+1]
  }

  if (assert_error){
    assertthat::assert_that(sum(logic_out) == 0,
                            msg = paste("provided states order isn't correct /",
                                        "met, use",
                                        "'check_ordered' function with",
                                        "'assert_error = FALSE' to get info",
                                        "on which rows made this assumption",
                                        "incorrect"))
  }

  if (sum(logic_out) != 0) {
    ordering <- df[, states] %>% apply(1, function(x){
      paste(states[order(x)], collapse = " <= ")})
    row_logic <- logic_out %>% apply(1, function(x){sum(x) != 0})
    df_out <- data.frame(id = rownames(df[,states]),
                         error = row_logic,
                         ordering = ordering)

    summary_out <- df_out %>% dplyr::group_by(.data$ordering, .data$error) %>%
      dplyr::summarize(count = dplyr::n())

    return(list(ordering_df = df_out, summary_df = summary_out))
  } else {
    return(TRUE)
  }


}




#' add class and time placeholder with zero individuals to data frame
#'
#' this is an internal function (so t_min, t_max, K should be correct)
#'
#' @param df data frame with \code{state}, \code{t} and \code{count} columns
#' which provide information on the number of agents that became said
#' \code{state} at time \code{t}.
#' @param t_min minimum integer time
#' @param t_max maximum integer time
#' @param K number of stages - 1 (e.g.: SIR has K = 2)
#'
#' @return expanded data frame
expanding_info <- function(df, t_min, t_max, K){
  if (tidyr_new_interface()){
    ## just running
    # ```
    #  %>% tidyr::pivot_wider(names_from = .data$t, values_from = .data$count,
    #                        values_fill = list(count = 0))
    # ```
    # would loose the desire to have all values of t - even if no changes for
    # that t. specifically it would have a `spec`` like:
    # ```
    # spec <- new %>% tidyr::build_wider_spec(names_from = .data$t,
    #                                 values_from = .data$count)
    # ```
    # so instead - we make our own `spec`:

    my_spec <- tibble::tibble(.name = as.character(t_min:t_max),
                              .value = "count",
                              t = as.integer(t_min:t_max))

    hold <- df %>% dplyr::mutate(t = as.integer(t)) %>%
      tidyr::pivot_wider_spec(my_spec, values_fill = list(count = 0)) %>%
      t()

    names(hold) <- hold[1,]
    hold <- hold %>% as.data.frame()
    hold <- hold[-1,] %>% tibble::rownames_to_column() %>%
      dplyr::rename(t = "rowname") %>%
      tidyr::pivot_longer(cols = dplyr::one_of(paste0("V",1:K)),
                          names_to = "state",
                          values_to = "count") %>%
      dplyr::mutate(state = as.numeric(factor(.data$state,
                                              levels = paste0("V",1:K),
                                              labels = 1:K)))
  } else {
    hold <- df %>% dplyr::mutate(t = factor(.data$t, levels = t_min:t_max)) %>%
      tidyr::spread(key = .data$t,
                    value = .data$count,
                    drop = FALSE, fill = 0) %>%
      t()

    names(hold) <- hold[1,]
    hold <- hold %>% as.data.frame()
    hold <- hold[-1,] %>% tibble::rownames_to_column() %>%
      dplyr::rename(t = "rowname") %>%
      tidyr::gather(dplyr::one_of(paste0("V",1:K)),
                    key = "state",
                    value = "count") %>%
      dplyr::mutate(state = as.numeric(factor(.data$state,
                                              levels = paste0("V",1:K),
                                              labels = 1:K)))

  }

  return(hold)
}

#' generalized function to convert raw agent based data to aggregate data
#'
#' @param agents data frame with individual agent information
#' @param states time entered state. Do not include column for original state.
#' These need to be ordered, for example: for an SIR model, with columns
#' "\code{tI}" and "\code{tR}" expressing the time the individual became
#' infected and recovered (respectively), we want
#' "\code{states = c("tI", "tR")}".
#' @param death string for column with death time information (default
#' \code{NULL})
#' @param birth string for column with birth time information (default
#' \code{NULL})
#' @param min_max_time vector (length 2) of minimum and maximum integer time,
#' the second value can be \code{NA} - and if so, we estimate maximum time from
#' the data.
#'
#' @return dataset with aggregated information, We label classes
#' "\code{class_\{i\}}" for i in \code{0:(length(states))}.
#' @export
#'
#' @examples
#' agents <- timeternR::hagelloch_raw
#' states <- c("tI", "tR")
#' death <- "tDEAD"
#' birth <- NULL
#' min_max_time <- c(0, NA)
#'
#' b <- raw_agents_to_aggregate(agents, states, death, birth = birth)
#' a <- timeternR::hagelloch_raw %>%
#' timeternR::fortify_agents() %>%
#' timeternR::agents_to_aggregate_SIR() %>%
#' as.matrix
#'
#' #look at:
#' b[1:(nrow(b)-2),] - a[2:nrow(a),]
#'
#' # keeping in mind:
#' timeternR::hagelloch_raw %>% dplyr::filter(!is.na(.data$tDEAD)) %>%
#' dplyr::pull(.data$tDEAD) %>% ceiling() %>% sort
#'
#' ### example 2 of code:
#' agents <- timeternR::hagelloch_raw
#' # making babies
#' set.seed(5)
#' babies <- sample(nrow(agents),size = 5)
#' agents$tBIRTH <- NA
#' agents$tBIRTH[babies] <- agents$tI[babies] - 5
#' states <- c("tI", "tR")
#' death <- NULL
#' birth <- "tBIRTH"
#' min_max_time <- c(0, NA)
#'
#' b <- raw_agents_to_aggregate(agents, states, death, birth = birth)
#' a <- timeternR::hagelloch_raw %>%
#' timeternR::fortify_agents() %>%
#' timeternR::agents_to_aggregate_SIR() %>%
#' as.matrix
#'
#' b[1:(nrow(b)-2),] - a[2:nrow(a),]
#'
#' agents %>% dplyr::filter(!is.na(.data$tBIRTH)) %>%
#' dplyr::pull(.data$tBIRTH) %>% ceiling() %>% sort
raw_agents_to_aggregate <- function(agents,
                                states,
                                death = NULL,
                                birth = NULL,
                                min_max_time = c(0, NA)
                                ){
  # parameter check -----------------------------------
  check_min_max_time(min_max_time)
  t_min <- min_max_time[1]
  t_max <- min_max_time[2]

  N <- nrow(agents)
  K <- length(states)

  # ordering of states check --------------------------
  check_ordered(agents, states)

  info_only <- agents[,c(states, death, birth)] %>% sapply(as.numeric) %>%
    as.data.frame


  # beyond death and before birth ----------------------

  # if change after death, then convert to NA
  if (!is.null(death)) {
    change_of_state_after_death <- which(info_only[,states] > info_only[,death])
    info_only[,states][change_of_state_after_death] <- NA

    # new change:
    only_states <- states
    states <- c(states, death)
    K <- K + 1
  } else {
    only_states <- states
  }

  # if changed before birth, convert to time of birth
  if(!is.null(birth)){
    non_na_birth <- !is.na(info_only[,birth])
    change_of_state_before_birth <-
      apply(info_only[non_na_birth,], 1, function(x){x[states] < x[birth]}) %>%
      t()

    change_of_state_before_birth[is.na(change_of_state_before_birth)] <- FALSE

    inner <- info_only[non_na_birth,states]
    inner[change_of_state_before_birth] <-
      matrix(rep(info_only[non_na_birth,birth], 3), ncol = 3)[
        change_of_state_before_birth
        ]

    info_only[non_na_birth,states] <- inner

    N_born <- sum(info_only[birth] >= t_min, na.rm = TRUE)
  } else {
    N_born <- 0
  }

  # beyond time max and time min -----------------------
  # if t_state is strictly less than t_min, convert to t_min
  info_only[,states][info_only[,states] < t_min] <- t_min

  # if t_state is strictly more than t_max, convert to NA
  if(!is.na(t_max)){
    info_only[,states][info_only[,states] > t_max] <- NA
  } else {
    t_max <- max(ceiling(info_only[,states]), na.rm = TRUE)
  }

  # Switch to integer values ------------------------------------------

  info_only_int <- info_only %>% ceiling()

  # dealing with NAs --------------------------------------------------
  # in all but the last class - NA just means the person jumped that class
  if(sum(is.na(info_only_int[, only_states])) > 0 & K > 1){

    info_only_int_na <- info_only_int[, only_states]

    for (i in (K):2) {
      if (sum(is.na(info_only_int_na[,i-1])) > 0) {
        info_only_int_na[,i-1][is.na(info_only_int_na[,i-1])] <- info_only_int_na[,i][is.na(info_only_int_na[,i-1])]
      }
    }

    info_only_int[, only_states] <- info_only_int_na
  }

  # getting new counts of state membership (not 0) --------------------
  if (tidyr_new_interface()){
    info_only_into_long <- info_only_int[,states] %>%
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "state",
                          values_to = "t") %>%
      dplyr::mutate(state = as.numeric(factor(.data$state, labels = 1:K,
                                              levels = states))) %>%
      dplyr::arrange(.data$state)
  } else {
    info_only_into_long <- info_only_int[,states] %>%
      tidyr::gather(dplyr::everything(),
                    key = "state",
                    value = "t") %>%
      dplyr::mutate(state = as.numeric(factor(.data$state, labels = 1:K,
                                              levels = states)))
  }

  one_plus_state_count <- info_only_into_long %>%
    dplyr::group_by(.data$state, .data$t) %>%
    dplyr::summarize(count = dplyr::n())

  one_plus_state_count <- expanding_info(one_plus_state_count, t_min = t_min,
                                         t_max = t_max, K = K)

  # getting new counts of state membership for 0 class --------------------

  init_0_cum <- N - N_born
  zero_state_count <- data.frame(state = rep(0, t_max - t_min + 1),
                                 t = t_min:t_max,
                                 count = c(init_0_cum, rep(0, t_max-t_min)))

  # applying births to state member counts ----------------------

  if(!is.null(birth)){
    if(sum(!is.na(info_only_int[, birth]))){
      births_col <- info_only_int %>%
        dplyr::select(dplyr::one_of(birth)) %>%
        tidyr::drop_na()

      births_counts <- births_col %>%
        dplyr::group_by(!!dplyr::sym(birth)) %>%
        dplyr::summarize(count = dplyr::n())

      # just add to 0 class (and other classes will then steal from it?)
      zero_state_count <- zero_state_count %>%
        dplyr::left_join(births_counts, by = c("t" = birth)) %>%
        dplyr::mutate(count = .data$count.x +
                        ifelse(is.na(.data$count.y), 0, .data$count.y)) %>%
        dplyr::select(-.data$count.x, -.data$count.y)


    }
  }

  # population counts from cummulative counts -------------------
  state_count <- one_plus_state_count %>% as.data.frame %>%
    rbind(.data$.,zero_state_count)

  if (tidyr_new_interface()){
    cum_state_count <- state_count %>%
      dplyr::mutate(state = factor(.data$state, levels = 0:K,
                                   labels = paste0("X",0:K))) %>%
      dplyr::group_by(.data$state) %>%
      tidyr::nest() %>%
      dplyr::mutate(new_data =
                      purrr::map(.data$data,
                                 function(df){df %>%
                                     dplyr::mutate(cum_count = cumsum(.data$count))})) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(.data$new_data) %>% dplyr::select(-.data$count) %>%
      tidyr::pivot_wider(id_cols = "t",values_fill = list("cum_count" = 0),
                         names_from = "state", values_from = "cum_count") %>%
      dplyr::filter(.data$t >= t_min) %>%
      dplyr::arrange(.data$t) %>%
      dplyr::select(t, dplyr::one_of(paste0("X", 0:K)))
  } else {
    cum_state_count <- state_count %>%
      dplyr::mutate(state = factor(.data$state, levels = 0:K,
                                   labels = paste0("X",0:K))) %>%
      dplyr::group_by(.data$state) %>%
      tidyr::nest() %>% # nest_legacy
      dplyr::mutate(new_data =
                      purrr::map(.data$data,
                                 function(df){df %>%
                                     dplyr::mutate(cum_count = cumsum(.data$count))})) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest() %>%  # unnest_legacy
      dplyr::select(-.data$count) %>%
      dplyr::group_by(.data$t) %>%
      tidyr::spread(key = "state",value = "cum_count", fill = 0) %>%
      dplyr::filter(.data$t >= t_min) %>%
      dplyr::select(.data$t, dplyr::one_of(paste0("X", 0:K)))
    }



  final_state_count <- cum_state_count %>%
    dplyr::select(dplyr::one_of(c("t",paste0("X",0:K))))
  for(i in 0:(K-1)){
    final_state_count[, paste0("X",i)] <- cum_state_count[, paste0("X",i)] -
      cum_state_count[, paste0("X",i+1)]
  }

  final_state_count <- final_state_count %>% dplyr::ungroup() %>%
    dplyr::mutate(t = as.numeric(.data$t)) %>%
    dplyr::arrange(.data$t)


  if(!is.null(death)){
    final_state_count <- final_state_count %>%
      dplyr::select(-dplyr::one_of(paste0("X", K)))
  }

  class(final_state_count) <- c("aggregate", class(final_state_count))

  return(final_state_count)
}




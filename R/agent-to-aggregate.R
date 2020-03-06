# Comments for Shannon:
# # not sure about how I'm dealing with the start (t_min -1 vs t_min) in example
# (difference between the below examples. - this may correspond to comment:
# timeternR::agents_to_aggregate_SIR.data.frame statement "## Ben, I think this
# is part of the problem...") additionally, this doesn't cost us anything in
# the filament visualization as the data points are the same.
#
# # example code:
# agents <- timeternR::hagelloch_raw
# states <- c("tI", "tR")
# death <- "tDEAD"
# birth <- NULL
# min_max_time <- c(0, NA)
# b <- agents_to_aggregate(agents, states, death, birth = birth)
# a <- timeternR::hagelloch_raw %>%
# timeternR::fortify_agents() %>%
# timeternR::agents_to_aggregate_SIR() %>%
# as.matrix
#
# #look at:
# b[1:(nrow(b)-2),] - a[2:nrow(a),]
#
# # keeping in mind:
# timeternR::hagelloch_raw %>% dplyr::filter(!is.na(.data$tDEAD)) %>%
# dplyr::pull(.data$tDEAD) %>% ceiling() %>% sort
#
# ### example 2 of code:
# agents <- timeternR::hagelloch_raw
# # making babies
# set.seed(5)
# babies <- sample(nrow(agents),size = 5)
# agents$tBIRTH <- NA
# agents$tBIRTH[babies] <- agents$tI[babies] - 5
# states <- c("tI", "tR")
# death <- NULL
# birth <- "tBIRTH"
# min_max_time <- c(0, NA)
#
# b <- agents_to_aggregate(agents, states, death, birth = birth)
# a <- timeternR::hagelloch_raw %>%
# timeternR::fortify_agents() %>%
# timeternR::agents_to_aggregate_SIR() %>%
# as.matrix
#
# b[1:(nrow(b)-2),] - a[2:nrow(a),]
#
# agents %>% dplyr::filter(!is.na(.data$tBIRTH)) %>%
# dplyr::pull(.data$tBIRTH) %>% ceiling() %>% sort



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
#' This function assesses whether agents's time they enter each state is
#' correctly order. The assumption is that these would be ordered in increasing
#' values, with the allowance of \code{NA} values not effecting this decision.
#'
#' If this important assumption is violated this function either raises an error
#' or provides the user with information on what when wrong - to allow the user
#' to how to corect the error (see \code{assert_error} to change between these
#' states).
#'
#' @param df data frame with individual agent information (n x p)
#' @param states time entered state, in expected order
#' @param assert_error boolean if we should raise error if ordering assumption
#' is violated.
#'
#' @return This function returns either
#' \itemize{
#'   \item TRUE, if the states are ordered,
#'   \item an error is observed (when assert_error = TRUE), \strong{or}
#'   \item a list of 2 data frames. The first, \code{ordering_df}, an (n x 3)
#'   data.frame, contains information per agent on if they violated the
#'   assumption ("error" column), and the ordering of their states ("ordering"
#'   column). The second, \code{summary_df} (k x 3) data frame contains
#'   information on the number of unique ordering ("ordering"), if they caused
#'   the assumption to be violated ("error") and the number of agents that had
#'   the ordering ("count").
#' }
#'
#' @examples
#' df_not_ordered <- data.frame(group1 = 1:5,
#'                              group2 = c(2:5,1))
#' output <- check_ordered(df_not_ordered, c("group1", "group2"),
#'                         assert_error = FALSE)
#' output
#'
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
                            msg = paste("provided 'states' values are not",
                                        "ordered (some agents have times they",
                                        "enter each state in a different order",
                                        "than the 'state' parameter suggests).",
                                        "Use the 'check_ord ered' function with",
                                        "'assert_error = FALSE' to get info",
                                        "on which rows made the assumption",
                                        "that states are ordered correctly",
                                        "violated."))
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


#' expanding aggregate data to desirable format
#'
#' This internal function specifically takes a data frame the contains rows that
#' provide the an integer time, a state and the number of agents that entered
#' that state at that time (this data frame only contains counts > 0).
#'
#' This function then outputs a "\code{pivot_longer}" data frame, that has 1 row
#' per integer time period between (and including) t_min and t_max. Even if 0
#' agents change their state at that given time. The columns (beyond \code{t})
#' relate to each state.
#'
#' @param df data frame with \code{state}, \code{t} and \code{count} columns
#'   which provide information on the number of agents that became said
#'   \code{state} at time \code{t}.
#' @param t_min minimum integer time
#' @param t_max maximum integer time
#' @param K (number of stages - 1) (e.g.: SIR has K = 2)
#'
#' @return expanded data frame: a data frame with columns \code{t} and a set of
#'   the unique \code{states} as columns. For each row, it contains the number
#'   of agents to change to said state at specified time.
expanding_info <- function(df, t_min, t_max, K){
  if (tidyr_new_interface()){
    ## The below code uses tidyr's "spec" approach (which allows for specs to
    # be provided to alter the outcome of "pivot").
    #
    # In this example, in tidyr <= 8.3 we'd do:
    # ```
    # df %>% dplyr::mutate(t = factor(.data$t, levels = t_min:t_max)) %>%
    #   tidyr::spread(key = .data$t,
    #                 value = .data$count,
    #                 drop = FALSE, fill = 0)
    # ```
    # which expands the t value sto get all integers between t_min:t_max in the
    # t column.
    #
    # In the new tidyr (1.0.0) pivot_wider doesn't include a "drop = FALSE"
    # parameter, and if we do the following:
    # ```
    # df %>% tidyr::pivot_wider(names_from = .data$t, values_from = .data$count,
    #                        values_fill = list(count = 0))
    # ```
    # we would loose the desire to have all values of t - even if no changes for
    # that t (aka the original data frame never saw certain values of t).
    #
    # Trying to understanding tidyr's new "spec" approach, I would encourage you
    # to run the following code.
    # ```
    # spec <- new %>% tidyr::build_wider_spec(names_from = .data$t,
    #                                 values_from = .data$count)
    # ```
    # this is the associated spec with the pivot_wider that dropped the `t`s we
    # wanted to keep. We really want a spec as defined below in `my_spec`:
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


#' generalized method to convert raw agent based data to aggregate data
#'
#' This function converts data on an agent-based level (1 row = 1 agent)
#' relative when an agent is in each state and aggregates it, so that the user
#' can know how many agents are in each state at a given time point (integer
#' based). This function can take standard \code{data.frame}s and
#' \code{grouped_df} \code{data.frame}s (from \pkg{dplyr}). For the
#' later, this function aggregates within grouping parameters and also provides
#' the columns associated with the grouping.
#'
#' @param agents data frame style object (currently either of class
#'   \code{data.frame} or \code{grouped_df})
#' @param states time entered state. Do not include column for original state.
#'   These need to be ordered, for example: for an SIR model, with columns
#'   "\code{tI}" and "\code{tR}" expressing the time the individual became
#'   infected and recovered (respectively), we want "\code{states = c("tI",
#'   "tR")}". \strong{See details for more information.}
#' @param death string for column with death time information (default
#'   \code{NULL})
#' @param birth string for column with birth time information (default
#'   \code{NULL})
#' @param min_max_time vector (length 2) of minimum and maximum integer time,
#'   the second value can be \code{NA} - and if so, we estimate maximum time
#'   from the data.
#'
#' @return dataset with aggregated information, we label classes \code{X\{i\}}
#'   for i in \code{0:(length(states))}. Potentially calculated per group of a
#'   \code{grouped_df} (and retains grouping columns).
#'
#' @details
#'
#' This function converts data on an agent-based level (1 row = 1 agent)
#' relative when an agent is in each state and aggregates it, so that the user
#' can know how many agents are in each state at a given time point (integer
#' based). This function can take standard \code{data.frame}s and
#' \code{grouped_df} \code{data.frame}s (from \pkg{dplyr}). For the
#' later, this function aggregates within grouping parameters and also provides
#' the columns associated with the grouping.
#'
#' \strong{D.1. What each column should have (NAs, orderings, births & deaths,...)}
#'
#' The parameters \code{state}, \code{death}, \code{birth}, and
#' \code{min_max_time} provide the user with the flexibility to capture any
#' potential structure related to agent's progression to through the epidemic
#' (and life).
#'
#' As mentioned in the \code{states} parameter details, we expect a set of
#' column names \code{X1}, \code{X2}, ..., \code{XK} that contain information on
#' when an individual enters each state. Also mentioned in the parameter details
#' is that the function assumes that each agent is in the initial state
#' \code{X0} until \code{X1} (except if \code{min_max_time[1] >= X1},
#' which means the agent starts out at state \code{X1}).
#'
#' This function expects transition in an \strong{ordered} fashion, i.e.
#' \code{X(I+1) >= X(I)}, but does allow agents to jump states. This can either
#' be recorded with a value at the jumped state the same as the next non-jumped
#' state or an \code{NA} (and the authors of this package believe this is a
#' cleaner approach - and matches expectation in \code{birth} and \code{death}).
#'
#' Specifically, \code{birth} and \code{death} can contain \code{NA} values,
#' which the function interprets as an individual not being born (or dying
#' respectively) in the given time interval.
#'
#' The time interval (defined by \code{min_max_time}) can be moved, which
#' abstractly just shifts the rows (or time points) the user gets at the end.
#'
#' \strong{D.2. Changing time points}
#'
#' Beyond defining the time interval with \code{min_max_time}, if a user wishes
#' to have more minute (smaller) time steps than integers, we recommend they
#' just multiple all values by \eqn{1/s} where \eqn{s} is the length of the
#' desired time steps. A transformation of the output's \code{t} column by
#' \eqn{s} would get the time back to the standard time.
#'
#' @export
#'
#' @examples
#'
#' ###
#' # for standard data.frame objects (agents_to_aggregate.grouped_df)
#' ###
#' library(dplyr)
#' agents <- timeternR::hagelloch_raw
#' # making babies
#' set.seed(5)
#' babies <- sample(nrow(agents),size = 5)
#' agents$tBIRTH <- NA
#' agents$tBIRTH[babies] <- agents$tI[babies] - 5
#'
#' aggregate_b <- agents_to_aggregate(agents, states = c(tI, tR),
#'                                    death = NULL, birth = tBIRTH)
#'
#' # looking at when babies where born:
#' agents %>% dplyr::filter(!is.na(.data$tBIRTH)) %>%
#'   dplyr::pull(.data$tBIRTH) %>% ceiling() %>% sort
#' # vs:
#' data.frame(counts = 1:nrow(aggregate_b),
#'            num_people = aggregate_b %>% select(-t) %>% apply(1, sum))
#'
#'
#' # including death
#' aggregate_d <- agents_to_aggregate(agents, states = c(tI, tR),
#'                                    death = tDEAD, birth = NULL)
#'
#' # looking at when people died:
#' agents %>% dplyr::filter(!is.na(.data$tDEAD)) %>%
#'   dplyr::pull(.data$tDEAD) %>% ceiling() %>% sort
#' # vs:
#' data.frame(counts = 1:nrow(aggregate_d),
#'            num_people = aggregate_d %>% select(-t) %>% apply(1, sum))
#'
#' ###
#' # for grouped_df objects (agents_to_aggregate.grouped_df)
#' ###
#'
#'
#' max_time <- 100
#' agents_g <- hagelloch_raw %>%
#'   filter(SEX %in% c("female", "male")) %>% group_by(SEX)
#' sir_group <- agents_to_aggregate(agents_g, states = c(tI, tR),
#'                                  min_max_time = c(0, max_time))
#' agents <- agents_g %>%
#'   filter(SEX == "female") %>% ungroup()
#' sir_group1 <- agents_to_aggregate(agents, states = c(tI, tR),
#'                                  min_max_time = c(0, max_time))
#' sir_group_1 <- sir_group %>% filter(SEX == "female")
#' assertthat::are_equal(sir_group1,
#'                       sir_group_1 %>% ungroup %>% select(t, X0, X1, X2))
agents_to_aggregate <- function(agents,
                                states,
                                death = NULL,
                                birth = NULL,
                                min_max_time = c(0, NA)
){
  UseMethod("agents_to_aggregate")
}



#' generalized function to convert raw agent based data to aggregate data
#'
#' This function converts data on an agent-based level (1 row = 1 agent)
#' relative when an agent is in each state and aggregates it, so that the user
#' can know how many agents are in each state at a given time point (integer
#' based).
#'
#' @details note that all parameters related to name columns can also be in a
#'   string format. More details can be found in \code{agent_to_aggregate}'s
#'   documentation.
#'
#' @param agents data frame with individual agent information
#' @param states Name-variable pairs of the form \code{states = c(col1, col2)},
#'   that describe which columns contain the time one entered the state. Do not
#'   include column for original state. These need to be ordered, for example:
#'   for an SIR model, with columns "\code{tI}" and "\code{tR}" expressing the
#'   time the individual became infected and recovered (respectively), we want
#'   "\code{states = c(tI, tR)}".
#' @param death string for column with death time information (default
#'   \code{NULL})
#' @param birth string for column with birth time information (default
#'   \code{NULL})
#' @param min_max_time vector (length 2) of minimum and maximum integer time,
#'   the second value can be \code{NA} - and if so, we estimate maximum time
#'   from the data.
#'
#' @return dataset with aggregated information, We label classes "\code{X\{i\}}"
#'   for i in \code{0:(length(states))}.
#' @export
#'
#' @examples
#' library(dplyr)
#' agents <- timeternR::hagelloch_raw
#' # making babies
#' set.seed(5)
#' babies <- sample(nrow(agents),size = 5)
#' agents$tBIRTH <- NA
#' agents$tBIRTH[babies] <- agents$tI[babies] - 5
#'
#' aggregate_b <- agents_to_aggregate(agents, states = c(tI, tR),
#'                                    death = NULL, birth = tBIRTH)
#'
#' # looking at when babies where born:
#' agents %>% dplyr::filter(!is.na(.data$tBIRTH)) %>%
#'   dplyr::pull(.data$tBIRTH) %>% ceiling() %>% sort
#' # vs:
#' data.frame(counts = 1:nrow(aggregate_b),
#'            num_people = aggregate_b %>% select(-t) %>% apply(1, sum))
#'
#'
#' # including death
#' aggregate_d <- agents_to_aggregate(agents, states = c(tI, tR),
#'                                    death = tDEAD, birth = NULL)
#'
#' # looking at when people died:
#' agents %>% dplyr::filter(!is.na(.data$tDEAD)) %>%
#'   dplyr::pull(.data$tDEAD) %>% ceiling() %>% sort
#' # vs:
#' data.frame(counts = 1:nrow(aggregate_d),
#'            num_people = aggregate_d %>% select(-t) %>% apply(1, sum))
agents_to_aggregate.data.frame <- function(agents,
                                           states,
                                           death = NULL,
                                           birth = NULL,
                                           min_max_time = c(0, NA)
                                           ){
  # columns states - converting to strings ------------
  # this code mirrors tidyr::nest's code

  # states ---------
  state_cols <- dplyr::enquos(states)
  if (any(rlang::names2(state_cols) == "")) {
    state_col_names <- unname(tidyselect::vars_select(dplyr::tbl_vars(agents),
                                                !!!state_cols))
    state_cols_expr <- dplyr::expr(c(!!!dplyr::syms(state_col_names)))
    states <- state_col_names
  }
  # death ----------
  death_col <- dplyr::enquo(death)
  death_col2 <- dplyr::enquos(death)

  if (length(death_col2) !=  1){
    stop("death should be a single column or NULL")
  }

  if (all(rlang::names2(death_col) == "")){
    death_col_name <- unname(tidyselect::vars_select(dplyr::tbl_vars(agents),
                                                      !!death_col))
    death_cols_expr <- dplyr::expr(c(!!dplyr::syms(death_col_name)))

    if (length(death_col_name) != 0){
      death <- death_col_name
    }
  }

  # birth -------------
  birth_col <- dplyr::enquo(birth)
  birth_col2 <- dplyr::enquos(birth)
  if (length(birth_col2) != 1){
    stop("birth should be a single column or NULL")
  }

  if (all(rlang::names2(birth_col) == "")){
    birth_col_name <- unname(tidyselect::vars_select(dplyr::tbl_vars(agents),
                                                     !!birth_col))
    birth_cols_expr <- dplyr::expr(c(!!dplyr::syms(birth_col_name)))
    if (length(birth_col_name) != 0){
      birth <- birth_col_name
    }
  }


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





#' generalized function to convert raw agent based data to aggregate data
#' for grouped_data (preforms per group)
#'
#' This function converts data on an agent-based level (1 row = 1 agent)
#' relative when an agent is in each state and aggregates it, so that the user
#' can know how many agents are in each state at a given time point (integer
#' based). This function takes \code{grouped_df} \code{data.frame}s (from
#' \pkg{dplyr}) and aggregates within grouping parameters and also
#' provides the columns associated with the grouping.
#'
#' @details note that all parameters related to name columns can also be in a
#'   string format. More details can be found in \code{agent_to_aggregate}'s
#'   documentation.
#'
#' @param agents grouped data.frame with individual agent information
#' @param states Name-variable pairs of the form
#' \code{states = c(col1, col2, col3)}, that describe which columns contain the
#' time one entered the state. Do not include column for original state. These
#' need to be ordered, for example: for an SIR model, with columns
#' "\code{tI}" and "\code{tR}" expressing the time the individual became
#' infected and recovered (respectively), we want "\code{states = c(tI, tR)}".
#' @param death string for column with death time information (default
#' \code{NULL})
#' @param birth string for column with birth time information (default
#' \code{NULL})
#' @param min_max_time vector (length 2) of minimum and maximum integer time,
#' the second value can be \code{NA} - and if so, we estimate maximum time from
#' the data.
#'
#' @return grouped dataset with aggregated information per group, We label
#' classes "\code{X\{i\}}" for i in \code{0:(length(states))}.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' max_time <- 100
#' agents_g <- hagelloch_raw %>%
#'   filter(SEX %in% c("female", "male")) %>% group_by(SEX)
#' sir_group <- agents_to_aggregate(agents_g, states = c(tI, tR),
#'                                  min_max_time = c(0, max_time))
#' agents <- agents_g %>%
#'   filter(SEX == "female") %>% ungroup()
#' sir_group1 <- agents_to_aggregate(agents, states = c(tI, tR),
#'                                  min_max_time = c(0, max_time))
#' sir_group_1 <- sir_group %>% filter(SEX == "female")
#' assertthat::are_equal(sir_group1,
#'                       sir_group_1 %>% ungroup %>% select(t, X0, X1, X2))
agents_to_aggregate.grouped_df <- function(agents,
                                           states,
                                           death = NULL,
                                           birth = NULL,
                                           min_max_time = c(0, NA)
                                           ){
  check_min_max_time(min_max_time)
  t_min <- min_max_time[1]
  t_max <- min_max_time[2]

  if (is.null(t_max)) {
    t_max <- max(ceiling(agents[,states]), na.rm = TRUE)
  }

  min_max_time_all <- c(t_min, t_max)

  if (tidyr_new_interface()){
    out <- agents %>%
      tidyr::nest() %>%
      dplyr::mutate(update = purrr::map(.data$data, agents_to_aggregate,
                                        states = !!!dplyr::enquos(states),
                                        death = !!dplyr::enquo(death),
                                        birth = !!dplyr::enquo(birth),
                                        min_max_time = min_max_time_all)) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(cols = c(.data$update)) # only change
  } else {
    # old
    out <- agents %>% tidyr::nest() %>%
      dplyr::mutate(update = purrr::map(.data$data, agents_to_aggregate,
                                        states = !!!dplyr::enquos(states),
                                        death = !!dplyr::enquo(death),
                                        birth = !!dplyr::enquo(birth),
                                        min_max_time = min_max_time_all)) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(.drop = FALSE)
  }

  return(out)
}

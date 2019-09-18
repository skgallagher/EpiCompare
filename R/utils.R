#' fortify agent data frame with columns when individual stops being suspectable
#' and stops being infected (as well as initial state).
#'
#' @param raw_df data frame, agent based data frame
#' @param time_col length 2 string vector, column names recording when
#' individual is infected and when they enter the recovery stage
#' @param max_time int, maximum time for infection process
#'
#' @return \code{fortified_df} data frame, the \code{raw_df} plus three
#' additional columns:
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was suspectable (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#'
#'
#' @export
#'
#' @examples
#' fortify_df <- fortify_agents(timeternR::hagelloch_raw,
#'                              time_col = c("tI","tR"),
#'                              max_time = 90)
#' assertthat::are_equal(fortify_df[,(ncol(fortify_df) - 2):ncol(fortify_df)],
#'                       timeternR::hagelloch_agents)
fortify_agents <- function(raw_df, time_col = c("tI","tR"),
                           max_time = floor(max(raw_df[,time_col])) + 1){
  assertthat::assert_that(inherits(time_col, "character") &&
                          length(time_col) == 2 &&
                          all(time_col %in% names(raw_df)),
                          msg = paste("time_col should be a string vector of",
                                      "length 2 that has column names relative",
                                      "to raw_df."))
  N <- nrow(raw_df)

  # initial state (was the individual the original one infected?)
  A0 <- rep(0, N)
  inf_ind <- intersect(which.min(raw_df[,time_col[1]]),
                       which(raw_df[,time_col[1]] < 0))
  A0[inf_ind] <- 1

  ## round I and R time - going to use floor
  SMax <- floor(raw_df[,time_col[1]]) + 1
  SMax <- ifelse(SMax > max_time-1, max_time-1, SMax)
  IMax <- floor(raw_df[,time_col[2]]) + 1
  IMax <- ifelse(IMax > max_time-1, max_time-1, IMax)
  U <- data.frame(init_state = factor(A0),
                  max_time_S = SMax,
                  max_time_I = IMax)

  fortified_df <- cbind(raw_df, U)

  return(fortified_df)
}


#' Convert agent information to SIR format
#'
#' @param U data frame, with the following format
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was suspectable (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#' @param max_time integer, max length of outbreak (default NULL)
#' @param ind integer vector which columns match up with the columns described
#' above (default NULL)
#'
#' @return \code{sir_out} data frame, with columns
#' \describe{
#'   \item{t}{time since outbreak}
#'   \item{S}{Number of individuals suspectable}
#'   \item{I}{Number of individuals infected}
#'   \item{R}{Number of individuals in recovery}
#' }
#' @export
#'
#'
#' @examples
#' sir_out <- UtoX_SIR(timeternR::hagelloch_agents)
#' assertthat::are_equal(sir_out, timeternR::hagelloch_sir)
UtoX_SIR <- function(U, max_time = NULL, ind = NULL){
  if (!is.null(ind)){
    names(U)[ind] <- c("init_state", "max_time_S", "max_time_I")
  }

  N <- nrow(U)
  if (is.null(max_time)) {
    max_time <- max(U$max_time_I)
  }
  start_infected <- sum(U$init_state == 1)

  if (tidyr_new_interface()){
    new <- U %>%
      dplyr::mutate(start_time_I = .data$max_time_S + 1,
                    start_time_R = .data$max_time_I + 1) %>%
      dplyr::select(.data$start_time_I, .data$start_time_R) %>%
      dplyr::rename(I = "start_time_I", R = "start_time_R") %>%
      tidyr::pivot_longer(c(.data$I, .data$R),
                          names_to = "key", values_to = "t") %>%
      dplyr::group_by(.data$key, .data$t) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      dplyr::mutate(t = factor(.data$t, levels = 0:max_time))

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

    my_spec <- tibble::tibble(.name = as.character(0:max_time),
                              .value = "count",
                              t = factor(0:max_time))

    new <- new %>%
      tidyr::pivot_wider_spec(my_spec, values_fill = list(count = 0))
  } else {
    new <- U %>%
      dplyr::mutate(start_time_I = .data$max_time_S + 1,
                    start_time_R = .data$max_time_I + 1) %>%
      dplyr::select(.data$start_time_I, .data$start_time_R) %>%
      dplyr::rename(I = "start_time_I", R = "start_time_R") %>%
      tidyr::gather(key = "key", value = "t", .data$I, .data$R) %>%
      dplyr::group_by(.data$key, .data$t) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      dplyr::mutate(t = factor(.data$t, levels = 0:max_time)) %>%
      tidyr::spread(key = "t", value = "count",
                    drop = FALSE, fill = 0)
  }
  t_new <- new[,colnames(new) %in% 0:max_time] %>% t %>% data.frame() %>%
    tibble::rownames_to_column(var = "t")

  t_new <- t_new %>% dplyr::rename(I = "X1", R = "X2")

  sir_out <- t_new %>% dplyr::mutate_at(c("I", "R"), cumsum) %>%
    dplyr::mutate(I = .data$I - .data$R,
           S = N - .data$I - .data$R
           ) %>%
    dplyr::select(.data$t, .data$S, .data$I, .data$R) %>%
    dplyr::mutate(t = as.numeric(.data$t))

  # correction for initial individuals infected
  if (start_infected > 0){
    sir_out[1, ] <- c(0, N - start_infected, start_infected, 0)
  }
  # removing rownames
  rownames(sir_out) <- NULL

  return(sir_out)
}

#' UtoX_SIR for grouped data frames
#' @param U_g grouped data frame, with the following format
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was susceptible (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#' @param max_time integer, max length of outbreak (default NULL), shared across
#' all groups
#'
#' @return \code{sir_out} data frame, with columns
#' \describe{
#'   \item{grouping variable name(s)}{column/columns of grouping variable(s)}
#'   \item{t}{time since outbreak}
#'   \item{S}{Number of individuals susceptible}
#'   \item{I}{Number of individuals infected}
#'   \item{R}{Number of individuals in recovery}
#' }
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#' max_time <- 100
#' U_g <- hagelloch_raw %>% fortify_agents() %>%
#'   filter(SEX %in% c("female", "male")) %>% group_by(SEX)
#' sir_group <- UtoX_SIR_group(U_g, max_time)
#' U <- U_g %>%
#'   filter(SEX == "female") %>% ungroup()
#' sir_group1 <- UtoX_SIR(U, max_time)
#' sir_group_1 <- sir_group %>% filter(SEX == "female")
#' assertthat::are_equal(sir_group1,
#'                       sir_group_1 %>% select(t, S, I, R) %>% data.frame)
UtoX_SIR_group <- function(U_g, max_time = NULL){
  if (is.null(max_time)) max_time <- max(U_g$max_time_I)

  if (tidyr_new_interface()){
    sir_out <- U_g %>% tidyr::nest() %>%
      dplyr::mutate(update = purrr::map(.data$data, UtoX_SIR,
                                        max_time = max_time)) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(cols = c(.data$update)) # only change
  } else {
    # old
    sir_out <- U_g %>% tidyr::nest() %>%
      dplyr::mutate(update = purrr::map(.data$data, UtoX_SIR,
                                        max_time = max_time)) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(.drop = FALSE)
  }

  return(sir_out)
}

#' logic to check if tidyverse (and tidyr specifically is up to version 1.0)
#'
#' @return logical value (boolean)
tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}



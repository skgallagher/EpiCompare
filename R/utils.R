#' fortity agent data frame with columns when individual stops being suspectable
#' and stops being infected (as well as initial state).
#'
#' @param raw_df data frame, agent based data frame
#' @param time_col length 2 string vector, column names recording when
#' individual is infected and when they enter the recovery stage
#' @param T int, maximum time for infection process
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
#'                              T = 90)
#' assertthat::are_equal(fortify_df[,(ncol(fortify_df) - 2):ncol(fortify_df)],
#'                       as.data.frame(t(timeternR::hagelloch_agents)))
fortify_agents <- function(raw_df, time_col = c("tI","tR"),
                           T = ceiling(max(raw_df[,time_col])) + 5){
  assertthat::assert_that(inherits(time_col, "character") &&
                          length(time_col) == 2 &&
                          all(time_col %in% names(raw_df)),
                          msg = paste("time_col should be a string vector of",
                                      "length 2 that has column names relative",
                                      "to raw_df."))
  N <- nrow(raw_df)

  # initial state (was the individual the original one infected?)
  A0 <- rep(0, N)
  inf_ind <- which.min(raw_df[,time_col[1]])
  A0[inf_ind] <- 1

  ## round I and R time - going to use floor
  SMax <- floor(raw_df[,time_col[1]]) + 1
  SMax <- ifelse(SMax > T-1, T-1, SMax)
  IMax <- floor(raw_df[,time_col[2]]) + 1
  IMax <- ifelse(IMax > T-1, T-1, IMax)
  U <- data.frame(init_state = A0,
                  max_time_S = SMax,
                  max_time_I = IMax)

  fortified_df <- cbind(raw_df, U)

  return(fortified_df)
}



# X <- UtoX_SIR(U, T = T) # may need to add catalyst:::


#' Convert agent information to SIR format
#'
#' @param U data frame, with the following format
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was suspectable (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#' @param T integer, max length of outbreak (default NULL)
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
#' sir_out <- UtoX_SIR(as.data.frame(t(timeternR::hagelloch_agents)))
#' assertthat::are_equal(sir_out, timeternR::hagelloch_sir)
UtoX_SIR <- function(U, T = NULL){

  N <- nrow(U)
  if (is.null(T)) T <- max(U$max_time_I) + 1
  start_infected <- sum(U$init_state)

  new <- U %>%
    dplyr::mutate(start_time_I = max_time_S + 1,
                  start_time_R = max_time_I + 1) %>%
    dplyr::select(start_time_I, start_time_R) %>%
    dplyr::rename(I = "start_time_I", R = "start_time_R") %>%
    tidyr::gather(key = "key", value = "t", I, R) %>%
    dplyr::group_by(key, t, add = TRUE) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(t = factor(t, levels = 0:T)) %>%
    tidyr::spread(key = "t", value = "count",
                  drop = FALSE, fill = 0)

  t_new <- new[,colnames(new) %in% 0:T] %>% t %>% data.frame() %>%
    tibble::rownames_to_column(var = "t")

  # if (inherits(U, "grouped_df")) { # not actually expected - code wouldn't work
  #   t_new_inner <- t_new
  #   names(t_new_inner)[names(t_new_inner) != "t"] <-
  #     new[,!(colnames(new) %in% 0:T)] %>% sapply(as.character) %>%
  #     data.frame(check.names = F) %>%
  #     tidyr::unite("combined", everything(), sep = "_%my_split%_") %>%
  #     dplyr::pull(combined)
  #
  #   t_new_inner <- t_new_inner %>% dplyr::mutate(t = factor(t, levels = 0:T))
  #   t_new <- t_new_inner %>% tidyr::gather(key = "grouping", value = "value", -t) %>%
  #     tidyr::separate(grouping, sep = "_%my_split%_",
  #                     into = colnames(new)[!(colnames(new) %in% 0:T)]) %>%
  #     tidyr::spread(key = "key", value = "value") %>%
  #     dplyr::group_by(.dots = sapply(group_vars(U), function(x) paste0("`",x ,"`")))
  # } else {
    t_new <- t_new %>% dplyr::rename(I = "X1", R = "X2")
  #}

  sir_out <- t_new %>% dplyr::mutate_at(c("I", "R"), cumsum) %>%
    dplyr::mutate(I = I - R,
           S = N - I - R
           ) %>%
    dplyr::select(t, S, I, R) %>%
    dplyr::mutate(t = as.numeric(t))

  # correction for initial individuals infected
  sir_out[1, ] <- c(0, N - start_infected, start_infected, 0)

  return(sir_out)
}

#' UtoX_SIR for grouped data frames
#' @param U_g grouped data frame, with the following format
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was suspectable (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#' @param T integer, max length of outbreak (default NULL)
#'
#' @return \code{sir_out} data frame, with columns
#' \describe{
#'   \item{grouping variable name(s)}{column/columns of grouping variable(s)}
#'   \item{t}{time since outbreak}
#'   \item{S}{Number of individuals suspectable}
#'   \item{I}{Number of individuals infected}
#'   \item{R}{Number of individuals in recovery}
#' }
#' @export
#' @examples
#' T <- 100
#' U_g <- hagelloch_raw %>% fortify_agents() %>% group_by(AGE2 = as.numeric(cut(AGE,3)))
#' sir_group <- UtoX_SIR_group(U_g, T)
#' U <- U_g %>%
#'   filter(AGE2 == 1) %>% ungroup()
#' sir_group1 <- UtoX_SIR(U, T)
#' sir_group_1 <- sir_group %>% filter(AGE2 == 1)
#' assertthat::are_equal(sir_group1,
#'                       sir_group_1 %>% select(t, S, I, R) %>% data.frame)
UtoX_SIR_group <- function(U_g, T = NULL){
  N <- nrow(U)
  if (is.null(T)) T <- max(U$max_time_I) + 1

  sir_out <- U_g %>% nest() %>%
    mutate(update = purrr::map(data,UtoX_SIR, T = T)) %>%
    select(-data) %>%
    unnest(.drop = FALSE)

  return(sir_out)
}

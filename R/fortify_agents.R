
#' Fortify agent data frame with columns when individual stops being susceptible
#' and stops being infected (as well as initial state).
#'
#' @param data data frame, agent based data frame, class ("individuals_df")
#' @param time_col length 2 string vector, column names recording when
#' individual is infected and when they enter the recovery stage
#' @param max_time int, maximum time for infection process
#' @return \code{fortified_df} data frame, the \code{raw_df} plus three
#' additional columns:
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was susceptible (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#'
#'
#' @export
#'
#' @examples
#' fortify_df <- fortify_agents(timeternR::hagelloch_raw,
#'                              time_col = c("tI","tR"),
#'                              max_time = 95)
#' assertthat::are_equal(fortify_df[,c("init_state",
#' "max_time_S", "max_time_I")],
#'                       timeternR::hagelloch_agents)
fortify_agents <- function(data, time_col = c("tI","tR"),
                           max_time = floor(
                               max(raw_df[,time_col], na.rm = TRUE)) + 1){
    raw_df <- data

  assertthat::assert_that(inherits(time_col, "character") &&
                          length(time_col) == 2 &&
                          all(time_col %in% names(raw_df)),
                          msg = paste("time_col should be a string vector of",
                                      "length 2 that has column names relative",
                                      "to raw_df."))
  N <- nrow(raw_df)

  # initial state (was the individual the original one infected?)
  A0 <- rep(0, N)
  initial_inf <- intersect(which(raw_df[,time_col[1]] < 0),
                           which(raw_df[,time_col[2]] >= 0))
  initial_rec <- intersect(which(raw_df[,time_col[1]] < 0),
                           which(raw_df[,time_col[2]] < 0))
  A0[initial_inf] <- 1
  A0[initial_rec] <- 2



  ## round I and R time - going to use floor
  SMax <- ceiling(raw_df[,time_col[1]])
  SMax <- ifelse(SMax > max_time-1, max_time-1, SMax)
  IMax <- ceiling(raw_df[,time_col[2]])
  IMax <- ifelse(IMax > max_time-1, max_time-1, IMax)
  agents <- data.frame(init_state = factor(A0),
                  max_time_S = SMax,
                  max_time_I = IMax)
  # dealing with initially infected
  agents[union(initial_inf, initial_rec),"max_time_S"] <- NA
  agents[initial_rec,"max_time_I"] <- NA


  inner_na_agents <- is.na(agents[,c("max_time_S", "max_time_I")])

  if (sum(inner_na_agents) > 0){
    # check NAs are logical
    assertthat::assert_that(all(inner_na_agents[-c(initial_inf, initial_rec),1] <=
                                  inner_na_agents[-c(initial_inf, initial_rec),2]),
                            msg = paste("Please manually correct the fact that",
                                        "an individual has a NA for the time",
                                        "they reached the Infected stage, but",
                                        "a time for when they reached the",
                                        "Recovered stage."))


    ### standard clean up of NAs
    # update agents
    agents[agents$init_state == 0,
      c("max_time_S", "max_time_I")][inner_na_agents[agents$init_state == 0,]] <- max_time
    agents[agents$init_state == 1,
      "max_time_I"][inner_na_agents[agents$init_state == 1,2]] <- max_time

  }


  fortified_df <- cbind(raw_df, agents)
  class(fortified_df) <- c("agents_df", class(fortified_df))

  return(fortified_df)
}


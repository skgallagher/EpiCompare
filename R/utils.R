#' fortify agent data frame with columns when individual stops being susceptible
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
#'                              max_time = 90)
#' assertthat::are_equal(fortify_df[,(ncol(fortify_df) - 2):ncol(fortify_df)],
#'                       timeternR::hagelloch_agents)
fortify_agents <- function(raw_df, time_col = c("tI","tR"),
                           max_time = floor(
                             max(raw_df[,time_col], na.rm = TRUE)) + 1){
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
  U <- data.frame(init_state = factor(A0),
                  max_time_S = SMax,
                  max_time_I = IMax)
  # dealing with initially infected
  U[union(initial_inf, initial_rec),"max_time_S"] <- NA
  U[initial_rec,"max_time_I"] <- NA


  inner_na_U <- is.na(U[,c("max_time_S", "max_time_I")])

  if (sum(inner_na_U) > 0){
    # check NAs are logical
    assertthat::assert_that(all(inner_na_U[-c(initial_inf, initial_rec),1] <=
                                  inner_na_U[-c(initial_inf, initial_rec),2]),
                            msg = paste("Please manually correct the fact that",
                                        "an individual has a NA for the time",
                                        "they reached the Infected stage, but",
                                        "a time for when they reached the",
                                        "Recovered stage."))


    ### standard clean up of NAs
    # update U
    U[U$init_state == 0,
      c("max_time_S", "max_time_I")][inner_na_U[U$init_state == 0,]] <- max_time
    U[U$init_state == 1,
      "max_time_I"][inner_na_U[U$init_state == 1,2]] <- max_time

  }


  fortified_df <- cbind(raw_df, U)

  return(fortified_df)
}


#' Convert agent information to SIR format
#'
#' @param U data frame, with the following format
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was susceptible (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#' @param max_time integer, max length of outbreak (default NULL)
#' @param ind integer vector which columns match up with the columns described
#' above (default NULL)
#'
#' @return \code{sir_out} data frame, with columns
#' \describe{
#'   \item{t}{time since outbreak}
#'   \item{S}{Number of individuals susceptible}
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
    max_time <- max(c(U$max_time_I, U$max_time_S), na.rm = TRUE)
  }

  start_infected <- sum(U$init_state == 1)
  start_recovered <- sum(U$init_state == 2)
  start_susceptible <- sum(U$init_state == 0)

  assertthat::assert_that(all(U$init_state %in% 0:2),
                          msg = "initial statement must be either 0, 1, or 2.")

  inner_na_U <- is.na(U[,c("max_time_S", "max_time_I")])

  if (sum(inner_na_U) > 0){
    # check NAs are logical
    assertthat::assert_that(all(inner_na_U[U$init_state == 0,1] <=
                                  inner_na_U[U$init_state == 0,2]),
                            msg = paste("Please manually correct the fact that",
                                        "an individual that started as",
                                        "susceptible (inital_state = 0) has a",
                                        "NA for the maximum time they were",
                                        "susceptible, but a maximum time when",
                                        "they were infected."))

    assertthat::assert_that(all(inner_na_U[U$init_state == 1,1]),
                            msg = paste("Please manually correct the fact that",
                                        "an individual that started as",
                                        "infected (inital_state = 1) should",
                                        "have NA for the maximum time time",
                                        "they were susceptible."))

    assertthat::assert_that(all(is.na(inner_na_U[U$init_state == 2,1:2])),
                            msg = paste("Please manually correct the fact that",
                                        "an individual that started as",
                                        "recovered (inital_state = 2) should",
                                        "have NA for the maximum time time",
                                        "they were susceptible and infected"))

    ### standard clean up of NAs
    # update U
    U[U$init_state == 0,
      c("max_time_S", "max_time_I")][inner_na_U[U$init_state == 0,]] <- max_time
    if (tibble::is_tibble(U)){
      U[U$init_state == 1,
        c("max_time_I")][inner_na_U[U$init_state == 1,2],] <- max_time
    } else{
      U[U$init_state == 1,
        c("max_time_I")][inner_na_U[U$init_state == 1,2]] <- max_time
    }

    U[,c("max_time_S", "max_time_I")][is.na(U[,c("max_time_S", "max_time_I")])] <- -1
  }


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

  # if (start_infected > 0){  ## Ben, I think this is part of the problem.  Works for Hagelloch data but not for cases with more than one infected at start
  #  sir_out[1, ] <- c(0, N - start_infected, start_infected, 0)
  #
  # }
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
  if (is.null(max_time)) max_time <- max(c(U_g$max_time_I, U_g$max_time_S), na.rm = TRUE)


  if (tidyr_new_interface()){
    sir_out <- U_g %>%
      tidyr::nest() %>%
      dplyr::mutate(update = purrr::map(.data$data, UtoX_SIR,
                                        max_time = max_time)) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(cols = c(.data$update)) # only change
  } else {
    # old
    sir_out <- U_g %>% tidyr::nest_legacy() %>%
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


#' Convert SEIR to XYZ coordinates fixed in a tetrahedron
#'
#' @param data data frame with the following columns
#' \describe{
#' \item{time}{time step}
#' \item{S}{Number of people in S}
#' \item{E}{Number of people in E}
#' \item{I}{Number of people in I}
#' \item{R}{Number of people in R}
#' }
#' @param var_order vector of column names corresponding to the different axes of the tetrahedron-based coordinate system:  (t, l, r, f) which stands for top, left, right, front.
#' @return original data frame along with columns x, y, and z
#' @export
#' @examples
#' seir <- data.frame(t = 0:3,
#' S = c(90, 80, 70, 60),
#' E = c(0, 10, 10, 10),
#' I = c(10, 10, 10, 10),
#' R = c(0, 0, 10, 20))
#' seir_xyz <- SEIR_to_XYZ(seir)
#' head(seir_xyz)
SEIR_to_XYZ <- function(data,
                      var_order=c("S", "E", "I", "R")){

  new_df <- data %>% dplyr::rename(time = "t",
                                   t = var_order[1],
                                   l = var_order[2],
                                   r = var_order[3],
                                   f = var_order[4]) %>%
    dplyr::mutate(N = .data$t + .data$l + .data$r + .data$f) %>%
    dplyr::mutate(x = (.data$r + 1 - .data$l ) / 2 / .data$N,
                  y = (sqrt(3)/2 * .data$t + sqrt(3)/6 * .data$f) / .data$N,
                  z = sqrt(6) / 3 * .data$f / .data$N) %>%
    dplyr::select(-.data$N) %>%
    dplyr::rename(t = "time")
  return(new_df)

}

#' Convert SEIR to XYZ coordinates fixed in a tetrahedron
#'
#' @param data data frame with the following columns
#' \describe{
#' \item{t}{time step}
#' \item{S}{Number of people in S}
#' \item{E}{Number of people in E}
#' \item{I}{Number of people in I}
#' \item{R}{Number of people in R}
#' }
#' @param ternary_vars named vector of the three variables to use as the sides of the ternary plot
#' @param group_var name of the variable to use as the color/feature/grouping vector
#' @return data frame with the transformed variables SEIR -> s, i, r, group variables
#' @export
#' @examples
#' seir <- data.frame(t = 0:3,
#' S = c(90, 80, 70, 60),
#' E = c(0, 10, 10, 10),
#' I = c(10, 10, 10, 10),
#' R = c(0, 0, 10, 20))
#' seir_xyz <- SEIR_to_XYZ(seir)
#' head(seir_xyz)
SEIR_to_SIR_E <- function(data,
                          ternary_vars = c("S", "I", "R"),
                          group_var = "E"){
  new_df <- data %>% dplyr::rename(S = ternary_vars[1],
                                   I = ternary_vars[2],
                                   R = ternary_vars[3],
                                   group = group_var[1]) %>%
    dplyr::mutate(N = .data$S + .data$I + .data$R + .data$group,
                  n = .data$S + .data$I + .data$R) %>%
    dplyr::mutate(S = .data$S / .data$n, I = .data$I / .data$n, R = .data$R / .data$n,
                  group = .data$group / .data$N) %>%
    dplyr::select(-c(.data$N, .data$n))
  return(new_df)
}

#' Updates approved layers of ggtern
#'
#' @param stat_names named vector of new stats.
#' @param geom_names named vector of new geoms
#'
#' @return NULL
#'
#' @examples
#' .newstat = c(sir_raw       = "StatSirRaw",
#'              sir_fortified = "StatSirFortified")
#' .newgeom = NULL
#' update_approved_layers(stat_name = .newstat, geom_name = .newgeom)
update_approved_layers <- function(stat_names = NULL, geom_names = NULL){
  approvestatupdate <- c(ggtern:::.approvedstat,
                         stat_names)
  assignInNamespace(".approvedstat", approvestatupdate,
                    pos = "package:ggtern")

  approvestatupdate <- c(ggtern:::.approvedgeom,
                         stat_names)
  assignInNamespace(".approvedstat", approvestatupdate,
                    pos = "package:ggtern")

}

# List of new stats
.newstat = c(sir_raw       = "StatSirRaw",
             sir_fortified = "StatSirFortified")
.newgeom = NULL
update_approved_layers(stat_name = .newstat, geom_name = .newgeom)


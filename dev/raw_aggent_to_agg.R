agents <- timeternR::hagelloch_raw

# making babies
set.seed(5)
babies <- sample(nrow(agents),size = 5)
agents$tBirth <- NA
agents$tBirth[babies] <- agents$tI[babies] - 5

states = c("tI", "tR")
death <- "tDEAD"
birth <- NULL
min_max_time <- c(0, NA)
init_state

#' internal min_max_time vector check
#'
#' @param x min_max_time
#'
#' @return
#' @export
#'
#' @examples
check_min_max_time <- function(x){
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

# count_new <- function(df, group_by_value = sym("value")){
#   df %>% group_by({{group_by_value}}) %>%
#     summarize(count = n())
# }

expanding_info <- function(df, t_min, t_max, K){
  min_t <- t_min - 1
  my_spec <- tibble::tibble(.name = as.character(min_t:t_max),
                            .value = "count",
                            t = min_t:t_max)

  hold <- df %>%
    tidyr::pivot_wider_spec(my_spec, values_fill = list(count = 0)) %>%
    t()
  names(hold) <- hold[1,]
  hold <- hold[-1,] %>% as.data.frame() %>% tibble::rownames_to_column() %>%
    dplyr::rename(t = "rowname") %>%
    tidyr::pivot_longer(cols = one_of(paste0("V",1:K)),
                 names_to = "state",
                 values_to = "count") %>%
    dplyr::mutate(state = as.numeric(factor(state, levels = paste0("V",1:K),
                                     labels = 1:K)))
  return(hold)
}

#' Title
#'
#' @param agents
#' @param states time entered state. Do not include column for original state.
#' These need to be ordered, for example: for an SIR model, with columns "tI"
#' and "tR" expressing the time the individual became infected and recovered
#' (respectively), we want "states = c("tI", "tR").
#' @param death
#' @param birth
#' @param min_max_time
#'
#' @return will relabel classes "class_i" for i in 0:(length(states))
#' @export
#'
#' @examples
#'
#' TODO:
#' #also - not sure about how I'm dealing with the start (t_min -1 vs t_min) in example
#' # example code:
#' b <- final_state_count %>% as.matrix
#' a <- timeternR::hagelloch_raw %>%
#' timeternR::fortify_agents() %>%
#' timeternR::agents_to_aggregate_SIR() %>%
#' as.matrix
#'
#' #look at:
#' b[1:(nrow(b)-2),] - a[2:nrow(a),]
#'
#' timeternR::hagelloch_raw %>% filter(!is.na(tDEAD)) %>% pull(tDEAD) %>% ceiling() %>% sort
raw_agents_to_aggregate <- function(agents,
                                states,
                                death = NULL,
                                birth = NULL,
                                min_max_time = c(0, NA)
                                ){
  # parameter check
  check_min_max_time(min_max_time)
  t_min <- min_max_time[1]
  t_max <- min_max_time[2]

  N <- nrow(agents)
  K <- length(states)



  # TODO:
  # 1. check that ordering is followed
  # 2. insert function to create new classes and columns if not ordered
  #

  info_only <- agents[,c(states, death, birth)] %>% sapply(as.numeric) %>%
    as.data.frame


  # dealing with death and birth
  # if change after death, then convert to NA
  if(!is.null(death)){
    change_of_state_after_death <- which(info_only[,states] > info_only[,death])
    info_only[,states][change_of_state_after_death] <- NA
    }

  # if changed before birth, convert to time of birth
  if(!is.null(birth)){
    change_of_state_before_birth <-
      apply(info_only, 2, function(x){x < info_only[,birth]})
    info_only[,states][change_of_state_before_birth] <-
      matrix(rep(info_only[,birth], 3), ncol = 3)[change_of_state_before_birth]

    N_born <- sum(info_only[births] > t_min)
  } else {
    N_born <- 0
  }

  # if t_state is strictly less than t_min, convert to t_min -1 (not sure needed)
  info_only[,states][info_only[,states] < t_min] <- t_min

  # if t_state is strictly more than t_max, convert to NA
  if(!is.na(t_max)){
    info_only[,states][info_only[,states] > t_max] <- NA
  }

  # Switch to integer values ------------------------------------------

  info_only_int <- info_only %>% ceiling()

  if (is.na(t_max)){
    t_max <- max(info_only_int[,states])
  }

  # getting new counts of state membership (not 0) --------------------
  # tidyr 1.0




  info_only_into_long <- info_only_int[,c(states)] %>%
    tidyr::pivot_longer(cols = everything(),
                 names_to = "state",
                 values_to = "t") %>%
    dplyr::mutate(state = as.numeric(factor(state, labels = 1:K, levels = states)))

  one_plus_state_count <- info_only_into_long %>%
    dplyr::group_by(state, t) %>%
    dplyr::summarize(count = n())

  one_plus_state_count <- expanding_info(one_plus_state_count, t_min = t_min,
                                         t_max = t_max, K = K)

  init_counts <- one_plus_state_count %>% dplyr::filter(t <= t_min) %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(value = sum(count)) ## do I need this?

  init_0 <- N - N_born - sum(init_counts$value)  ## do I need this?
  init_0_cum <- N - N_born
  init_counts <- init_counts %>%
    rbind(data.frame(state = 0, value = init_0), .)

  zero_state_count <- data.frame(state = rep(0, t_max - t_min + 2),
                                 t = (t_min - 1):t_max,
                                 count = c(0, init_0_cum, rep(0, t_max-t_min)))

  # dealing with births and deaths directly -----------------------
  if(!is.null(birth)){
    if(sum(!is.na(info_only_int[, birth]))){
      births_col <- info_only_int %>%
        tibble::as_tibble %>% dplyr::select(one_of(birth)) %>%
        tidyr::drop_na()

      births_counts <- births_col %>%
        dplyr::group_by(!!sym(birth)) %>%
        dplyr::summarize(count = n())

      # just add to 0 class (and other classes will then steal from it?)
      zero_state_count <- zero_state_count %>%
        dplyr::left_join(births_counts, by = c("t" = birth)) %>%
        dplyr::mutate(count = count.x + ifelse(is.na(count.y), 0, count.y)) %>%
        dplyr::select(-count.x, -count.y)


    }
  }
  if(!is.null(death)){
    if(sum(!is.na(info_only_int[, death]))){
      info_dead <- info_only_int[!is.na(info_only_int[,death]),]
      info_dead_states <- info_dead[, states]
      info_dead_col <- info_dead[, death]

      states_per <- info_dead[, states] %>% tibble::rownames_to_column() %>%
        pivot_longer(cols = one_of(states),
                     names_to = "state",
                     values_to = "t", values_drop_na = FALSE) %>%
        mutate(state = factor(state, levels = states,
                              labels = 1:K))

      death_per <- info_dead %>% tibble::rownames_to_column() %>%
        select(one_of(c(death, "rowname")))

      states_per <- states_per %>% left_join(death_per, by = c("rowname"))

      states_per$logic <- states_per$t <= as.vector(t(states_per[,death]))

      states_per <- states_per %>% filter(logic) %>%
        mutate(state = as.numeric(state)) %>%
        group_by(state, !!sym(death)) %>%
        dplyr::summarize(count = n())

      death_per_zero <- death_per %>% group_by(!!sym(death)) %>%
        dplyr::summarize(count = n())


      # death_counts <- apply(info_dead, 1,
      #       function(x_vec) {
      #         c(0, (1:K)[unlist(x_vec[states]) <= unlist(x_vec[death])])
      #         }) %>% as.data.frame %>% tibble::rownames_to_column() %>%
      #   dplyr::rename(state = ".", t = "rowname") %>%
      #   dplyr::group_by(state, t) %>%
      #   dplyr::summarize(count = n())



    one_plus_state_count <- one_plus_state_count %>%
      mutate(state = as.numeric(state),
             t = as.numeric(t)) %>%
      dplyr::left_join(states_per, by = c("state", "t" = death)) %>%
      dplyr::mutate(count = count.x - ifelse(is.na(count.y), 0, count.y)) %>%
      dplyr::select(-count.x, -count.y)

    zero_state_count <- zero_state_count %>%
      mutate(t = as.numeric(t)*1.0) %>%
      dplyr::left_join(death_per_zero, by = c("t" = death)) %>%
      dplyr::mutate(count = count.x - ifelse(is.na(count.y), 0, count.y)) %>%
      dplyr::select(-count.x, -count.y)

    }
  }

  # now to actually get the population counts -------------------
  state_count <- rbind(one_plus_state_count,zero_state_count)

  cum_state_count <- state_count %>%
    dplyr::mutate(state = factor(state, levels = 0:K, labels = paste0("X",0:K))) %>%
    dplyr::group_by(state) %>%
    tidyr::nest() %>%
    dplyr::mutate(new_data =
             purrr::map(data,
                        function(df){df %>%
                            mutate(cum_count = cumsum(count))})) %>%
    dplyr::select(-data) %>%
    unnest(new_data) %>% select(-count) %>%
    tidyr::pivot_wider(id_cols = "t",
                names_from = "state",values_from = "cum_count") %>%
    dplyr::filter(t >= t_min)


  final_state_count <- cum_state_count
  for(i in 0:(K-1)){
    final_state_count[, paste0("X",i)] <- cum_state_count[, paste0("X",i)] -
      cum_state_count[, paste0("X",i+1)]
  }

  final_state_count <- final_state_count %>%
    dplyr::select(one_of(c("t",paste0("X",0:K)))) %>%
    dplyr::mutate(t = as.numeric(t)) #%>%
    #rbind(c(-1, t(init_counts$value)), .) %>% # I don't think this is the correct way to solve the problem
    #mutate(t = t + 1)

  return(final_state_count)
}


# examples to deal with:
# think group1 = E, group2 = I, group3 = R
# the "_x" is that the individual starts at group "x" when start time = 0
data_example_s <- data.frame(group1 = c(.1,0),
                             group2 = c(2.1,2),
                             group3 = c(3.1,3))

data_example_e <- data.frame(group1 = c(-1.1, -.1,-1),
                             group2 = c(2.1,2.1,2),
                             group3 = c(3.1,3.1,3))

data_example_i <- data.frame(group1 = c(NA, -2.1, NA, -2.1, NA, -2),
                             group2 = c(-1.1,-1.1,-.1,-.1,-1,-1),
                             group3 = c(3.1,3.1,3.1,3.1,3,3))

data_example_r_cont <- data.frame(group1 = c(-3.1, -3.1, NA , NA  , -3.1, -3.1, NA  , NA),
                                  group2 = c(-2.1, -2.1,-2.1, -2.1,   NA, NA  , NA  , NA),
                                  group3 = c(-1.1, -.1, -1.1, -.1 , -1.1, -.1 , -1.1, -.1))

data_example_r_disc <- data.frame(group1 = c(-3, NA, -3, NA),
                                  group2 = c(-2, -2, NA, NA),
                                  group3 = c(-1, -1, -1, -1))


# if we make start_time = 1:
data_example_s[1,] # is now start at E
##all same for _e, _i, r_cont,r_disc

# if we make start_time = -1:
## data_example_s same
data_example_e[2,] #is now starting at S
data_example_i[c(3, 4)] # now start at E
data_example_r_cont[c(2,4,6,8)] # now start at I


# if start_time = -5, birth = -4
## all start out at S
# if start_time = 0, birth = 1
## _s output's S = 0 = EIR at time t = 0
## output t = 1: E = 2, rest == 0
## _e output's S = 0 = EIR at time t = 0
## output t = 1: E = 3, rest == 0
## _i output's S = 0 = rest at time t = 0
## output t = 1: I = 6
## _r_cont output's S = 0 = rest at time t = 0
## output t = 1: r = 8


context("sir-simulation functions")

test_that("update_agents works", {

  current_states <- c(0)
  SIR_count <- c(1, 0, 0)
  beta <- 0
  gamma <- .4
  out <- update_agents(current_states = current_states,
                       SIR_count = SIR_count,
                       beta = beta, gamma = gamma)
  exp_out <- list(states = 0, SIR_count = c(1, 0, 0))
  expect_equal(exp_out, out)

  ##########################
  current_states <- c(0, 1, 1, 2)
  SIR_count <- c(1, 2, 1)
  beta <- 0
  gamma <- 1
  out <- update_agents(current_states = current_states,
                       SIR_count = SIR_count,
                       beta = beta, gamma = gamma)
  exp_out <- list(states = c(0, 2, 2, 2), SIR_count = c(1, 0, 3))
  expect_equal(exp_out, out)

  ##################
  current_states <- c(0, rep(1, 10^2))
  SIR_count <- c(1, 100, 0)
  beta <- 1
  gamma <- 1
  out <- update_agents(current_states = current_states,
                       SIR_count = SIR_count,
                       beta = beta, gamma = gamma)
  exp_states <- rep(2, 100)
  expect_equal(exp_states, out$states[-1])


})


test_that("state_change_inds works", {

  new_states <- c(1, 0, 0)
  current_states <- c(0, 0, 0)
  type <- "inf"
  out <- state_change_inds(new_states = new_states,
                           current_states = current_states,
                           type = type)
  exp_out <- 1
  expect_equal(exp_out, out)
  #######################
  #######################

  new_states <- c(0, 0, 0)
  current_states <- c(0, 0, 0)
  type <- "inf"
  out <- state_change_inds(new_states = new_states,
                           current_states = current_states,
                           type = type)
  exp_out <- 1
  expect_equal(length(out), 0)
  #######################
  #######################

  new_states <- c(2, 2, 2)
  current_states <- c(1, 1, 1)
  type <- "inf"
  out <- state_change_inds(new_states = new_states,
                           current_states = current_states,
                           type = type)
  exp_out <- 1
  expect_equal(length(out), 0)
  #######################
  #######################

  new_states <- c(1, 1, 2)
  current_states <- c(1, 0, 1)
  type <- "rec"
  out <- state_change_inds(new_states = new_states,
                           current_states = current_states,
                           type = type)
  exp_out <- 3
  expect_equal(out, exp_out)
})


test_that("simulate_SIR_agents",{

  sims_data <- simulate_SIR_agents(n_sims = 2, n_time_steps = 5, beta = .5,
                                   gamma = .1, init_SIR = c(9,1,0))
  expect_true(inherits(x = sims_data, what = "data.frame"))
  ###########################
  #############################
  #############################

  ## Make sure max_time_S <= max_time_I
  init_SIR <-  c(9,1,0)
  out <- simulate_SIR_agents(n_sims = 1,
                             n_time_steps = 10,
                             beta = .5,  gamma = .1,
                             init_SIR = init_SIR)
  diffs <- out$tR - out$tI
  expect_true(all((diffs >= 0)|(is.na(diffs))))

})






test_that("sir sims and agents_to_aggregate", {


  n_sims <- 1
  n_time_steps <- 50
  beta <- .1
  gamma <- .03
  init_SIR <- c(90, 10, 0)
  

  agents <- simulate_SIR_agents(n_sims = n_sims,
                             n_time_steps = n_time_steps,
                             beta = beta, gamma = gamma,
                             init_SIR = init_SIR)

  
  out2 <- agents_to_aggregate(head(agents) %>% filter(sim == 1), states = c(tI, tR))

  expect_true("X0" %in% colnames(out2))
#############################################
### as data frame
  df <- agents %>% dplyr::filter(sim == 1) %>% as.data.frame()
  out2 <- agents_to_aggregate(df, states = c(tI, tR))
  expect_true("X0" %in% colnames(out2))

###################################
  ## grouped df
  n_sims <- 1
  n_time_steps <- 50
  beta <- .1
  gamma <- .03
  init_SIR <- c(90, 10, 0)
  

  agents <- simulate_SIR_agents(n_sims = 5,
                             n_time_steps = n_time_steps,
                             beta = beta, gamma = gamma,
                             init_SIR = init_SIR)
  df <- agents %>% group_by(sim)
  out2 <- agents_to_aggregate(df, states = c(tI, tR))


  expect_true("X0" %in% colnames(out2))

})




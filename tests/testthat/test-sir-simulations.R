context("sir-simulation functions")

test_that("simulate_SIR_agents_groups",{

  par_mat <- matrix(c(.5, .25,
                       .8, .2), byrow = TRUE, ncol = 2)
  init_mat <- matrix(c(90, 10, 0,
                       45, 5, 0), byrow = TRUE, ncol = 3)

  sims_data <- simulate_SIR_agents_groups(n_sims = 5,
                n_time_steps = 10,
                par_mat = par_mat,
                init_mat = init_mat,
                output_format = "data.frame")
  expect_equal(ncol(sims_data), 6)
  expect_equal(nrow(sims_data), 5 * (100 + 50))


})






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
  n_sims <- 1
  n_time_steps <- 5
  beta <- 0
  gamma <- 1
  init_SIR <- c(2, 1, 0)
  output_format <- "array"

  out <- simulate_SIR_agents(n_sims = n_sims,
                             n_time_steps = n_time_steps,
                             beta = beta, gamma = gamma,
                             init_SIR = init_SIR,
                             output_format = output_format)
  expect_equal(dim(out), c(n_sims, 3, sum(init_SIR)))
  mat <- matrix(c(0, 0, 1,
                  4, 4, 0,
                  4, 4, 0), byrow = TRUE, nrow = 3)
  expect_equal(as.numeric(out[1,,]), as.numeric(mat))
  ############
  ############
  n_sims <- 2
  n_time_steps <- 5
  beta <- 0
  gamma <- 1
  init_SIR <- c(2, 1, 0)
  output_format <- "array"

  out <- simulate_SIR_agents(n_sims = n_sims,
                             n_time_steps = n_time_steps,
                             beta = beta, gamma = gamma,
                             init_SIR = init_SIR,
                             output_format = output_format)
  expect_equal(dim(out), c(n_sims, 3, sum(init_SIR)))
  mat <- matrix(c(0, 0, 1,
                  4, 4, 0,
                  4, 4, 0), byrow = TRUE, nrow = 3)
  expect_equal(as.numeric(out[1,,]), as.numeric(mat))
  ############
  ############
  ############

  sims_data <- simulate_SIR_agents(n_sims = 2, n_time_steps = 5, beta = .5,
                                   gamma = .1, init_SIR = c(9,1,0),
                                   output_format = "data.frame")
  expect_true(inherits(x = sims_data, what = "data.frame"))
  ###########################
  #############################
  #############################

  ## Make sure max_time_S <= max_time_I
  n_sims <- 100
  n_time_steps <- 100
  beta <- .5
  gamma <- .1
  init_SIR <- c(10, 90, 0)
  output_format <- "array"

  out <- simulate_SIR_agents(n_sims = n_sims,
                             n_time_steps = n_time_steps,
                             beta = beta, gamma = gamma,
                             init_SIR = init_SIR,
                             output_format = output_format)
  expect_true(all(out[ ,2,] <= out[,3,]))

  ####
  ####  ## Make sure max_time_S <= max_time_I
  n_sims <- 100
  n_time_steps <- 100
  beta <- .5
  gamma <- .1
  init_SIR <- c(10, 90, 0)
  output_format <- "data.frame"

  out <- simulate_SIR_agents(n_sims = n_sims,
                             n_time_steps = n_time_steps,
                             beta = beta, gamma = gamma,
                             init_SIR = init_SIR,
                             output_format = output_format)
  expect_true(all(out$max_time_S <= out$max_time_I))





  # U_g <- out %>% dplyr::group_by(sim)
  # sir_group <- UtoX_SIR_group(U_g, T = 10)
})


test_that("fortify_sims_array", {

  ############
  n_sims <- 2
  n_time_steps <- 5
  beta <- 0
  gamma <- 1
  init_SIR <- c(2, 1, 0)
  output_format <- "array"

  sims_data <- simulate_SIR_agents(n_sims = n_sims,
                             n_time_steps = n_time_steps,
                             beta = beta, gamma = gamma,
                             init_SIR = init_SIR,
                             output_format = output_format)

  out <- fortify_sims_array(sims_data)
  expect_true(inherits(out, "data.frame"))





})

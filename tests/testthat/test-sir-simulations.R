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
  old_states <- c(0, 0, 0)
  type <- "inf"
  out <- state_change_inds(new_states = new_states,
                           current_states = current_states,
                           type = type)
  exp_out <- 1
  expect_equal(exp_out, out)

})

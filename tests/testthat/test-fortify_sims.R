
test_that("fortify_sims", {

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

  out <- fortify_sims(sims_data)
  expect_true(inherits(out, "data.frame"))






})



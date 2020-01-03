test_that("fortify_pomp()", {
  data("pomp_df")
  out <- fortify_pomp(pomp_df)
  expect_equal(ncol(out), 5)
  expect_true(all(c("t", "S", "I", "R", "sim") %in% colnames(out)))
#######
  data("pomp_pomp")
  out2 <- fortify_pomp(pomp_pomp)
  expect_equal(ncol(out2), 5)
  expect_true(all(c("t", "S", "I", "R", "sim") %in% colnames(out2)))
####
  data("pomp_arr")
  out3 <- fortify_pomp(pomp_arr)
  expect_equal(ncol(out3), 5)
  expect_true(all(c("t", "S", "I", "R", "sim") %in% colnames(out3)))

###
  out71 <- fortify_pomp(pomp_pomp)
  expect_equal(ncol(out71), 5)
  expect_true(all(c("t", "S", "I", "R", "sim") %in% colnames(out71)))


})


test_that("fortify_EpiModel()", {

  data(EpiModel_det)

  out <- fortify_EpiModel(EpiModel_det)
  expect_true("data.frame" %in% class(out))
  expect_equal(dim(out), c(300,4))

  ## THE ICM
  data(EpiModel_icm)
  out <- fortify_EpiModel(EpiModel_icm)
  expect_true("data.frame" %in% class(out))
  expect_equal(dim(out), c(3000,5))


})



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



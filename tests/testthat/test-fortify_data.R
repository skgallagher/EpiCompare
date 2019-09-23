test_that("Misc", {
  expect_equal(2 * 2, 4)

  # library(SimInf)
  #
  #
  # transitions <- c("S -> beta*S*I/(S+I+R) -> I",
  #                  "I -> gamma*I -> R")
  # compartments <- c("S", "I", "R")
  #
  # n <- 1000
  # u0 <- data.frame(S = rep(99, n), I = rep(5, n), R = rep(0, n))
  #
  # model <- mparse(transitions = transitions,
  #                 compartments = compartments,
  #                 gdata = c(beta = 0.16, gamma = 0.077),
  #                 u0 = u0,
  #                 tspan = 1:180)
  #
  # result <- run(model)
  # result


})

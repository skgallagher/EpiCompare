context("utility functions")

test_that("fortify_agent is able to recreate timeternR::hagelloch_agents", {
  # note currently agent 141 has values that don't match...
  fortify_df <- fortify_agents(timeternR::hagelloch_raw,
                               time_col = c("tI","tR"),
                               T = 90)
  testthat::expect_equal(fortify_df[,(ncol(fortify_df) - 2):ncol(fortify_df)],
                        as.data.frame(t(timeternR::hagelloch_agents)))
})

test_that("fortify_agent errors when incorrect time_cols entered", {
  # note currently agent 141 has values that don't match...
  testthat::expect_error(fortify_agents(timeternR::hagelloch_raw,
                               time_col = c("tI")))
  testthat::expect_error(fortify_agents(timeternR::hagelloch_raw,
                                        time_col = c("tI", "tR, tDeath")))
  testthat::expect_error(fortify_agents(timeternR::hagelloch_raw,
                                        time_col = c("tI", "banana")))
})


test_that("UtoX_SIR passes basic checks", {
  # same test as in the example string
  sir_out <- UtoX_SIR(as.data.frame(t(timeternR::hagelloch_agents)))
  testthat::expect_equal(sir_out, timeternR::hagelloch_sir)
})

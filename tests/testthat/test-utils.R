context("utility functions")

test_that("fortify_agent is able to recreate timeternR::hagelloch_agents", {
  # note currently agent 141 has values that don't match...
  fortify_df <- fortify_agents(timeternR::hagelloch_raw,
                               time_col = c("tI","tR"),
                               T = 94)
  testthat::expect_equal(fortify_df[,(ncol(fortify_df) - 2):ncol(fortify_df)],
                       timeternR::hagelloch_agents)
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
  sir_out <- UtoX_SIR(timeternR::hagelloch_agents)
  testthat::expect_equal(sir_out, timeternR::hagelloch_sir)
})

test_that("UtoX_SIR_group passes basic checks", {
  library(dplyr)
  # same test as in the example string
  T <- 100
  U_g <- hagelloch_raw %>% fortify_agents() %>% group_by(AGE2 = as.numeric(cut(AGE,3)))
  sir_group <- UtoX_SIR_group(U_g, T)
  U <- U_g %>%
    filter(AGE2 == 1) %>% ungroup()
  sir_group1 <- UtoX_SIR(U, T)
  sir_group_1 <- sir_group %>% filter(AGE2 == 1)
  testthat::expect_equal(sir_group1,
                         sir_group_1 %>% select(t, S, I, R) %>% data.frame)
})


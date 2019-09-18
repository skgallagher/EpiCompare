context("test for utility functions")

test_that("fortify_agent has logical output for hagelloch_raw", {
  fortify_df <- fortify_agents(timeternR::hagelloch_raw,
                               time_col = c("tI","tR"),
                               max_time = 94)
  # at least 1 person "started" the outbreak
  start_ind <- which(fortify_df$init_state == 1) # this might always be true tho
  testthat::expect_true(length(start_ind) > 0)

  testthat::expect_true(min(fortify_df$tI) <= 0)

})

test_that(paste("fortify_agent has logical output for hagelloch_raw2",
                "(and works with NAs in tI, tR spots)"), {
  fortify_df <- fortify_agents(timeternR::hagelloch_raw2,
                               time_col = c("tI","tR"),
                               max_time = 94)
  # at least 1 person "started" the outbreak
  start_ind <- which(fortify_df$init_state == 1) # this might always be true tho
  testthat::expect_true(length(start_ind) > 0)

  testthat::expect_true(min(fortify_df$tI, na.rm = TRUE) <= 0)

  testthat::expect_equal(sum(is.na(fortify_df[, c("max_time_S","max_time_I")])),
                         0)

  bad <- hagelloch_raw2
  bad$tI[188] <- NA
  testthat::expect_error(fortify_agents(bad, time_col = c("tI","tR"),
                                        max_time = 94))

})

test_that("fortify_agent is able to recreate timeternR::hagelloch_agents", {
  fortify_df <- fortify_agents(timeternR::hagelloch_raw,
                               time_col = c("tI","tR"),
                               max_time = 94)

  testthat::expect_setequal(union(unique(which(fortify_df$init_state == 1)),
                               unique(which.min(fortify_df$max_time_S))),
                         unique(which.min(fortify_df$max_time_S)))

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
  sir_out <- UtoX_SIR(timeternR::hagelloch_agents, max_time = 94)
  testthat::expect_equal(sir_out, timeternR::hagelloch_sir)

  testthat::expect_true(all(diff(sir_out$S) <= 0),
                        label = paste("*S should always be decreasing*:",
                                      "all(diff(sir_out$S) <= -1)"))


})



test_that("UtoX_SIR_group passes basic checks", {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(dplyr, quietly = TRUE)
      ))
  # same test as in the example string
  max_time <- 100
  U_g <- hagelloch_raw %>% fortify_agents() %>% group_by(AGE2 = as.numeric(cut(AGE,3)))
  sir_group <- UtoX_SIR_group(U_g, max_time)
  U <- U_g %>%
    filter(AGE2 == 1) %>% ungroup()
  sir_group1 <- UtoX_SIR(U, max_time)
  sir_group_1 <- sir_group %>% filter(AGE2 == 1)
  testthat::expect_equal(sir_group1,
                         sir_group_1 %>% ungroup %>%
                           select(t, S, I, R) %>% data.frame)
})




test_that("fortification and UtoX_sir work together", {
  fortified_data <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    UtoX_SIR() %>%
    .[, c("S", "I", "R")] %>%
    dplyr::rename(x = "S", y = "I", z = "R")

  testthat::expect_equal(length(unique(apply(fortified_data[,c("x","y","z")]
                                             ,1, sum))), 1)
  testthat::expect_true(all(fortified_data[,c("x","y","z")] >= 0))
  testthat::expect_true(all(diff(fortified_data$x) <= 0),
                        label = paste("*S should always be decreasing*:",
                                      "all(diff(sir_out$S) <= -1)"))

})



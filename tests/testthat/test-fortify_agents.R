context("test for fortify_agent functions")

test_that("fortify_agent has logical output for hagelloch_raw", {
  fortify_df <- fortify_agents(timeternR::hagelloch_raw,
                               time_col = c("tI","tR"),
                               max_time = 94)
  # at least 1 person "started" the outbreak
  start_ind <- which(fortify_df$init_state == 1) # this might always be true tho
  testthat::expect_true(length(start_ind) > 0)

  testthat::expect_true(min(fortify_df$tI) <= 0)

})

test_that(paste("fortify_agents has logical output for hagelloch_raw2",
                "(and works with NAs in tI, tR spots)"), {
  fortify_df <- fortify_agents(timeternR::hagelloch_raw2,
                               time_col = c("tI","tR"),
                               max_time = 94)

  testthat::expect_equal(fortify_df %>% dplyr::pull(init_state) %>% table,
                         c(rep(0, 185),1,2,2) %>% table)

  # at least 1 person "starts" being infected at time 0
  testthat::expect_true(sum(fortify_df$init_state == 1) > 0)
  # and that, this idea relates to at least 1 person having a tI <= 0
  testthat::expect_true(min(fortify_df$tI, na.rm = TRUE) <= 0)

  testthat::expect_equal(sum(is.na(fortify_df[, c("max_time_S","max_time_I")])),
                         5)

  bad <- hagelloch_raw2
  bad$tI[188] <- NA
  testthat::expect_error(fortify_agents(bad, time_col = c("tI","tR"),
                                        max_time = 94))

})

test_that("fortify_agent is able to recreate timeternR::hagelloch_agents", {
  fortify_df <- fortify_agents(timeternR::hagelloch_raw,
                               time_col = c("tI","tR"),
                               max_time = 94)

  testthat::expect_setequal(unique(which(fortify_df$init_state == 1)),
                         unique(which(is.na(fortify_df$max_time_S))))

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

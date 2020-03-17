context("tests for stat_aggregate visual")

.newstat = c(sir_aggregate = "StatSirAggregate")
.newgeom = c(sir = "GeomSirAggregate")
update_approved_layers(stat_name = .newstat, geom_name = .newgeom)

# StatSirAggregate ----------------------------

test_that("check StatSirAggregate works correctly with groups", {
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    geom_path(stat = StatSirAggregate) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 1,
                         label = "*single group problem*: group_count")
  testthat::expect_true(all(data_vis[,c("x", "y", "z")] >= 0))
  testthat::expect_true(all(diff(data_vis$x) <= 0),
                        label = paste("*S should always be decreasing*:",
                                      "all(diff(sir_out$S) <= -1)"))
  # multiple groups:
  vis <- EpiCompare::hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR, color = SEX)) +
    geom_path(stat = StatSirAggregate) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R",
         color = "Gender")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 2,
                         label = "*multiple group problem*: group_count")
  testthat::expect_true(all(data_vis[,c("x", "y", "z")] >= 0))

})

# stat_aggregate testing ----------------------

test_that("check stat_aggregate works correctly with groups", {
  library(ggplot2)
  # a single group
  vis <- EpiCompare::hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    stat_aggregate(geom = "path") + # note geom = "path" is the default
    ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 1,
                         label = "*single group problem*: group_count")
  # multiple groups:
  vis <- EpiCompare::hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR, color = SEX)) +
    stat_aggregate(geom = "path") + # note geom = "path" is the default
    ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R",
         color = "Gender")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 2,
                         label = "*multiple group problem*: group_count")

})

test_that("check stat_aggregate for raw works correctly with NAs", {
  library(ggplot2)
  set.seed(1)
  pretend_actual_data <- EpiCompare::simulate_SIR_agents(
    n_sims = 1,
    n_time_steps = 1000,
    beta = .0099, gamma = .0029,
    init_SIR = c(950, 50, 0))

  # just making sure it has NA values...
  testthat::expect_true(sum(is.na(pretend_actual_data[, c("tI", "tR")])) > 0)

  stat_agg_vis_with_na <- pretend_actual_data %>%
    ggplot() +
    stat_aggregate(aes(y = tI, z = tR)) +
    coord_tern()
  # tells us below if there are removals (which there shouldn't be...)
  testthat::expect_silent(layer_data(stat_agg_vis_with_na))
})

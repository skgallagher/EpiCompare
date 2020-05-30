context("tests for geom_aggregate visual")


.newstat = c(sir_aggregate = "StatSirAggregate")
.newgeom = c(sir = "GeomSirAggregate")
update_approved_layers(stat_name = .newstat, geom_name = .newgeom)

# geom_aggregate testing --------------

test_that("check geom_aggregate for raw works correctly with groups", {
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    geom_aggregate() + ggtern::coord_tern() +
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

test_that("check geom_aggregate for raw works correctly with NAs", {
  library(ggplot2)
  set.seed(1)
  pretend_actual_data <- EpiCompare::simulate_SIR_agents(
    n_sims = 1,
    n_time_steps = 1000,
    beta = .0099, gamma = .0029,
    init_SIR = c(950, 50, 0))

  # just making sure it has NA values...
  testthat::expect_true(sum(is.na(pretend_actual_data[, c("tI", "tR")])) > 0)

  geom_agg_vis_with_na <- pretend_actual_data %>%
    ggplot() +
    geom_aggregate(aes(y = tI, z = tR)) +
    coord_tern()
  # tells us below if there are removals (which there shouldn't be...)
  testthat::expect_silent(layer_data(geom_agg_vis_with_na))
})

test_that("NAs for geom_aggregate", {
  df <- data.frame(agent_id = factor(1:5),
                   sim = factor(1),
                   tI = c(446, NA, 196, 465, NA),
                   tR = c(464, NA, 425, 476, NA))
  
  g <- df %>% ggplot() +
    geom_aggregate(aes(y = tI, z = tR),
                   color = "blue") +
    coord_tern()
  
  print(g)
  dev.off()
  expect_true(is.ggplot(g))
})

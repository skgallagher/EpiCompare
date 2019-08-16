context("tests for visualizations")


test_that("check StatSIR underlying data is as expected", {
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    geom_path(stat = StatSIR) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)[,c("x", "y", "z")]

  fortified_data <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    UtoX_SIR() %>%
    .[, c("S", "I", "R")] %>%
    dplyr::rename(x = "S", y = "I", z = "R")

  testthat::expect_equal(fortified_data, data_vis)

})

test_that("check statSIR works correctly with groups", {
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    geom_path(stat = StatSIR) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 1,
                         label = "*single group problem*: group_count")
  # multiple groups:
  vis <- timeternR::hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR, color = SEX)) +
    geom_path(stat = StatSIR) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R",
         color = "Gender")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 2,
                         label = "*multiple group problem*: group_count")

})

test_that("check stat_sir underlying data is as expected", {
  library(ggplot2)
  library(ggtern)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    stat_sir() + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)[,c("x", "y", "z")]

  fortified_data <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    UtoX_SIR() %>%
    .[, c("S", "I", "R")] %>%
    dplyr::rename(x = "S", y = "I", z = "R")

  testthat::expect_equal(fortified_data, data_vis)

})

test_that("check stat_sir works correctly with groups", {
  library(ggplot2)
  # a single grouyp
  vis <- timeternR::hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    stat_sir(geom = "path") + # note geom = "path" is the default
    ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 1,
                         label = "*single group problem*: group_count")
  # multiple groups:
  vis <- timeternR::hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR, color = SEX)) +
    stat_sir(geom = "path") + # note geom = "path" is the default
    ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R",
         color = "Gender")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 2,
                         label = "*multiple group problem*: group_count")

})




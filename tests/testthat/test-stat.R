context("tests for visualizations/ stats")


.newstat = c(sir_raw       = "StatSirRaw",
             sir_fortified = "StatSirFortified")
.newgeom = c(sir = "GeomSIR")
update_approved_layers(stat_name = .newstat, geom_name = .newgeom)

# stat_sir testing ----------------------

# StatSirRaw ----------------------------

test_that("check StatSirRaw underlying data is as expected (data_type = 'raw')", {
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    geom_path(stat = StatSirRaw) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)[,c("x", "y", "z")]
  testthat::expect_equal(length(unique(apply(data_vis,1, sum))), 1)
  testthat::expect_true(all(data_vis >= 0))
  testthat::expect_true(all(diff(data_vis$x) <= 0),
                        label = paste("*S should always be decreasing*:",
                                      "all(diff(sir_out$S) <= -1)"))

  fortified_data <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    agents_to_aggregate_SIR() %>%
    .[, c("S", "I", "R")] %>%
    dplyr::rename(x = "S", y = "I", z = "R")


  testthat::expect_true(all(c(any(class(fortified_data) == "data.frame"),
                              any(class(data_vis) == "data.frame"))))
  expect_equal(as.matrix(fortified_data),
               as.matrix(data_vis))

})

test_that("check StatSirRaw works correctly with groups (data_type = 'raw')", {
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    geom_path(stat = StatSirRaw) + ggtern::coord_tern() +
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
  vis <- timeternR::hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR, color = SEX)) +
    geom_path(stat = StatSirRaw) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R",
         color = "Gender")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 2,
                         label = "*multiple group problem*: group_count")
  testthat::expect_true(all(data_vis[,c("x", "y", "z")] >= 0))

})

test_that("check stat_sir underlying data is as expected (data_type = 'raw')", {
  library(ggplot2)
  library(ggtern)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    stat_sir(data_type = "raw") + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)[,c("x", "y", "z")]

  testthat::expect_equal(length(unique(apply(data_vis,1, sum))), 1)
  testthat::expect_true(all(data_vis >= 0))
  testthat::expect_true(all(diff(data_vis$x) <= 0),
                        label = paste("*S should always be decreasing*:",
                                      "all(diff(sir_out$S) <= -1)"))

  fortified_data <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    agents_to_aggregate_SIR() %>%
    .[, c("S", "I", "R")] %>%
    dplyr::rename(x = "S", y = "I", z = "R")

  testthat::expect_true(all(c(any(class(fortified_data) == "data.frame"),
                              any(class(data_vis) == "data.frame"))))
  expect_equal(as.matrix(fortified_data), as.matrix(data_vis))

})

test_that("check stat_sir works correctly with groups (data_type = 'raw')", {
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



# StatSirFortified -----------------------

test_that("check StatSirFortified underlying data is as expected (data_type = 'fortified')", {
  # this test is currently failing...
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    ggplot(., aes(x = max_time_S, y = max_time_I, init_state = `init_state`)) +
    geom_path(stat = StatSirFortified) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)[,c("x", "y", "z")]

  testthat::expect_equal(length(unique(apply(data_vis,1, sum))), 1)
  testthat::expect_true(all(data_vis >= 0))

  fortified_data <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    agents_to_aggregate_SIR() %>%
    .[, c("S", "I", "R")] %>%
    dplyr::rename(x = "S", y = "I", z = "R")

   testthat::expect_true(all(c(any(class(fortified_data) == "data.frame"),
                               any(class(data_vis) == "data.frame"))))
   expect_equal(as.matrix(fortified_data), as.matrix(data_vis))

})

test_that("check StatSirFortified underlying with multiple groups is as expected (data_type = 'fortified')", {
  library(ggplot2)
  # a single group
  vis <- agents_sims_tidy %>%
    ggplot(., aes(x = SMax, y = IMax, init_state = init_state, group = sim)) +
    geom_path(stat = StatSirFortified, alpha = .1, na.rm = FALSE) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")
  # ^ this seems to be due to the fact that SMax and IMax has NA values and these are dropped
  data_vis <- layer_data(vis)[,c("x", "y", "z")]


  testthat::expect_true(all(data_vis >= 0))


})


# geom_sir testing --------------

## Raw ----------------------------

test_that("check geom_sir for Raw underlying data is as expected (data_type = 'raw')", {
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    geom_sir() + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)[,c("x", "y", "z")]
  testthat::expect_equal(length(unique(apply(data_vis,1, sum))), 1)
  testthat::expect_true(all(data_vis >= 0))
  testthat::expect_true(all(diff(data_vis$x) <= 0),
                        label = paste("*S should always be decreasing*:",
                                      "all(diff(sir_out$S) <= -1)"))

  fortified_data <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    agents_to_aggregate_SIR() %>%
    .[, c("S", "I", "R")] %>%
    dplyr::rename(x = "S", y = "I", z = "R")

  testthat::expect_true(all(c(any(class(fortified_data) == "data.frame"),
                              any(class(data_vis) == "data.frame"))))
  expect_equal(as.matrix(fortified_data), as.matrix(data_vis))
  })

test_that("check geom_sir for Raw works correctly with groups (data_type = 'raw')", {
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR)) +
    geom_sir() + ggtern::coord_tern() +
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
  vis <- timeternR::hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>%
    ggplot(., aes(y = tI, z = tR, color = SEX)) +
    geom_path(stat = StatSirRaw) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R",
         color = "Gender")

  data_vis <- layer_data(vis)

  group_count <- length(unique(apply(data_vis[,c("x", "y", "z")], 1, sum)))
  testthat::expect_equal(group_count, 2,
                         label = "*multiple group problem*: group_count")
  testthat::expect_true(all(data_vis[,c("x", "y", "z")] >= 0))

})


## Fortified -----------------------

test_that("check geom_sir with fortified underlying data is as expected (data_type = 'fortified')", {
  # this test is currently failing...
  library(ggplot2)
  # a single group
  vis <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    ggplot(., aes(x = max_time_S, y = max_time_I, init_state = `init_state`)) +
    geom_sir(data_type = "fortified") + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)[,c("x", "y", "z")]

  testthat::expect_equal(length(unique(apply(data_vis,1, sum))), 1)
  testthat::expect_true(all(data_vis >= 0))

  fortified_data <- hagelloch_raw %>%
    dplyr::filter(SEX %in% c("male", "female")) %>% fortify_agents() %>%
    agents_to_aggregate_SIR() %>%
    .[, c("S", "I", "R")] %>%
    dplyr::rename(x = "S", y = "I", z = "R")

    testthat::expect_true(all(c(any(class(fortified_data) == "data.frame"),
                                any(class(data_vis) == "data.frame"))))
    expect_equal(as.matrix(fortified_data),
                 as.matrix(data_vis))
})

test_that("check geom_sir with fortitfied data underlying with multiple groups is as expected (data_type = 'fortified')", {
  library(ggplot2)
  # a single group
  vis <- agents_sims_tidy %>%
    ggplot(., aes(x = SMax, y = IMax, init_state = init_state, group = sim)) +
    geom_sir(data_type = "fortified", alpha = .1) + ggtern::coord_tern() +
    labs(x = "S", y = "I", z = "R")

  data_vis <- layer_data(vis)[,c("x", "y", "z")]


  expect_equal(length(unique(rowSums(data_vis))), 1)
  testthat::expect_true(all(data_vis >= 0))


})


test_that("Test individual simulation of agents_sims_tidy", {

    out1 <- agents_sims_tidy %>%
        dplyr::filter(sim == 1)

    out1 <- agents_to_aggregate_SIR(out1, ind = c(3, 4, 5))


    sims1 <- agents_sims_tidy %>%
        dplyr::filter(sim == 1)


    library(ggplot2)
    vis <- sims1 %>%
        ggplot(., aes(x = SMax, y = IMax,
                      init_state = init_state, group = sim)) +
        geom_sir(data_type = "fortified", alpha = .1) + ggtern::coord_tern() +
        labs(x = "S", y = "I", z = "R")

    data_vis <- layer_data(vis)[,c("x", "y", "z")]


    expect_equal(rowSums(out1[, -1]), rowSums(data_vis))

  testthat::expect_true(all(data_vis >= 0))

})

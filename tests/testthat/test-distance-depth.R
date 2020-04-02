library(dplyr)
library(tidyr)

context("test for distance-depth related functions")

test_that("test for depth_curves_to_points.list", {
  set.seed(1)
  random_data_list <- lapply(1:3, function(x){data.frame(matrix(rnorm(10),
                                                                ncol = 2))})

  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)

  combined_points_list <- depth_curves_to_points(random_data_list,
                                                 alpha = .2,
                                                 dist_mat)

  testthat::expect_equal(combined_points_list, random_data_list[[1]])
})

test_that("test for depth_curves_to_points.grouped_df", {
  set.seed(1)

  random_data_df <- lapply(1:3, function(x){data.frame(matrix(rnorm(10),
                                                              ncol = 2))}) %>%
    do.call(rbind, .) %>%
    mutate(id = rep(1:3, each = 5)) %>%
    group_by(id)

  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)

  combined_points_df <- depth_curves_to_points(random_data_df,
                                               alpha = .2,
                                               dist_mat)

  testthat::expect_equal(combined_points_df,
                         random_data_df %>% filter(id == 1))
})

test_that("test for distance_depth_function", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)

  dd_vec <- distance_depth_function(dist_mat)

  testthat::expect_equal(dd_vec, c(1,0,0))

  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ))
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ))

  testthat::expect_error(distance_depth_function(dist_mat_not_sym))
  testthat::expect_error(distance_depth_function(dist_mat_not_pos))

})

test_that("test for local_distance_depth_function", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  
  dd_vec <- local_distance_depth_function(dist_mat) # c(1,0,0)
  
  ldd_vec1 <- local_distance_depth_function(dist_mat, tau = 2) # c(1,0,0)
  ldd_vec2 <- local_distance_depth_function(dist_mat, tau = 1.5) # c(1,0,0)
  ldd_vec3 <- local_distance_depth_function(dist_mat, tau = 1) # c(0,0,0)
  ldd_vec4 <- local_distance_depth_function(dist_mat, tau = .1) # c(0,0,0)
  
  testthat::expect_equal(dd_vec, c(1,0,0)) # same as global depth
  testthat::expect_equal(ldd_vec1, c(1,0,0)) # same as global depth
  testthat::expect_equal(ldd_vec2, c(1,0,0)) # works relative to S constraint
  testthat::expect_equal(ldd_vec3, c(0,0,0)) # works relative to S constraint (only 1 in each box)
  testthat::expect_equal(ldd_vec4, c(0,0,0)) # works relative to S constraint (only 0 in each box)
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ))
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ))
  
  testthat::expect_error(local_distance_depth_function(dist_mat_not_sym))
  testthat::expect_error(local_distance_depth_function(dist_mat_not_pos))
  
})


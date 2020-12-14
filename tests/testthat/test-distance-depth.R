library(dplyr)
library(tidyr)

context("test for distance-depth related functions")

test_that("test for quantile_curves_to_points.list, depth", {
  set.seed(1)
  random_data_list <- lapply(1:3, function(x){data.frame(matrix(rnorm(10),
                                                                ncol = 2))})

  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)

  combined_points_list <- quantile_curves_to_points(random_data_list,
                                                 alpha = .2,
                                                 dist_mat)

  testthat::expect_equal(combined_points_list, random_data_list[[1]])
})

test_that("test for quantile_curves_to_points.grouped_df, depth", {
  set.seed(1)

  random_data_df <- lapply(1:3, function(x){data.frame(matrix(rnorm(10),
                                                              ncol = 2))}) %>%
    do.call(rbind, .) %>%
    dplyr::mutate(id = rep(1:3, each = 5)) %>%
    dplyr::group_by(id) %>% dplyr::select(id, X1, X2)

  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)

  combined_points_df <- quantile_curves_to_points(random_data_df,
                                               alpha = .2,
                                               dist_mat)

  testthat::expect_equivalent(combined_points_df,
                              random_data_df %>% filter(id == 1))
})

test_that("test for top_curves_to_points.list, depth (univariate ordering)", {
  set.seed(1)
  random_data_list <- lapply(1:3, function(x){data.frame(matrix(rnorm(10),
                                                                ncol = 2))})
  
  
  
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  tidy_dm <- tidy_dist_mat(dist_mat)
  x_names_df <- rownames(tidy_dm)
  
  
  # missing x_names_df
  testthat::expect_error(
    combined_points_list <- top_curves_to_points(random_data_list,
                                               alpha = .2,
                                               tidy_dm = tidy_dm)
  )
  
  combined_points_list <- top_curves_to_points(random_data_list,
                                                 alpha = .2,
                                                 tidy_dm = tidy_dm,
                                                 x_names_df = x_names_df)
  
  testthat::expect_equal(combined_points_list, random_data_list[[1]])
  
  # different ordering of x_names_df and rownames(tidy_dm)
  x_names_df2 <- data.frame(id = c(2,1,3))
  
  combined_points_list2 <- top_curves_to_points(random_data_list,
                                                    alpha = .2,
                                                    tidy_dm = tidy_dm,
                                                    x_names_df = x_names_df2)
  
  testthat::expect_equal(combined_points_list2, random_data_list[[2]])
  
})

test_that("test for top_curves_to_points.list, depth (bivariate ordering)", {
  set.seed(1)
  random_data_list <- lapply(1:4, function(x){data.frame(matrix(rnorm(10),
                                                                ncol = 2))})

  dist_mat <- as.matrix(dist(data.frame(matrix(rnorm(4), ncol = 1))))
  
  rownames_df <- data.frame(id = c(1,2,1,2),
                            id2 = rep(c("first","second"), each =2))
  
  tidy_dm <- tidy_dist_mat(dist_mat, rownames_df, rownames_df)
  x_names_df <- rownames_df
  
  
  # missing x_names_df
  testthat::expect_error(
    combined_points_list <- top_curves_to_points(random_data_list,
                                                 alpha = .2,
                                                 tidy_dm = tidy_dm)
  )
  
  combined_points_list <- top_curves_to_points(random_data_list,
                                                    alpha = .2,
                                                    tidy_dm = tidy_dm,
                                                    x_names_df = x_names_df)
  
  #quantile_scores <- distance_depth_function(tidy_dm, df_out = T)
  
  testthat::expect_equal(combined_points_list, 
                         rbind(random_data_list[[1]],
                               random_data_list[[4]]))
  
  # different ordering of x_names_df and rownames(tidy_dm)
  x_names_df2 <- rownames_df[c(2,1,3,4),]
  
  combined_points_list2 <- top_curves_to_points(random_data_list,
                                                     alpha = .2,
                                                     tidy_dm = tidy_dm,
                                                     x_names_df = x_names_df2)
  
  testthat::expect_equal(combined_points_list2, rbind(random_data_list[[2]],
                                                      random_data_list[[4]]))
  
})

test_that("test for top_curves_to_points.grouped_df, depth (univariate ordering)", {
  set.seed(1)

  random_data_list <- lapply(1:3, function(x){data.frame(matrix(rnorm(10),
                                                                ncol = 2))})
  random_data_df <- random_data_list %>%
    do.call(rbind, .) %>%
    dplyr::mutate(id = rep(1:3, each = 5)) %>%
    dplyr::group_by(id) %>% dplyr::select(id, X1, X2)
  
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  tidy_dm <- tidy_dist_mat(dist_mat)
  
  
  combined_points_gd <- top_curves_to_points(random_data_df,
                                             alpha = .2,
                                             tidy_dm = tidy_dm)
  
  testthat::expect_equal(as.data.frame(combined_points_gd), 
                         random_data_list[[1]])
  
  # different ordering of random_data_df and rownames(tidy_dm)
  rownames <- data.frame(id = c(2,1,3))
  tidy_dm2 <-  tidy_dist_mat(dist_mat, rownames, rownames)
  
  combined_points_gd2 <- top_curves_to_points(random_data_df,
                                              alpha = .2,
                                              tidy_dm = tidy_dm2)
  
  testthat::expect_equal(as.data.frame(combined_points_gd2), 
                         random_data_list[[2]])
  
})

test_that("test for top_curves_to_points.grouped_df, depth (bivariate ordering)", {
  set.seed(1)
  random_data_list <- lapply(1:4, function(x){data.frame(matrix(rnorm(10),
                                                                ncol = 2))})
  
  random_data_df <- random_data_list %>%
    do.call(rbind, .) %>%
    dplyr::mutate(id = rep(c(1,2,1,2), each = 5),
                  id2 = rep(rep(c("first","second"), each =2), each = 5)) %>%
    dplyr::group_by(id, id2) %>% dplyr::select(id, id2, X1, X2)
  
  dist_mat <- as.matrix(dist(data.frame(matrix(rnorm(4), ncol = 1))))
  
  rownames_df <- data.frame(id = c(1,2,1,2),
                            id2 = rep(c("first","second"), each =2))
  
  tidy_dm <- tidy_dist_mat(dist_mat, rownames_df, rownames_df)
  
  
  
  combined_points_gd <- top_curves_to_points(random_data_df,
                                               alpha = .2,
                                               tidy_dm = tidy_dm)
  
  #quantile_scores <- distance_depth_function(tidy_dm, df_out = T)
  
  testthat::expect_equal(as.data.frame(combined_points_gd), 
                         rbind(random_data_list[[1]],
                               random_data_list[[4]]))
  
  # different ordering of x_names_df and rownames(tidy_dm)
  rownames_df2 <- rownames_df[c(2,1,3,4),]
  tidy_dm2 <- tidy_dist_mat(dist_mat, rownames_df2, rownames_df2)
  
  combined_points_gd2 <- top_curves_to_points(random_data_df,
                                                alpha = .2,
                                                tidy_dm = tidy_dm2)
  
  testthat::expect_equal(as.data.frame(combined_points_gd2), 
                         rbind(random_data_list[[2]],
                               random_data_list[[4]]))
  
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
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)

  testthat::expect_error(distance_depth_function(dist_mat_not_sym))
  testthat::expect_error(distance_depth_function(dist_mat_not_pos))

})

test_that("test for distance_depth_function.matrix", {
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


test_that("test for distance_depth_function.tidy_dist_mat (univariate)", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  tidy_dm <- tidy_dist_mat(dist_mat)
  
  # df_out = F
  dd_vec <- distance_depth_function(tidy_dm, df_out = F)
  
  sol1 <- c(1,0,0)
  names(sol1) <- as.character(1:3)
  testthat::expect_equal(dd_vec, sol1)
  
  # df_out = T
  dd_df <- distance_depth_function(tidy_dm, df_out = T)
  
  sol2 <- data.frame(id = 1:3, depth = c(1,0,0))
  testthat::expect_equivalent(dd_df, sol2)
  
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_sym <- tidy_dist_mat(dist_mat_not_sym)
  
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_pos <- tidy_dist_mat(dist_mat_not_pos)
  
  testthat::expect_error(distance_depth_function(tidy_dist_mat_not_sym))
  testthat::expect_error(distance_depth_function(tidy_dist_mat_not_pos))
  
})


test_that("test for distance_depth_function.tidy_dist_mat (bivariate)", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  rownames_df <- data.frame(id = c(1,2,1),
                            id2 = c("a","a", "b"))
  
  tidy_dm <- tidy_dist_mat(dist_mat, rownames_df, rownames_df)
  
  
  
  # df_out = F
  dd_vec <- distance_depth_function(tidy_dm, df_out = F)
  
  sol1 <- c(1,0,0)
  names(sol1) <- rownames_df %>% tidyr::unite(col = "names", sep = "|") %>%
    dplyr::pull(.data$names)
  testthat::expect_equal(dd_vec, sol1)
  
  # df_out = T
  dd_df <- distance_depth_function(tidy_dm, df_out = T)
  
  sol2 <- rownames_df %>% dplyr::mutate(depth = c(1,0,0))
  testthat::expect_equivalent(dd_df, sol2)
  
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_sym <- tidy_dist_mat(dist_mat_not_sym, 
                                         rownames_df, rownames_df)
  
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_pos <- tidy_dist_mat(dist_mat_not_pos, 
                                         rownames_df, rownames_df)
  
  testthat::expect_error(distance_depth_function(tidy_dist_mat_not_sym))
  testthat::expect_error(distance_depth_function(tidy_dist_mat_not_pos))
  
})


test_that("test for local_distance_depth_function.matrix, df_out = F", {
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
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  
  testthat::expect_error(local_distance_depth_function(dist_mat_not_sym))
  testthat::expect_error(local_distance_depth_function(dist_mat_not_pos))
  
})

test_that("test for local_distance_depth_function.matrix, df_out = T", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  
  dd_vec <- local_distance_depth_function(dist_mat, df_out = T) # c(1,0,0)
  
  ldd_vec1 <- local_distance_depth_function(dist_mat, tau = 2, df_out = T) # c(1,0,0)
  ldd_vec2 <- local_distance_depth_function(dist_mat, tau = 1.5, df_out = T) # c(1,0,0)
  ldd_vec3 <- local_distance_depth_function(dist_mat, tau = 1, df_out = T) # c(0,0,0)
  ldd_vec4 <- local_distance_depth_function(dist_mat, tau = .1, df_out = T) # c(0,0,0)
  
  testthat::expect_equal(dd_vec, data.frame(names = 1:3, local_depth = c(1,0,0))) # same as global depth
  testthat::expect_equal(ldd_vec1, data.frame(names = 1:3, local_depth = c(1,0,0))) # same as global depth
  testthat::expect_equal(ldd_vec2, data.frame(names = 1:3, local_depth = c(1,0,0))) # works relative to S constraint
  testthat::expect_equal(ldd_vec3, data.frame(names = 1:3, local_depth = c(0,0,0))) # works relative to S constraint (only 1 in each box)
  testthat::expect_equal(ldd_vec4, data.frame(names = 1:3, local_depth = c(0,0,0))) # works relative to S constraint (only 0 in each box)
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  
  testthat::expect_error(local_distance_depth_function(dist_mat_not_sym, df_out = T))
  testthat::expect_error(local_distance_depth_function(dist_mat_not_pos, df_out = T))
  
})



test_that("test for local_distance_depth_function.tidy_dist_mat (univariate, df_out = F)", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  tidy_dm <- tidy_dist_mat(dist_mat)
  
  
  dd_vec <- local_distance_depth_function(tidy_dm, df_out = F) # c(1,0,0)
  
  ldd_vec1 <- local_distance_depth_function(tidy_dm, tau = 2, df_out = F) # c(1,0,0)
  ldd_vec2 <- local_distance_depth_function(tidy_dm, tau = 1.5, df_out = F) # c(1,0,0)
  ldd_vec3 <- local_distance_depth_function(tidy_dm, tau = 1, df_out = F) # c(0,0,0)
  ldd_vec4 <- local_distance_depth_function(tidy_dm, tau = .1, df_out = F) # c(0,0,0)
  
  testthat::expect_equivalent(dd_vec, c(1,0,0)) # same as global depth
  testthat::expect_equivalent(ldd_vec1, c(1,0,0)) # same as global depth
  testthat::expect_equivalent(ldd_vec2, c(1,0,0)) # works relative to S constraint
  testthat::expect_equivalent(ldd_vec3, c(0,0,0)) # works relative to S constraint (only 1 in each box)
  testthat::expect_equivalent(ldd_vec4, c(0,0,0)) # works relative to S constraint (only 0 in each box)
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_sym <- tidy_dist_mat(dist_mat_not_sym)
  
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_pos <- tidy_dist_mat(dist_mat_not_pos)
  
  
  testthat::expect_error(local_distance_depth_function(tidy_dist_mat_not_sym))
  testthat::expect_error(local_distance_depth_function(tidy_dist_mat_not_pos))
  
})


test_that("test for local_distance_depth_function.tidy_dist_mat (univariate, df_out = T)", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  tidy_dm <- tidy_dist_mat(dist_mat)
  
  
  dd_vec <- local_distance_depth_function(tidy_dm, df_out = T) # c(1,0,0)
  
  ldd_vec1 <- local_distance_depth_function(tidy_dm, tau = 2, df_out = T) # c(1,0,0)
  ldd_vec2 <- local_distance_depth_function(tidy_dm, tau = 1.5, df_out = T) # c(1,0,0)
  ldd_vec3 <- local_distance_depth_function(tidy_dm, tau = 1, df_out = T) # c(0,0,0)
  ldd_vec4 <- local_distance_depth_function(tidy_dm, tau = .1, df_out = T) # c(0,0,0)
  
  testthat::expect_equivalent(dd_vec, data.frame(id = 1:3, depth = c(1,0,0))) # same as global depth
  testthat::expect_equivalent(ldd_vec1, data.frame(id = 1:3, depth = c(1,0,0))) # same as global depth
  testthat::expect_equivalent(ldd_vec2, data.frame(id = 1:3, depth = c(1,0,0))) # works relative to S constraint
  testthat::expect_equivalent(ldd_vec3, data.frame(id = 1:3, depth = c(0,0,0))) # works relative to S constraint (only 1 in each box)
  testthat::expect_equivalent(ldd_vec4, data.frame(id = 1:3, depth = c(0,0,0))) # works relative to S constraint (only 0 in each box)
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_sym <- tidy_dist_mat(dist_mat_not_sym)
  
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_pos <- tidy_dist_mat(dist_mat_not_pos)
  
  
  testthat::expect_error(local_distance_depth_function(tidy_dist_mat_not_sym))
  testthat::expect_error(local_distance_depth_function(tidy_dist_mat_not_pos))
  
})


test_that("test for local_distance_depth_function.tidy_dist_mat (bivariate, df_out = F)", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  rownames_df <- data.frame(id = c(1,2,1),
                            id2 = c("a","a", "b"))
  
  tidy_dm <- tidy_dist_mat(dist_mat, rownames_df, rownames_df)
  

  sol1 <- c(1,0,0)
  names(sol1) <- rownames_df %>% tidyr::unite(col = "names", sep = "|") %>%
    dplyr::pull(.data$names)

  
  dd_vec <- local_distance_depth_function(tidy_dm, df_out = F) # c(1,0,0)
  
  ldd_vec1 <- local_distance_depth_function(tidy_dm, tau = 2, df_out = F) # c(1,0,0)
  ldd_vec2 <- local_distance_depth_function(tidy_dm, tau = 1.5, df_out = F) # c(1,0,0)
  ldd_vec3 <- local_distance_depth_function(tidy_dm, tau = 1, df_out = F) # c(0,0,0)
  ldd_vec4 <- local_distance_depth_function(tidy_dm, tau = .1, df_out = F) # c(0,0,0)
  
  testthat::expect_equal(dd_vec, sol1) # same as global depth
  testthat::expect_equivalent(ldd_vec1, c(1,0,0)) # same as global depth
  testthat::expect_equivalent(ldd_vec2, c(1,0,0)) # works relative to S constraint
  testthat::expect_equivalent(ldd_vec3, c(0,0,0)) # works relative to S constraint (only 1 in each box)
  testthat::expect_equivalent(ldd_vec4, c(0,0,0)) # works relative to S constraint (only 0 in each box)
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_sym <- tidy_dist_mat(dist_mat_not_sym, 
                                         rownames_df, rownames_df)
  
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_pos <- tidy_dist_mat(dist_mat_not_pos, 
                                         rownames_df, rownames_df)
  
  
  testthat::expect_error(local_distance_depth_function(tidy_dist_mat_not_sym))
  testthat::expect_error(local_distance_depth_function(tidy_dist_mat_not_pos))
  
})


test_that("test for local_distance_depth_function.tidy_dist_mat (bivariate, df_out = T)", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  rownames_df <- data.frame(id = c(1,2,1),
                            id2 = c("a","a", "b"))
  
  tidy_dm <- tidy_dist_mat(dist_mat, rownames_df, rownames_df)
  
  
  dd_vec <- local_distance_depth_function(tidy_dm, df_out = T) # c(1,0,0)
  
  ldd_vec1 <- local_distance_depth_function(tidy_dm, tau = 2, df_out = T) # c(1,0,0)
  ldd_vec2 <- local_distance_depth_function(tidy_dm, tau = 1.5, df_out = T) # c(1,0,0)
  ldd_vec3 <- local_distance_depth_function(tidy_dm, tau = 1, df_out = T) # c(0,0,0)
  ldd_vec4 <- local_distance_depth_function(tidy_dm, tau = .1, df_out = T) # c(0,0,0)
  
  testthat::expect_equivalent(dd_vec, rownames_df %>% 
                                dplyr::mutate(depth = c(1,0,0))) # same as global depth
  testthat::expect_equivalent(ldd_vec1, rownames_df %>% 
                                dplyr::mutate(depth = c(1,0,0))) # same as global depth
  testthat::expect_equivalent(ldd_vec2, rownames_df %>% 
                                dplyr::mutate(depth = c(1,0,0))) # works relative to S constraint
  testthat::expect_equivalent(ldd_vec3, rownames_df %>% 
                                dplyr::mutate(depth = c(0,0,0))) # works relative to S constraint (only 1 in each box)
  testthat::expect_equivalent(ldd_vec4, rownames_df %>% 
                                dplyr::mutate(depth = c(0,0,0))) # works relative to S constraint (only 0 in each box)
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_sym <- tidy_dist_mat(dist_mat_not_sym,
                                         rownames_df, rownames_df)
  
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ), nrow = 3, byrow = 3)
  tidy_dist_mat_not_pos <- tidy_dist_mat(dist_mat_not_pos,
                                         rownames_df, rownames_df)
  
  
  testthat::expect_error(local_distance_depth_function(tidy_dist_mat_not_sym))
  testthat::expect_error(local_distance_depth_function(tidy_dist_mat_not_pos))
  
})


test_that("test for local_distance_depth_function (tau percentage)", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  rownames_df2 <- data.frame(id = c(1,2,1),
                            id2 = c("a","a", "b"))
  
   for (df_out in c(T, F)) {
    for (rownames_df in list(NULL, rownames_df2)){
      for (tidy_approach in c(T, F)) {
        if (tidy_approach) {
          dist_info <- tidy_dist_mat(dist_mat, rownames_df, rownames_df)
        } else {
          dist_info <- dist_mat
        }
        
        for (attempts in 1:5){
          percentage <- round(runif(1, min = .1), 5)
          string_percentage <- sprintf("%.3f%%",percentage*100)
          tau <- stats::quantile(as.matrix(dist_info), percentage)
          
          ldd_vec_perc <- local_distance_depth_function(dist_info, 
                                                        tau = string_percentage, 
                                                        df_out = df_out) 
          ldd_vec_tau <- local_distance_depth_function(dist_info, 
                                                       tau = tau, 
                                                       df_out = df_out) 
          
          testthat::expect_equal(ldd_vec_perc, ldd_vec_tau)
        }
        
        
      }
    }
  }


})


test_that("distance_depth_function methods, basic",{
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  # auto and default for matrix return vector
  dd_vec <- distance_depth_function(dist_mat) # c(1,0,0)
  testthat::expect_equal(dd_vec, c(1,0,0))
  
  dd_vec <- distance_depth_function(dist_mat, df_out = F) # c(1,0,0)
  testthat::expect_equal(dd_vec, c(1,0,0))
  
  dd_vec <- distance_depth_function(dist_mat, df_out = "auto") # c(1,0,0)
  testthat::expect_equal(dd_vec, c(1,0,0))
  
  # matrix, df_out = T
  dd_df <- distance_depth_function(dist_mat, df_out = T) # c(1,0,0)
  testthat::expect_equal(dd_df$depth, c(1,0,0))
  testthat::expect_true(inherits(dd_df, "data.frame"))
  testthat::expect_equivalent(dd_df, data.frame(id = 1:3, depth = c(1,0,0)))
  
  # matrix, new rownames , df_out=T
  dist_mat2 <- dist_mat
  rownames(dist_mat2) <- 2:4
  dd_df2 <- distance_depth_function(dist_mat2, df_out = T) # c(1,0,0)
  testthat::expect_equal(dd_df2$depth, c(1,0,0))
  testthat::expect_true(inherits(dd_df2, "data.frame"))
  testthat::expect_equivalent(dd_df2, data.frame(names = as.character(2:4), 
                                                 depth = c(1,0,0)))
  
  # tidy_dm, df_out = T
  tidy_dm <- tidy_dist_mat(dist_mat)
  dd_df_tidy <- distance_depth_function(tidy_dm) # depth = c(1,0,0)
  testthat::expect_equal(dd_df_tidy$depth, c(1,0,0))
  testthat::expect_true(inherits(dd_df_tidy, "data.frame"))
  testthat::expect_equivalent(dd_df_tidy, data.frame(id = 1:3, 
                                                     depth = c(1,0,0)))
  
  # tidy_dm, df_out = F
  tidy_dm <- tidy_dist_mat(dist_mat)
  dd_vec_tidy <- distance_depth_function(tidy_dm, df_out = F) # depth = c(1,0,0)
  testthat::expect_equivalent(dd_vec_tidy, c(1,0,0)) 
  #^slightly different - as naturally named
  
  
})

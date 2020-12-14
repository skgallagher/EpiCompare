context("test for distance-psuedo-density related functions")

test_that("test for distance_psuedo_density_function.matrix, df_out = F", {
  # x only --------------
  inner_data <- data.frame(x = c(-1,0,1))
  dist_mat <- as.matrix(dist(inner_data))
  
  expected_results1 <- dist_mat %>% dnorm() %>% apply(1, mean)
  pd_vec1 <- distance_psuedo_density_function(dist_mat)
  
  expected_results2 <- (dist_mat/2) %>% dnorm() %>% apply(1, mean)
  pd_vec2 <- distance_psuedo_density_function(dist_mat, sigma = 2) 

  testthat::expect_equal(pd_vec1, expected_results1) 
  testthat::expect_equal(pd_vec2, expected_results2) 
  
  # x new ------------------
  pd_vec1_ind <- c()
  pd_vec2_ind <- c()
  
  for (idx in 1:3){
    dist_mat_small <- dist_mat[-idx, -idx]
    dis_mat_x_new <- dist_mat[idx, -idx, drop = F]
    pd_vec1_ind <- c(pd_vec1_ind,
                     distance_psuedo_density_function(x = dist_mat_small, 
                                     x_new = dis_mat_x_new,
                                     df_out = F))
    pd_vec2_ind <- c(pd_vec2_ind,
                     distance_psuedo_density_function(x = dist_mat_small, 
                                                      x_new = dis_mat_x_new,
                                                      df_out = F,
                                                      sigma = 2))
  }
  
  testthat::expect_equal(3/2*(expected_results1 - 1/3*dnorm(0)), pd_vec1_ind ) 
  testthat::expect_equal(3/2*(expected_results2 - 1/3*dnorm(0)), pd_vec2_ind ) 
  
  
})

test_that("test for distance_psuedo_density_function.matrix, df_out = T", {
  # x only -----------------
  inner_data <- data.frame(x = c(-1,0,1))
  dist_mat <- as.matrix(dist(inner_data))
  
  expected_results_vec1 <- dist_mat %>% dnorm() %>% apply(1, mean)
  expected_results1 <- data.frame(names = as.character(1:3), 
                                 psuedo_density = expected_results_vec1)
  pd_vec1 <- distance_psuedo_density_function(dist_mat, df_out = T)
  
  expected_results_vec2 <- (dist_mat/2) %>% dnorm() %>% apply(1, mean)
  expected_results2 <- data.frame(names = as.character(1:3), 
                                 psuedo_density = expected_results_vec2)
  
  pd_vec2 <- distance_psuedo_density_function(dist_mat, sigma = 2, df_out = T) 
  
  testthat::expect_equivalent(pd_vec1, expected_results1) 
  testthat::expect_equivalent(pd_vec2, expected_results2) 
  
  # x new ------------------
  pd_vec1_ind_df <- data.frame()
  pd_vec2_ind_df <- data.frame()
  
  for (idx in 1:3){
    dist_mat_small <- dist_mat[-idx, -idx]
    dis_mat_x_new <- dist_mat[idx, -idx, drop = F]
    pd_vec1_ind_df <- rbind(pd_vec1_ind_df,
                     distance_psuedo_density_function(x = dist_mat_small, 
                                                      x_new = dis_mat_x_new,
                                                      df_out = T))
    pd_vec2_ind_df <- rbind(pd_vec2_ind_df,
                     distance_psuedo_density_function(x = dist_mat_small, 
                                                      x_new = dis_mat_x_new,
                                                      df_out = T,
                                                      sigma = 2))
  }
  
  testthat::expect_equal(
    data.frame(names = as.character(1:3),
               psuedo_density = 3/2*(expected_results1$psuedo_density - 1/3*dnorm(0)),
               row.names = as.character(1:3)), 
    pd_vec1_ind_df) 
  testthat::expect_equal(
    data.frame(names = as.character(1:3),
               psuedo_density = 3/2*(expected_results2$psuedo_density - 1/3*dnorm(0)),
               row.names = as.character(1:3)), 
    pd_vec2_ind_df) 
})



test_that("test for distance_psuedo_density_function.tidy_dist_mat, (univariate) df_out = F", {
  # x only ----------------------
  inner_data <- data.frame(x = c(-1,0,1))
  dist_mat <- as.matrix(dist(inner_data))
  
  tidy_dm <- tidy_dist_mat(dist_mat)
  
  expected_results1 <- dist_mat %>% dnorm() %>% apply(1, mean)
  pd_vec1 <- distance_psuedo_density_function(tidy_dm, df_out = F)
  
  expected_results2 <- (dist_mat/2) %>% dnorm() %>% apply(1, mean)
  pd_vec2 <- distance_psuedo_density_function(tidy_dm, sigma = 2, df_out = F) 
  
  testthat::expect_equal(pd_vec1, expected_results1) 
  testthat::expect_equal(pd_vec2, expected_results2) 
  
  # x new ------------------
  pd_vec1_ind <- c()
  pd_vec2_ind <- c()
  
  for (idx in 1:3){
    dist_mat_small <- tidy_dm[-idx, -idx]
    dis_mat_x_new <- tidy_dm[idx, -idx]
    pd_vec1_ind <- c(pd_vec1_ind,
                     distance_psuedo_density_function(x = dist_mat_small, 
                                                      x_new = dis_mat_x_new,
                                                      df_out = F))
    pd_vec2_ind <- c(pd_vec2_ind,
                     distance_psuedo_density_function(x = dist_mat_small, 
                                                      x_new = dis_mat_x_new,
                                                      df_out = F,
                                                      sigma = 2))
  }
  
  expect_results1.1 <- 3/2*(expected_results1 - 1/3*dnorm(0))
  names(expect_results1.1) <- c("1","2","3")
  expect_results2.1 <- 3/2*(expected_results2 - 1/3*dnorm(0))
  names(expect_results2.1) <- c("1","2","3")
  
  testthat::expect_equal(expect_results1.1, pd_vec1_ind ) 
  testthat::expect_equal(expect_results2.1, pd_vec2_ind ) 
})

test_that("test for distance_psuedo_density_function.tidy_dist_mat, (univariate) df_out = T", {
  # x only ------------------------------
  inner_data <- data.frame(x = c(-1,0,1))
  dist_mat <- as.matrix(dist(inner_data))
  
  tidy_dm <- tidy_dist_mat(dist_mat)
  
  expected_results_vec1 <- dist_mat %>% dnorm() %>% apply(1, mean)
  expected_results1 <- data.frame(id = 1:3, 
                                  psuedo_density = expected_results_vec1)
  pd_vec1 <- distance_psuedo_density_function(tidy_dm, df_out = T)
  
  expected_results_vec2 <- (dist_mat/2) %>% dnorm() %>% apply(1, mean)
  expected_results2 <- data.frame(id = 1:3, 
                                  psuedo_density = expected_results_vec2)
  pd_vec2 <- distance_psuedo_density_function(tidy_dm, sigma = 2, df_out = T) 
  
  testthat::expect_equivalent(pd_vec1, expected_results1) 
  testthat::expect_equivalent(pd_vec2, expected_results2) 
  
  # x new ------------------
  pd_vec1_ind_df <- data.frame()
  pd_vec2_ind_df <- data.frame()
  
  for (idx in 1:3){
    dist_mat_small <- tidy_dm[-idx, -idx]
    dis_mat_x_new <- tidy_dm[idx, -idx]
    pd_vec1_ind_df <- rbind(pd_vec1_ind_df,
                            distance_psuedo_density_function(x = dist_mat_small, 
                                                             x_new = dis_mat_x_new,
                                                             df_out = T))
    pd_vec2_ind_df <- rbind(pd_vec2_ind_df,
                            distance_psuedo_density_function(x = dist_mat_small, 
                                                             x_new = dis_mat_x_new,
                                                             df_out = T,
                                                             sigma = 2))
  }
  
  
  testthat::expect_equal(
    data.frame(id = 1:3,
               psuedo_density = 3/2*(expected_results1$psuedo_density - 1/3*dnorm(0))), 
    pd_vec1_ind_df) 
  testthat::expect_equal(
    data.frame(id = 1:3,
               psuedo_density = 3/2*(expected_results2$psuedo_density - 1/3*dnorm(0))), 
    pd_vec2_ind_df) 
  
})

test_that("test for distance_psuedo_density_function.tidy_dist_mat, (bivariate) df_out = F", {
  # x only ---------------------
  inner_data <- data.frame(x = c(-1, 0, 1))
  dist_mat <- as.matrix(dist(inner_data))
  
  rownames_df <- data.frame(id = c(1, 2, 1),
                            id2 = c("a", "a", "b"))
  
  tidy_dm <- tidy_dist_mat(dist_mat, rownames_df, rownames_df)
  
  expected_results1 <- dist_mat %>% dnorm() %>% apply(1, mean)
  names(expected_results1) <- rownames_df %>% 
    tidyr::unite(col = "names", sep = "|") %>% pull("names")
  pd_vec1 <- distance_psuedo_density_function(tidy_dm, df_out = F)
  
  expected_results2 <- (dist_mat/2) %>% dnorm() %>% apply(1, mean)
  names(expected_results2) <- rownames_df %>% 
    tidyr::unite(col = "names", sep = "|") %>% pull("names")
  pd_vec2 <- distance_psuedo_density_function(tidy_dm, sigma = 2, df_out = F) 
  
  testthat::expect_equal(pd_vec1, expected_results1) 
  testthat::expect_equal(pd_vec2, expected_results2) 
  
  # x new ------------------
  pd_vec1_ind <- c()
  pd_vec2_ind <- c()
  
  for (idx in 1:3){
    dist_mat_small <- tidy_dm[-idx, -idx]
    dis_mat_x_new <- tidy_dm[idx, -idx]
    pd_vec1_ind <- c(pd_vec1_ind,
                     distance_psuedo_density_function(x = dist_mat_small, 
                                                      x_new = dis_mat_x_new,
                                                      df_out = F))
    pd_vec2_ind <- c(pd_vec2_ind,
                     distance_psuedo_density_function(x = dist_mat_small, 
                                                      x_new = dis_mat_x_new,
                                                      df_out = F,
                                                      sigma = 2))
  }
  
  expect_results1.1 <- 3/2*(expected_results1 - 1/3*dnorm(0))
  names(expect_results1.1) <- c("1|a","2|a","1|b")
  expect_results2.1 <- 3/2*(expected_results2 - 1/3*dnorm(0))
  names(expect_results2.1) <- c("1|a","2|a","1|b")
  
  testthat::expect_equal(expect_results1.1, pd_vec1_ind ) 
  testthat::expect_equal(expect_results2.1, pd_vec2_ind ) 
})

test_that("test for distance_psuedo_density_function.tidy_dist_mat, (bivariate) df_out = T", {
  # x only -------------------------
  inner_data <- data.frame(x = c(-1,0,1))
  dist_mat <- as.matrix(dist(inner_data))
  
  rownames_df <- data.frame(id = c(1,2,1),
                            id2 = c("a","a", "b"))
  
  tidy_dm <- tidy_dist_mat(dist_mat, rownames_df, rownames_df)
  
  expected_results_vec1 <- dist_mat %>% dnorm() %>% apply(1, mean)
  expected_results1 <- rownames_df %>% 
    dplyr::mutate(psuedo_density = expected_results_vec1)
  pd_vec1 <- distance_psuedo_density_function(tidy_dm, df_out = T)
  
  expected_results_vec2 <- (dist_mat/2) %>% dnorm() %>% apply(1, mean)
  expected_results2 <- rownames_df %>% 
    dplyr::mutate(psuedo_density = expected_results_vec2)
  pd_vec2 <- distance_psuedo_density_function(tidy_dm, sigma = 2, df_out = T) 
  
  testthat::expect_equivalent(pd_vec1, expected_results1) 
  testthat::expect_equivalent(pd_vec2, expected_results2) 
  
  # x new ------------------
  pd_vec1_ind_df <- data.frame()
  pd_vec2_ind_df <- data.frame()
  
  for (idx in 1:3){
    dist_mat_small <- tidy_dm[-idx, -idx]
    dis_mat_x_new <- tidy_dm[idx, -idx]
    pd_vec1_ind_df <- rbind(pd_vec1_ind_df,
                            distance_psuedo_density_function(x = dist_mat_small, 
                                                             x_new = dis_mat_x_new,
                                                             df_out = T))
    pd_vec2_ind_df <- rbind(pd_vec2_ind_df,
                            distance_psuedo_density_function(x = dist_mat_small, 
                                                             x_new = dis_mat_x_new,
                                                             df_out = T,
                                                             sigma = 2))
  }
  
  
  testthat::expect_equal(
    data.frame(id = c(1,2,1),
               id2 = c("a","a","b"),
               psuedo_density = 3/2*(expected_results1$psuedo_density - 1/3*dnorm(0)),
               row.names = 1:3), 
    pd_vec1_ind_df) 
  testthat::expect_equal(
    data.frame(id = c(1,2,1),
               id2 = c("a","a","b"),
               psuedo_density = 3/2*(expected_results2$psuedo_density - 1/3*dnorm(0)),
               row.names = 1:3), 
    pd_vec2_ind_df) 
})

context("test for distance-psuedo-density related functions")

test_that("test for distance_psuedo_density_function.matrix, df_out = F", {
  inner_data <- data.frame(x = c(-1,0,1))
  dist_mat <- as.matrix(dist(inner_data))
  
  expected_results1 <- dist_mat %>% dnorm() %>% apply(1, mean)
  pd_vec1 <- distance_psuedo_density_function(dist_mat)
  
  expected_results2 <- (dist_mat/2) %>% dnorm() %>% apply(1, mean)
  pd_vec2 <- distance_psuedo_density_function(dist_mat, sigma = 2) 

  testthat::expect_equal(pd_vec1, expected_results1) 
  testthat::expect_equal(pd_vec2, expected_results2) 
})

test_that("test for distance_psuedo_density_function.matrix, df_out = T", {
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
})



test_that("test for distance_psuedo_density_function.tidy_dist_mat, (univariate) df_out = F", {
  inner_data <- data.frame(x = c(-1,0,1))
  dist_mat <- as.matrix(dist(inner_data))
  
  tidy_dm <- tidy_dist_mat(dist_mat)
  
  expected_results1 <- dist_mat %>% dnorm() %>% apply(1, mean)
  pd_vec1 <- distance_psuedo_density_function(tidy_dm, df_out = F)
  
  expected_results2 <- (dist_mat/2) %>% dnorm() %>% apply(1, mean)
  pd_vec2 <- distance_psuedo_density_function(tidy_dm, sigma = 2, df_out = F) 
  
  testthat::expect_equal(pd_vec1, expected_results1) 
  testthat::expect_equal(pd_vec2, expected_results2) 
})

test_that("test for distance_psuedo_density_function.tidy_dist_mat, (univariate) df_out = T", {
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
})


test_that("test for distance_psuedo_density_function.tidy_dist_mat, (bivariate) df_out = F", {
  inner_data <- data.frame(x = c(-1,0,1))
  dist_mat <- as.matrix(dist(inner_data))
  
  rownames_df <- data.frame(id = c(1,2,1),
                            id2 = c("a","a", "b"))
  
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
})


test_that("test for distance_psuedo_density_function.tidy_dist_mat, (bivariate) df_out = T", {
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
})

context("tests for tidy_dist_mat structure")

testthat::test_that("tidy_dist_mat basic checks", {
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = 4:6)
  
  my_tidy_dm <- tidy_dist_mat(my_dist_mat,rownames_df, colnames_df)
  
  # correct structure expected
  testthat::expect_true(
    all(c("rownames_df", "colnames_df") %in% names(attributes(my_tidy_dm))))
  
})

testthat::test_that("check_tidy_dist_mat_dimensions basic", {
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = 4:6)
  
  testthat::expect_true(
    check_tidy_dist_mat_dimensions(my_dist_mat, rownames_df, colnames_df))
  
  rownames_df <- data.frame(id = 1:3, id2 = 4:6)
  
  testthat::expect_true(
    check_tidy_dist_mat_dimensions(my_dist_mat, rownames_df, colnames_df)
  )
  
  # incorrect rownames_df length
  rownames_df <- rownames_df[1:2,]
  testthat::expect_error(
    check_tidy_dist_mat_dimensions(my_dist_mat, rownames_df, colnames_df)
  )
  
  # colnames_df as a vector
  rownames_df <- data.frame(id = 1:3, id2 = 4:6)
  colnames_df <- 1:3
  testthat::expect_error(
    check_tidy_dist_mat_dimensions(my_dist_mat, rownames_df, colnames_df)
  )
  
  # colnames_df as a matrix - but correct form
  colnames_df <- matrix(1:3, ncol = 1)
  testthat::expect_error(
    check_tidy_dist_mat_dimensions(my_dist_mat, rownames_df, colnames_df)
  )
  
  # colnames_df as data.frame, but incorrect form
  colnames_df <- data.frame(matrix(1:3, ncol = 3))
  testthat::expect_error(
    check_tidy_dist_mat_dimensions(my_dist_mat, rownames_df, colnames_df)
  )
  
})

testthat::test_that("check_tidy_dist_names_distinct basic", {
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = 4:6)
  
  testthat::expect_true(
    check_tidy_dist_names_distinct(rownames_df, colnames_df))
  
  # multiple dimensions (row)
  rownames_df <- data.frame(id = c(1,2,1), id2 = 4:6)
  
  testthat::expect_true(
    check_tidy_dist_names_distinct(rownames_df, colnames_df)
  )
  
  # multiple dimensions (both)
  colnames_df <- data.frame(id = c(1,2,1), id2 = 4:6)
  
  testthat::expect_true(
    check_tidy_dist_names_distinct(rownames_df, colnames_df)
  )
  
  
  
  # 1d row error
  rownames_df <- data.frame(id = c(1,2,1))
  testthat::expect_error(
    check_tidy_dist_names_distinct(rownames_df, colnames_df)
  )
  
  # 1d column error
  rownames_df <- data.frame(id = c(1,2,1))
  testthat::expect_error(
    check_tidy_dist_names_distinct(colnames_df, rownames_df)
  )
  
  # 2d row error
  rownames_df <- data.frame(id = c(1,2,1), id2 = rep(2,3))
  testthat::expect_error(
    check_tidy_dist_names_distinct(rownames_df, colnames_df)
  )
  
  # 2d column error
  rownames_df <- data.frame(id = c(1,2,1), id2 = rep(2,3))
  testthat::expect_error(
    check_tidy_dist_names_distinct(colnames_df, rownames_df)
  )
  
  
})


testthat::test_that(paste("dimnames.tidy_dist_mat basic checks (rownames and",
                          "colnames as well - and assignment)"), {
                            inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
                            my_dist_mat <- as.matrix(dist(inner_data)) 
                            rownames_df <- data.frame(id = 1:3)
                            colnames_df <- data.frame(id = 4:6)
                            
                            my_tidy_dm <- tidy_dist_mat(my_dist_mat,rownames_df, colnames_df)
                            
                            # correct structure expected (dimnames)
                            testthat::expect_identical(dimnames(my_tidy_dm), 
                                                       list("rownames" = rownames_df, 
                                                            "colnames" = colnames_df))
                            
                            # correct structure expected (rownames and colnames)
                            testthat::expect_identical(rownames(my_tidy_dm) , rownames_df)
                            testthat::expect_identical(colnames(my_tidy_dm) , colnames_df)
                            
                            # dimname assignment
                            dimnames(my_tidy_dm) <- list(data.frame(id = 2:4), 
                                                         data.frame(id = (-4):(-2)))
                            testthat::expect_identical(rownames(my_tidy_dm), data.frame(id = 2:4))
                            testthat::expect_identical(colnames(my_tidy_dm), data.frame(id = (-4):(-2)))
                            
                            # rowname assignment
                            my_tidy_dm <- tidy_dist_mat(my_dist_mat,rownames_df, colnames_df)
                            
                            rownames(my_tidy_dm) <- data.frame(id = 2:4)
                            testthat::expect_identical(dimnames(my_tidy_dm), 
                                                       list("rownames" = data.frame(id = 2:4),
                                                            "colnames" = colnames_df))
                            
                            
                            # colname assignment
                            my_tidy_dm <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
                            
                            colnames(my_tidy_dm) <- data.frame(id = 2:4)
                            testthat::expect_identical(dimnames(my_tidy_dm), 
                                                       list("rownames" = rownames_df,
                                                            "colnames" = data.frame(id = 2:4)))
                            
                            
                          })


testthat::test_that("format.tidy_dist_mat check, static", {
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data))
  
  # both one dimensional
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = c(1,2,4))
  
  my_tidy_dm <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  m <- format(my_tidy_dm)
  testthat::expect_true(inherits(m, "noquote"))
  testthat::expect_equal(dim(m), c(5,5))
  
  # colnames multidimensional
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = c(1,2,1), id2 = c("f", "f", "s"))
  
  my_tidy_dm <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  m <- format(my_tidy_dm)
  testthat::expect_true(inherits(m, "noquote"))
  testthat::expect_equal(dim(m), c(6,5))
  
  # rownames multidimensional
  colnames_df <- data.frame(id = 1:3)
  rownames_df <- data.frame(id = c(1,2,1), id2 = c("f", "f", "s"))
  my_tidy_dm <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  m <- format(my_tidy_dm)
  testthat::expect_true(inherits(m, "noquote"))
  testthat::expect_equal(dim(m), c(5,6))
  
  # both multidimensional
  rownames_df <- data.frame(id = c(1,2,1), id2 = c("f", "f", "s"))
  colnames_df <- rownames_df
  my_tidy_dm <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  m <- format(my_tidy_dm)
  testthat::expect_true(inherits(m, "noquote"))
  testthat::expect_equal(dim(m), c(6,6))
  
})

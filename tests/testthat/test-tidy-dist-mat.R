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
  
  # no names_dfs
  my_tidy_dm <- tidy_dist_mat(my_dist_mat)
  testthat::expect_equal(dim(rownames(my_tidy_dm)), c(3,1))
  testthat::expect_equal(rownames(my_tidy_dm), data.frame(id = 1:3))
  
  testthat::expect_equal(dim(colnames(my_tidy_dm)), c(3,1))
  testthat::expect_equal(colnames(my_tidy_dm), data.frame(id = 1:3))

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

  # digits
  my_dist_mat2 <- matrix(c(0,1,2.1245,
                           1,0,2,
                           2.1245,2,0), byrow = T, ncol = 3)
  
  my_tdm2 <- tidy_dist_mat(my_dist_mat2, rownames_df, rownames_df)
  
  format3digit <- format(my_tdm2, digits = 3)
  testthat::expect_equal(unclass(format3digit)[4,6], "2.12")
  
  # size 
  my_dist_mat2 <- matrix(c(0,1,2.1245,
                           1,0,2,
                           2.1245,2,0), byrow = T, ncol = 3)
  
  my_tdm2 <- tidy_dist_mat(my_dist_mat2, rownames_df, rownames_df)
  
  ## row drop alter
  format2_3 <- format(my_tdm2[1:2,1:3], more_rows = T)
  testthat::expect_equal(format2_3[1:(nrow(format2_3)-1),], 
                         format(my_tdm2)[1:(nrow(format2_3)-1),])
  testthat::expect_equivalent(format2_3[nrow(format2_3),,drop = F],
                              noquote(matrix(rep("...",6), ncol = 6)))
  
  ## col drop alter
  format3_2 <- format(my_tdm2[1:3,1:2], more_cols = T)
  testthat::expect_equal(format3_2[,1:(ncol(format3_2)-1)], 
                         format(my_tdm2)[,1:(ncol(format3_2)-1)])
  testthat::expect_equivalent(format3_2[,ncol(format3_2),drop = F],
                              noquote(matrix(rep("...",6), ncol = 1)))
  ## row and column drop alter
  format2_2 <- format(my_tdm2[1:2,1:2], more_cols = T, more_rows = T)
  testthat::expect_equal(format2_2[1:(nrow(format2_3)-1),1:(ncol(format2_3)-1)], 
                         format(my_tdm2)[1:(nrow(format2_2)-1),1:(ncol(format2_3)-1)])
  testthat::expect_equivalent(format2_2[,ncol(format2_2),drop = F],
                              noquote(matrix(rep("...",6), ncol = 1)))
  testthat::expect_equivalent(format2_2[nrow(format2_2),,drop = F],
                              noquote(matrix(rep("...",6), ncol = 6)))
  
})

testthat::test_that("test print.tidy_dist_mat, basic", {
  # this is a dumb test - but meh
  inner_data <- data.frame(x = 1:3)
  my_dist_mat <- as.matrix(dist(inner_data))
  
  # both one dimensional
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = c(1,2,4))
  
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  default_digits <- 6
  for (option in c("id", "\\nid", 
                  paste0("-  \\+ ", 
                         paste0(rep("-", default_digits+1), collapse = ""),
                         " ",
                         paste0(rep("-", default_digits+1), collapse = ""),
                         " ",
                         paste0(rep("-", default_digits+1), collapse = "")),
                  paste0("1  | 0", 
                         paste0(rep(" ",default_digits-1), collapse = ""),
                         "1",
                         paste0(rep(" ",default_digits-1), collapse = ""),
                         "2"),
                  paste0("2  | 1", 
                         paste0(rep(" ",default_digits-1), collapse = ""),
                         "0",
                         paste0(rep(" ",default_digits-1), collapse = ""),
                         "1"),
                  paste0("3  | 2", 
                         paste0(rep(" ",default_digits-1), collapse = ""),
                         "1",
                         paste0(rep(" ",default_digits-1), collapse = ""),
                         "0"))){
  testthat::expect_output(print(td_mat),
                          regexp = option)
  testthat::expect_output(print(td_mat, n = 100), # pointless n - rest in format
                            regexp = option)
  }
                          
})

testthat::test_that("rownames and colname assignment for tidy_dist_mat",{
  my_dist_mat2 <- matrix(c(0,1,2.1245,
                           1,0,2,
                           2.1245,2,0), byrow = T, ncol = 3)
  
  rownames_df <- data.frame(id = c(1,2,1), id2 = c("f", "f", "s"))
  colnames_df <- rownames_df
  
  my_tdm2 <- tidy_dist_mat(my_dist_mat2, rownames_df, rownames_df)
  
  rownames(my_tdm2) <- data.frame(id = 1:3)
  testthat::expect_equal(attr(my_tdm2, "rownames_df"), data.frame(id = 1:3))
  testthat::expect_equal(rownames(my_tdm2), data.frame(id = 1:3))
  testthat::expect_equal(attr(my_tdm2, "colnames_df"), colnames_df)
  testthat::expect_equal(colnames(my_tdm2), colnames_df)
  
  colnames(my_tdm2) <- data.frame(id = 2:4)
  testthat::expect_equal(attr(my_tdm2, "rownames_df"), data.frame(id = 1:3))
  testthat::expect_equal(rownames(my_tdm2), data.frame(id = 1:3))
  testthat::expect_equal(attr(my_tdm2, "colnames_df"), data.frame(id = 2:4))
  testthat::expect_equal(colnames(my_tdm2), data.frame(id = 2:4))
  
})

testthat::test_that("test is.tidy_dist_mat basic", {
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data))
  
  # both one dimensional
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = c(1,2,4))
  
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  testthat::expect_true(is.tidy_dist_mat(td_mat))
  testthat::expect_false(is.tidy_dist_mat(my_dist_mat))
  testthat::expect_false(is.tidy_dist_mat(inner_data))
})

testthat::test_that("test which_index.tidy_dist_mat", {
  # single columns
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = 4:6)
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  # row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
   output_index <- which_index(td_mat, data.frame(id = index), margin = 1)
   testthat::expect_equal(index, output_index)
  }
  # col
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_index(td_mat, data.frame(id = index + 3), margin = 2)
    testthat::expect_equal(index, output_index)
  }

  # multiple columns
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3,
                            id2 = rep("a",3))
  colnames_df <- data.frame(id = 4:6,
                            id2 = rep("b", 3))
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  # row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_index(td_mat, 
                                data.frame(id = index,
                                           id2 = rep("a",length(index))), 
                                margin = 1)
    testthat::expect_equal(index, output_index)
  }
  # col
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_index(td_mat, 
                                data.frame(id = index + 3,
                                           id2 = rep("b",length(index))),
                                margin = 2)
    testthat::expect_equal(index, output_index)
  }
  
  # errors (naming) -----
  
  # row and row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    testthat::expect_error(which_index(td_mat, 
                                data.frame(id = index), 
                                margin = 1))
    testthat::expect_error(which_index(td_mat, 
                                data.frame(id = index + 3),
                                margin = 2))
  }

  
})

testthat::test_that("test which_index.tidy_dist_mat (not)", {
  # single columns
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = 4:6)
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  # row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_index(td_mat, not(data.frame(id = index)), margin = 1)
    testthat::expect_equal(c(1:3)[!(c(1:3) %in% index)], output_index)
  }
  # col
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_index(td_mat, not(data.frame(id = index + 3)), margin = 2)
    testthat::expect_equal(c(1:3)[!(c(1:3) %in% index)], output_index)
  }
  
  # multiple columns
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3,
                            id2 = rep("a",3))
  colnames_df <- data.frame(id = 4:6,
                            id2 = rep("b", 3))
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  # row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_index(td_mat, 
                                not(data.frame(id = index,
                                           id2 = rep("a",length(index)))), 
                                margin = 1)
    testthat::expect_equal(c(1:3)[!(c(1:3) %in% index)], output_index)
  }
  # col
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_index(td_mat, 
                                not(data.frame(id = index + 3,
                                           id2 = rep("b",length(index)))),
                                margin = 2)
    testthat::expect_equal(c(1:3)[!(c(1:3) %in% index)], output_index)
  }
  
  # errors (naming) -----
  
  # row and row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    testthat::expect_error(which_index(td_mat, 
                                       not(data.frame(id = index)), 
                                       margin = 1))
    testthat::expect_error(which_index(td_mat, 
                                       not(data.frame(id = index + 3)),
                                       margin = 2))
  }
  
  
})

testthat::test_that("test which_not_index.tidy_dist_mat", {
  # single columns
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = 4:6)
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  # row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_not_index(td_mat, data.frame(id = index), margin = 1)
    testthat::expect_equal(c(1:3)[!(c(1:3) %in% index)], output_index)
  }
  # col
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_not_index(td_mat, data.frame(id = index + 3), margin = 2)
    testthat::expect_equal(c(1:3)[!(c(1:3) %in% index)], output_index)
  }
  
  # multiple columns
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3,
                            id2 = rep("a",3))
  colnames_df <- data.frame(id = 4:6,
                            id2 = rep("b", 3))
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  # row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_not_index(td_mat, 
                                data.frame(id = index,
                                           id2 = rep("a",length(index))), 
                                margin = 1)
    testthat::expect_equal(c(1:3)[!(c(1:3) %in% index)], output_index)
  }
  # col
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_not_index(td_mat, 
                                data.frame(id = index + 3,
                                           id2 = rep("b",length(index))),
                                margin = 2)
    testthat::expect_equal(c(1:3)[!(c(1:3) %in% index)], output_index)
  }
  
  # errors (naming) -----
  
  # row and row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    testthat::expect_error(which_not_index(td_mat, 
                                       data.frame(id = index), 
                                       margin = 1))
    testthat::expect_error(which_not_index(td_mat, 
                                       data.frame(id = index + 3),
                                       margin = 2))
  }
  
  
})

testthat::test_that("test which_not_index.tidy_dist_mat (not)", {
  # single columns
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3)
  colnames_df <- data.frame(id = 4:6)
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  # row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_not_index(td_mat, not(data.frame(id = index)), margin = 1)
    testthat::expect_equal(index, output_index)
  }
  # col
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_not_index(td_mat, not(data.frame(id = index + 3)), margin = 2)
    testthat::expect_equal(index, output_index)
  }
  
  # multiple columns
  inner_data <- data.frame(x = rnorm(3), y = rnorm(3))
  my_dist_mat <- as.matrix(dist(inner_data)) 
  rownames_df <- data.frame(id = 1:3,
                            id2 = rep("a",3))
  colnames_df <- data.frame(id = 4:6,
                            id2 = rep("b", 3))
  td_mat <- tidy_dist_mat(my_dist_mat, rownames_df, colnames_df)
  
  # row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_not_index(td_mat, 
                                not(data.frame(id = index,
                                               id2 = rep("a",length(index)))), 
                                margin = 1)
    testthat::expect_equal(index, output_index)
  }
  # col
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    output_index <- which_not_index(td_mat, 
                                not(data.frame(id = index + 3,
                                               id2 = rep("b",length(index)))),
                                margin = 2)
    testthat::expect_equal(index, output_index)
  }
  
  # errors (naming) -----
  
  # row and row
  for(index in list(1, 2, 3, 1:2, 2:3, c(1,3), 1:3)){
    testthat::expect_error(which_not_index(td_mat, 
                                       not(data.frame(id = index)), 
                                       margin = 1))
    testthat::expect_error(which_not_index(td_mat, 
                                       not(data.frame(id = index + 3)),
                                       margin = 2))
  }
  
  
})


testthat::test_that("[.tidy_dist_mat single column", {
  x_mat <- as.matrix(dist(rnorm(5))) 
  x <- x_mat %>%
    tidy_dist_mat()
  
  # index integer ------------
  testthat::expect_equal(x[1:3], x_mat[1:3,1:3] %>% tidy_dist_mat())
  testthat::expect_equal(x[2:4], x_mat[2:4,2:4] %>% 
                           tidy_dist_mat(rownames_df = data.frame(id = 2:4),
                                         colnames_df =  data.frame(id = 2:4)))
  testthat::expect_equal(x[2:3,5], x_mat[2:3,5, drop = F] %>% 
                           tidy_dist_mat(rownames_df = data.frame(id = 2:3),
                                         colnames_df = data.frame(id = 5)))
  testthat::expect_equal(x[5,2:3], x_mat[5,2:3, drop = F] %>% 
                           tidy_dist_mat(rownames_df = data.frame(id = 5),
                                         colnames_df = data.frame(id = 2:3)))
  
  testthat::expect_equal(x[-5,2:3], x_mat[-5,2:3, drop = F] %>% 
                           tidy_dist_mat(rownames_df = data.frame(id = 1:4),
                                         colnames_df = data.frame(id = 2:3)))
  
  # index data.frame  ------------
  testthat::expect_equal(x[data.frame(id = 1:3)], x_mat[1:3,1:3] %>% tidy_dist_mat())
  testthat::expect_equal(x[data.frame(id = 2:4)], x_mat[2:4,2:4] %>% 
                           tidy_dist_mat(rownames_df = data.frame(id = 2:4),
                                         colnames_df =  data.frame(id = 2:4)))
  testthat::expect_equal(x[data.frame(id = 2:3),data.frame(id = 5)], x_mat[2:3,5, drop = F] %>% 
                           tidy_dist_mat(rownames_df = data.frame(id = 2:3),
                                         colnames_df = data.frame(id = 5)))
  testthat::expect_equal(x[data.frame(id = 5),data.frame(id = 2:3)], x_mat[5,2:3, drop = F] %>% 
                           tidy_dist_mat(rownames_df = data.frame(id = 5),
                                         colnames_df = data.frame(id = 2:3)))
  
  testthat::expect_equal(x[not(data.frame(id = 5)),data.frame(id = 2:3)], x_mat[-5,2:3, drop = F] %>% 
                           tidy_dist_mat(rownames_df = data.frame(id = 1:4),
                                         colnames_df = data.frame(id = 2:3)))
})

testthat::test_that("[.tidy_dist_mat multiple columns", {
  x_mat <- as.matrix(dist(rnorm(5))) 
  rownames_df <- data.frame(id = rep(1:2, length.out = 5),
                            id2 = rep(c("a","b","c"), each = 2)[1:5]) %>%
    tibble()
  
  x <- x_mat %>%
    tidy_dist_mat(rownames_df, rownames_df)
  
  # index integer ------------
  testthat::expect_equivalent(x[1:3], (x_mat[1:3,1:3] %>% 
                           tidy_dist_mat(rownames_df[1:3,],
                                         rownames_df[1:3,])))
  testthat::expect_equivalent(x[2:4], x_mat[2:4,2:4] %>% 
                           tidy_dist_mat(rownames_df = rownames_df[2:4,],
                                         colnames_df =  rownames_df[2:4,]))
  testthat::expect_equivalent(x[2:3,5], x_mat[2:3,5, drop = F] %>% 
                           tidy_dist_mat(rownames_df = rownames_df[2:3,],
                                         colnames_df =rownames_df[5,]))
  testthat::expect_equivalent(x[5,2:3], x_mat[5,2:3, drop = F] %>% 
                           tidy_dist_mat(rownames_df = rownames_df[5,],
                                         colnames_df = rownames_df[2:3,]))
  
  testthat::expect_equivalent(x[-5,2:3], x_mat[-5,2:3, drop = F] %>% 
                           tidy_dist_mat(rownames_df = rownames_df[1:4,],
                                         colnames_df = rownames_df[2:3,]))
  
  # index data.frame  ------------
  testthat::expect_equivalent(x[rownames_df[1:3,]], (x_mat[1:3,1:3] %>% 
                                         tidy_dist_mat(rownames_df[1:3,],
                                                       rownames_df[1:3,])))
  testthat::expect_equivalent(x[rownames_df[2:4,]], x_mat[2:4,2:4] %>% 
                                tidy_dist_mat(rownames_df = rownames_df[2:4,],
                                              colnames_df =  rownames_df[2:4,]))
  testthat::expect_equivalent(x[rownames_df[2:3,],5], x_mat[2:3,5, drop = F] %>% 
                                tidy_dist_mat(rownames_df = rownames_df[2:3,],
                                              colnames_df =rownames_df[5,]))
  testthat::expect_equivalent(x[rownames_df[2:3,],rownames_df[5,]], x_mat[2:3,5, drop = F] %>% 
                                tidy_dist_mat(rownames_df = rownames_df[2:3,],
                                              colnames_df =rownames_df[5,]))
  testthat::expect_equivalent(x[rownames_df[5,],rownames_df[2:3,]], 
                              x_mat[5,2:3, drop = F] %>% 
                                tidy_dist_mat(rownames_df = rownames_df[5,],
                                              colnames_df = rownames_df[2:3,]))
  
  testthat::expect_equivalent(x[not(rownames_df[5,]),rownames_df[2:3,]], 
                              x_mat[-5,2:3, drop = F] %>% 
                                tidy_dist_mat(rownames_df = rownames_df[1:4,],
                                              colnames_df = rownames_df[2:3,]))
})

testthat::test_that("test head.tidy_dist_mat, basic", {
  x_mat <- as.matrix(dist(rnorm(10))) 
  x <- x_mat %>%
    tidy_dist_mat()
  for (i in 1:8){
    testthat::expect_equal(head(x, n = i), x[1:i])
  }
})

testthat::test_that("test tail.tidy_dist_mat, basic", {
  x_mat <- as.matrix(dist(rnorm(10))) 
  x <- x_mat %>%
    tidy_dist_mat()
  for (i in 1:10){
    testthat::expect_equal(tail(x, n = i), x[(10-i):10])
  }
})



testthat::test_that("test not_df functions: creation, not(), reverse(), is.not_df()", {
  df <- data.frame(x = rnorm(100),
                   y = rnorm(100))
  
  testthat::expect_false(is.not_df(df))
  not_df_object2 <- not(df)
  testthat::expect_true(is.not_df(not_df_object2))
  
  testthat::expect_false(is.not_df(reverse_not_df(not_df_object2)))
  testthat::expect_error(reverse_not_df(df))
})




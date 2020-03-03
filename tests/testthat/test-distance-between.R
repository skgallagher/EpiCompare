context("tests for functions to check distance between elements")

test_that("dist_along_path basic checks of angle and distance", {
  # basic example 1:
  data_df <- data.frame(x = c(0,1), y = c(0,1))
  out_info <- dist_along_path(data_df)

  testthat::expect_equal(out_info$distance, sqrt(2))
  testthat::expect_equal(out_info$angle, 45*pi/180)

  # basic example 2:
  data_df <- data.frame(x = c(0,1), y = c(0,sqrt(3)))
  out_info <- dist_along_path(data_df)

  testthat::expect_equal(out_info$distance, 2)
  testthat::expect_equal(out_info$angle, 60*pi/180)

  # basic example 3:
  data_df <- data.frame(x = c(0,1,2), y = c(0,1,1+sqrt(3)))
  out_info <- dist_along_path(data_df)

  testthat::expect_equal(out_info$distance, c(sqrt(2),2))
  testthat::expect_equal(out_info$angle, c(45,60)*pi/180)
})

test_that("step_along basic", {
  # basic example 1:
  data_df <- data.frame(x = c(0,1), y = c(0,1))
  new_point <- step_along(data_df[1,],
                          angle = 45*pi/180,
                          distance = sqrt(2))

  testthat::expect_true(all.equal(as.numeric(new_point[1]),
                                  1, tolerance = 1.5e-8))
  testthat::expect_true(all.equal(as.numeric(new_point[2]),
                                  1, tolerance = 1.5e-8))

  # basic example 2:
  data_df <- data.frame(x = c(0,1), y = c(0,sqrt(3)))
  new_point <- step_along(data_df[1,],
                          angle = 60*pi/180,
                          distance = 2)

  testthat::expect_true(all.equal(as.numeric(new_point[1]),
                                  1, tolerance = 1.5e-8))
  testthat::expect_true(all.equal(as.numeric(new_point[2]),
                                  sqrt(3), tolerance = 1.5e-8))

})

test_that("basic tests for equa_dist_points",{
  #straight line
  my_df <- data.frame(x = 1:20) %>%
    dplyr::mutate(y = x)

  for (num_splits in sample(5:25,size = 5)){
    my_df_compression <- equa_dist_points(my_df, num_splits = num_splits)

    testthat::expect_equal(nrow(my_df_compression), num_splits)

    testthat::expect_true(
      all.equal(diff(my_df_compression$x),
                rep(19/(num_splits - 1), num_splits-1)
      ))
    testthat::expect_equal(my_df_compression$x, my_df_compression$y)
  }

  # up then down line
  my_df <- data.frame(x = c(1:20)) %>%
    dplyr::mutate(y = abs(10-x))

  for (num_splits in sample(7:25,size = 5)){
    my_df_compression <- equa_dist_points(my_df, num_splits = num_splits)

    # ignore middle values
    down_df <- my_df_compression[1:(floor(num_splits/2)-1),]

    out <- dist_along_path(down_df)
    testthat::expect_equal(out$distance,  rep(19/(num_splits - 1)*sqrt(2),
                                              floor(num_splits/2) -2))

    testthat::expect_equal(out$angle, rep(-45*pi/180, floor(num_splits/2) -2))

    up_df <-  my_df_compression[(ceiling(num_splits/2)+1):num_splits,]
    out <- dist_along_path(up_df)
    testthat::expect_equal(out$angle,
                           rep(45*pi/180,
                               length((ceiling(num_splits/2)+1):num_splits) - 1))

    testthat::expect_equal(out$distance,
                           rep(19/(num_splits - 1)*sqrt(2),
                               length((ceiling(num_splits/2)+1):num_splits) -1))
  }

})




test_that("test equa_dist_points_listable", {

  my_list <- lapply(5:7,function(c) data.frame(x = 1:c) %>%
                      dplyr::mutate(y = x))
  for (num_splits in sample(5:25,size = 5)){
    updated <- my_list %>% equa_dist_points_listable(verbose = F,
                                                     num_splits = num_splits)
    testthat::expect_true(all(sapply(updated, nrow) == num_splits))


  }

  my_list2 <- lapply(5:7,function(c) data.frame(a = 1, b = 2, x = 1:c) %>%
                       dplyr::mutate(y = x))
  for (num_splits in sample(5:25,size = 5)){
    updated2 <- my_list2 %>% equa_dist_points_listable(verbose = F,
                                                       position = 3:4,
                                                       num_splits = num_splits)
    updated <- my_list %>% equa_dist_points_listable(verbose = F,
                                                     num_splits = num_splits)
    testthat::expect_true(all(sapply(updated2, nrow) == num_splits))
    testthat::expect_equal(updated, updated2)


  }
})


test_that("test dist_matrix_innersq basic checks", {
  function_list <- list(function(x) x^2, function(x) sqrt(x), function(x) x)

  my_list <- lapply(function_list, function(f) {
    data.frame(x = 1:20,
               y = f(1:20))
  })
  my_dist <- dist_matrix_innersq(my_list, position = 1:2)

  testthat::expect_equal(t(my_dist), my_dist)
  testthat::expect_true(all(diag(my_dist) == 0))
  testthat::expect_true(all(my_dist >= 0))
})



test_that("distance_between_path tests",{
  # against the same df
  my_df <- data.frame(x = rnorm(20), y = rnorm(20))

  testthat::expect_true(all(dist_between_paths(my_df, my_df) == 0))

  #against another df
  my_df <- data.frame(x = 1:20) %>% dplyr::mutate(y = x)
  my_df2 <- data.frame(x = 1:20) %>% dplyr::mutate(y = abs(10 - x))

  testthat::expect_true(all(dist_between_paths(my_df, my_df2)[10:20] == 10))
  testthat::expect_true(all(
    dist_between_paths(my_df, my_df2)[1:10] == 2*c(4:0, 1:5))
  )
})

test_that("get_xy_coord tests", {
  # basic test
  df = data.frame(x = c(1,0,0),
                  y = c(0,1,0),
                  z = c(0,0,1))

  df2d <- get_xy_coord(df, xyz_col = c("x","y","z"))
  df2d_expected <- data.frame(x = c(0, .5        ,1),
                              y = c(0, .5*sqrt(3),0))
  testthat::expect_equivalent(df2d, df2d_expected)
})

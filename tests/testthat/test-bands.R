context("tests for band visuals")

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



test_that("get_closest static tests",{
  # single point:
  border_points <- data.frame(x = 0, y = 0)
  inner_points <- border_points[0,]
  xrange <- seq(-5,5, length.out = 11)
  yrange <- xrange
  delta <- 1.1 # only get 4 points (diagonal points not included)
  check <- get_closest(border_points, inner_points, delta, xrange, yrange)

  close_to_point <- data.frame(x = c(1,0,0,0,-1),
                               y = c(0,1,0,-1,0),
                               check = 2)
  testthat::expect_true(
    all((check %>% dplyr::filter(.data$z == 2) %>%
           dplyr::left_join(close_to_point, by = c("x", "y")) %>%
           dplyr::pull(.data$check) %>% is.na %>% sum) == 0))
  testthat::expect_true(all(table(check$z) == c(116, 5)))


  # small square box:
  border_points <- data.frame(x = c(-1,0,1,
                                    -1,  1,
                                    -1,0,1),
                              y = c(1,1,1,
                                    0,  0,
                                    -1,-1,-1))
  inner_points <- data.frame(x = 0, y = 0)
  xrange <- seq(-5,5, length.out = 11) + .01
  yrange <- xrange
  delta <- .5 # only points that basically the same points
  check <- get_closest(border_points, inner_points, delta, xrange, yrange)


  testthat::expect_true(all(check %>%
                              dplyr::filter((.data$x > 2.1 | .data$x < -1.9) |
                                            (.data$y > 2.1 | .data$y < -1.9)) %>%
                              dplyr::pull(.data$z) == 1))

  testthat::expect_true(
    all(check %>%
          dplyr::filter((.data$x == 1.01 & .data$y %in% c(1.01, .01, -.99))  |
                        (.data$x == -0.99 & .data$y %in% c(1.01, .01, -.99)) |
                        (.data$y == 1.01 & .data$x %in% c(1.01, .01, -.99))  |
                        (.data$y == -0.99 & .data$x %in% c(1.01, .01, -.99))) %>%
          dplyr::pull(.data$z) == 2))

  testthat::expect_true(
    all(check %>% dplyr::filter(.data$x == 0.01, .data$y == .01) %>%
          dplyr::pull(.data$z) == 3))

  testthat::expect_true(all(table(check$z) == c(11^2-8-1, 8,1)))

  # large square box:
  border_points <- data.frame(x = c(-2,-1,0,1,2,
                                    -2,       2,
                                    -2,       2,
                                    -2,       2,
                                    -2,-1,0,1,2),
                              y = c(2,2,2,2,2,
                                    1,      1,
                                    0,      0,
                                    -1,    -1,
                                    -2,-2,-2,-2,-2))
  inner_points <- data.frame(x = c(-1,0,1,
                                   -1,0,1,
                                   -1,0,1),
                             y = c(1,1,1,
                                   0,0,0,
                                   -1,-1,-1))
  xrange <- seq(-5,5, length.out = 11) + .01
  yrange <- xrange
  delta <- .5 # only get 4 points (diagonal points not included)
  check <- get_closest(border_points, inner_points, delta = delta,
                       xrange = xrange,
                       yrange = yrange)

  testthat::expect_true(all(check %>%
                              dplyr::filter((.data$x > 3.1 | .data$x < -2.9) |
                                            (.data$y > 3.1 | .data$y < -2.9)) %>%
                              dplyr::pull(.data$z) == 1))

  testthat::expect_true(
    all(check %>%
          dplyr::filter((.data$x == 2.01 & .data$y %in% c(2.01, 1.01, .01, -.99, -1.99))  |
                        (.data$x == -1.99 & .data$y %in% c(2.01, 1.01, .01, -.99, -1.99)) |
                        (.data$y == 2.01 & .data$x %in% c(2.01, 1.01, .01, -.99, -1.99))  |
                        (.data$y == -1.99 & .data$x %in% c(2.01, 1.01, .01, -.99, -1.99))) %>%
          dplyr::pull(.data$z) == 2))

  testthat::expect_true(
    all(check %>%
          dplyr::filter((.data$x == 1.01 & .data$y %in% c( 1.01, .01, -.99))  |
                        (.data$x == .01 & .data$y %in% c( 1.01, .01, -.99)) |
                        (.data$x == -.99 & .data$y %in% c( 1.01, .01, -.99)) ) %>%
          dplyr::pull(.data$z) == 3))


  # create gridpoints
  border_points <- data.frame(x = c(-2,2,
                                    -2,2),
                              y = c(2,2,
                                    -2,-2))
  inner_points <- border_points[0,]
  delta = .5
  check <- get_closest(border_points, inner_points, delta = delta,
                       gridbreaks = 5)
  # no "inside"
  testthat::expect_true(all(check$z != 3))

  border_points_check <- check %>%
    dplyr::inner_join(border_points, by = c("x","y"))
  testthat::expect_true(all(dim(border_points_check) == c(4,3)))
  testthat::expect_true(all(border_points_check$z == 2))
  testthat::expect_equivalent(table(check$z), table(c(rep(1,21), rep(2,4))))
})


test_that("get_delta basic tests", {
  # static
  data <- data.frame(x = c(0,1), y = c(0,1))
  dist_mat <- matrix(c(0,sqrt(2),
                       sqrt(2),0), byrow = T, nrow = 2)
  gd_out1 <- get_delta(data)
  gd_out2 <- get_delta(dist_mat = dist_mat)

  testthat::expect_equivalent(gd_out1,gd_out2)
  testthat::expect_equivalent(gd_out1, list("dist_mat" = dist_mat,
                                            "mm_delta" = sqrt(2)))
  # static 2
  data2 <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  dist_mat2 <- matrix(c(0,1,sqrt(2),1,
                        1,0,1,sqrt(2),
                        sqrt(2),1,0,1,
                        1,sqrt(2),1,0), byrow = T, nrow = 4)
  gd_out3 <- get_delta(data2)
  gd_out4 <- get_delta(dist_mat = dist_mat2)

  testthat::expect_equivalent(gd_out3, gd_out4)
  testthat::expect_equivalent(gd_out3, list("dist_mat" = dist_mat2,
                                            "mm_delta" = 1))

})

test_that("delta_structure tests", {
  data_deep_points <- data.frame(lat = c(0,1,1),
                                 long = c(0,0,1))

  out_list <- delta_structure(data_deep_points)
  structure_out_list <- out_list$structure
  line_numbers <- structure_out_list %>% dplyr::pull(idx) %>% unique

  expected_min_delta <- 1
  expected_structure <- list(first = data.frame(long = c(0,1),
                                                lat = c(0,0),
                                                extra = 1:2),
                             second = data.frame(long = c(1, 1),
                                                 lat = c(0, 1),
                                                 extra = 1:2))

  line_info <- c()
  for (line in expected_structure) {
    in_the_match <- FALSE
    for (line_num in line_numbers) {
      d_structure <- structure_out_list %>%
        dplyr::filter(idx == line_num)

      combined_info <- d_structure %>%
        dplyr::left_join(line, by = c("lat", "long"))

    if (sum(is.na(combined_info)) == 0){
        in_the_match  <- TRUE
      }
    }
    line_info <- c(line_info, in_the_match)

  }
  testthat::expect_true(all(line_info))

})

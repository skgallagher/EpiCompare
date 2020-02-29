context("tests for band visuals")

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

test_that("check_inside_elipsoid tests", {
            # all false since Sigma not PSD (and warning )
            Sigma <- matrix(c(0,1,1,0), nrow = 2)
            center <- c(0,0)
            bound <- 1
            data <- rnorm(1000) %>% matrix(ncol = 2) %>% data.frame

            # due to Sigma
            testthat::expect_warning(all_false <- check_inside_elipsoid(data, Sigma, center, bound)) # warning

            testthat::expect_true(all(all_false == F))
            all_false <- check_inside_elipsoid(data, Sigma, center, bound,
                                               suppress_warning = TRUE) # no warning
            testthat::expect_true(all(all_false == F))


            # due to bound
            Sigma <- matrix(c(1,0,0,1), nrow = 2)
            bound <- -1
            testthat::expect_warning(all_false <- check_inside_elipsoid(data, Sigma, center, bound)) # warning
            testthat::expect_true(all(all_false == F))

            all_false <- check_inside_elipsoid(data, Sigma, center, bound, suppress_warning = TRUE) # no warning
            testthat::expect_true(all(all_false == F))


            # actual checks:
            Sigma <- matrix(c(1,0,0,1), nrow = 2)
            bound <- 1

            # identity matrix
            expected_out <- data %>% .^2 %>% apply(1, sum) %>% "<="(.,1)
            actual_out <- check_inside_elipsoid(data, Sigma, center, bound)

            testthat::expect_equal(expected_out,actual_out)

            # identity matrix, shift center
            center <- c(1,2)
            expected_out <- data %>% t %>% "-"(., center) %>% t %>% "^"(.,2) %>%
              apply(1, sum) %>% "<="(.,1)
            actual_out <- check_inside_elipsoid(data, Sigma, center, bound)
            testthat::expect_equal(expected_out, actual_out)

            # 2* Identity, shift center
            Sigma <- 2 * matrix(c(1,0,0,1), nrow = 2)
            expected_out <- data %>% t %>% "-"(., center) %>% t %>% .^2 %>%
              "*"(.,1/2) %>%
              apply(1, sum) %>% "<="(.,1)
            actual_out <- check_inside_elipsoid(data, Sigma, center, bound)
            testthat::expect_equal(expected_out, actual_out)

            # non identify Sigma
            Sigma <- matrix(c(1,.2,.2,1), nrow = 2)
            S_neg1 <- solve(Sigma)
            center <- c(0,0)
            bound <- 1

            expected_out <- diag(as.matrix(data) %*%
                                   S_neg1 %*%
                                   t(as.matrix(data))) <= bound
            actual_out <- check_inside_elipsoid(data, Sigma, center, bound)
            testthat::expect_equal(expected_out, actual_out)

          })

test_that("check_inside_elipsoid_func tests - changes in parameters", {
            # basic example
            Sigma <- matrix(c(1,0,0,1), nrow = 2)
            center <- c(0,0)
            bound <- 1

            data <- rnorm(1000) %>% matrix(ncol = 2) %>% data.frame

            check_inside1 <- check_inside_elipsoid_func(Sigma, center, bound)

            first <- check_inside1(data)

            A <- matrix(runif(2^2)*2-1, ncol=2)
            Sigma <- t(A) %*% A
            center <- c(0,0)
            bound <- 1

            check_inside2 <- check_inside_elipsoid_func(Sigma, center, bound)
            second <- check_inside2(data)

            testthat::expect_true(any(first != second)) # need that the inputs are not overridden
          })

test_that("get_grid_elipsoid_containment tests - arbitary function lists",{
  false_function <- function(x){
    return(rep(FALSE, nrow(x)))
  }
  true_function <- function(x){
    return(rep(TRUE, nrow(x)))
  }
  greater_than2_function <- function(x){
    return(x[,1] > 2)
  }

  function_list <- list(false_function, true_function, greater_than2_function)

  a <- get_grid_elipsoid_containment(function_list,
                                     xrange = c(0,1),
                                     yrange = c(0))

  testthat::expect_equal(a , data.frame(x = c(0,1),
                                        y = c(0,0),
                                        included = c(1L,1L)))

  b <- get_grid_elipsoid_containment(list(false_function,
                                          greater_than2_function),
                                     xrange = c(0,1,3), yrange = c(0,1,2))

  testthat::expect_equal(b, data.frame(x = rep(c(0,1,3), 3),
                                       y = rep(0:2, each = 3)) %>%
                           dplyr::mutate(included = 1L * (x == 3)))

})

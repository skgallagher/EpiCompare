context("tests for prediction band visuals")

library(sp)
library(ggplot2)
library(dplyr)
library(ggtern); update_approved_layers()

# internal function to calculate area of contour(s) ---------------
get_area <- function(x) {
  if (!("piece" %in% names(x)) | length(unique(x$piece)) == 1) {
    data_inner <- x[,c("x", "y")]
    data_inner <- rbind(data_inner, data_inner[1,])
    sp::coordinates(data_inner) <- c("x","y")
    data_sp <- sp::Polygon(data_inner)
    return(data_sp@area)
  } else {
    areas <- sapply(dplyr::group_split(dplyr::group_by(x, .data$piece)), get_area)
    return(sum(areas))
  }
}

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

test_that("project_onto_simplex", {
  visual_check <- FALSE
  # 2d projection checks (from uniform(0,1))
  z <- 1
  for (i in 1:25){
    x <- runif(2, 0, 1)

    proj_x <- project_onto_simplex(x)

    if (visual_check) {
      data1 <- data.frame(X = x[1], Y = x[2],
                          X_proj = proj_x[1],
                          Y_proj = proj_x[2])


      data_simplex <- data.frame(X_low = 0,
                                 Y_low = z,
                                 X_high = z,
                                 Y_high = 0)

      ggplot() + geom_segment(data = data1, aes(x = X, y = Y,
                                                xend = X_proj,
                                                yend = Y_proj)) +
        geom_point(data = data1, aes(x = X, y = Y)) +
        geom_point(data = data1, aes(x = X_proj, y = Y_proj), color = "blue") +
        geom_segment(data = data_simplex, aes(x = X_low, y = Y_low,
                                              xend = X_high,
                                              yend = Y_high), color = "blue") +
        coord_fixed()
    }

    # project onto hyperplane
    testthat::expect_equal(sum(proj_x), z)
    # direction orthogonal to hyperplane
    if (all(proj_x > 0)){
      testthat::expect_equal((x - proj_x) %*% c(-1,1), matrix(0))
    }
    # all coords >= 0
    testthat::expect_true(all(proj_x >= 0))

  }


  # Nd projection checks
  for (i in 1:100){
    x <- runif(5, -10, 10)

    proj_x <- project_onto_simplex(x)

    # project onto hyperplane
    testthat::expect_equal(sum(proj_x), z)

    # if it's in the relative interior...
    if (all(proj_x > 0)){
      # direction orthogonal to hyperplane
      for (idx in 1:length(x)){
        e_i <- rep(0, length(x))
        e_i[idx] <- z
        direction_i <- e_i - rep(z/length(x), length(x))
        testthat::expect_equal((x - proj_x) %*% direction_i,
                               matrix(0))
      }

    }
    # all coords >= 0
    testthat::expect_true(all(proj_x >= 0))
  }
})

# simulation for geom_prediction_band testing --------------------

set.seed(1)
n_sims <- 10
n_time_steps <- 100
beta <- .1
gamma <- .03
init_SIR <- c(950, 50, 0)


sim10 <- EpiCompare::simulate_SIR_agents(n_sims = n_sims,
                                        n_time_steps = n_time_steps,
                                        beta = beta, gamma = gamma,
                                        init_SIR = init_SIR)

df_group <- sim10 %>% group_by(sim) %>%
  agents_to_aggregate(states = c("tI", "tR")) %>%
  rename(S = "X0", I = "X1", R = "X2")

## uniform + kde (sim_group)


test_that(paste("geom_prediction_band correctly deals with conf_level,",
                "(uniform bands, kde)"),
        {
          # different conf_level
          for (pb_type in c("kde", "delta_ball", "convex_hull")){
            vis_pred_level.9 <- ggtern() + #ggplot() +
              geom_prediction_band(data = df_group,
                                   aes(x = S, y = I, z = R,
                                       sim_group = as.numeric(sim)),
                                   conf_level = .9, pb_type = pb_type)

            data.9 <- ggtern::layer_data(vis_pred_level.9)
            testthat::expect_is(data.9, "data.frame")

            label <- paste0(pb_type,
                           paste0(names(data.9),collapse = "*_*"))
            testthat::expect_true(all(c("x", "y") %in% names(data.9)),
                                  label = label)
            vis_pred_level.1 <- ggplot() +
              geom_prediction_band(data = df_group,
                                   aes(x = S, y = I, z = R,
                                       sim_group = as.numeric(sim)),
                                   conf_level = .1, pb_type = pb_type) +
              coord_tern()

            data.1 <- ggtern::layer_data(vis_pred_level.1)
            data.1.area <- data.1 %>% get_area()
            data.9.area <- data.9 %>% get_area()

            testthat::expect_lt(data.1.area, data.9.area)
          }

          })


test_that(paste("geom_prediction_band correctly deals with grid_size,",
                "(kde, delta_ball)"),
         {

  for (pb_type in c("kde", "delta_ball")){
    #^ not convex hull doesn't really take grid_size

    # dealing with different grids:
    suppressWarnings(vis_pred_level.9.1 <- ggplot() +
                       geom_prediction_band(data = df_group,
                                            aes(x = S, y = I, z = R,
                                                sim_group = as.numeric(sim)),
                                            grid_size = rep(10,2),
                                            conf_level = .9, pb_type = pb_type) +
                       coord_tern())

    data.9.1 <- ggtern::layer_data(vis_pred_level.9.1)

    suppressWarnings(vis_pred_level.1 <- ggplot() +
                       geom_prediction_band(data = df_group,
                                            aes(x = S, y = I, z = R,
                                                sim_group = as.numeric(sim)),
                                            conf_level = .9, pb_type = pb_type) +
                       coord_tern())

    data.9 <- ggtern::layer_data(vis_pred_level.1)

    testthat::expect_lt(nrow(data.9.1), nrow(data.9))
  }

})

test_that(paste("stat_prediction_band correctly deals with conf_level,",
                "(uniform bands, kde)"),
          {

  # different conf_level
  for (pb_type in c("kde", "delta_ball", "convex_hull")){
    vis_pred_level.9 <- ggplot() +
      stat_prediction_band(data = df_group,
                           aes(x = S, y = I, z = R,
                               sim_group = as.numeric(sim)),
                           conf_level = .9, pb_type = pb_type) +
      coord_tern()

    data.9 <- ggtern::layer_data(vis_pred_level.9)

    vis_pred_level.1 <- ggplot() +
      stat_prediction_band(data = df_group,
                           aes(x = S, y = I, z = R,
                               sim_group = as.numeric(sim)),
                           conf_level = .1, pb_type = pb_type) +
      coord_tern()

    data.1 <- ggtern::layer_data(vis_pred_level.1)

    data.1.area <- data.1 %>% get_area()
    data.9.area <- data.9 %>% get_area()

    testthat::expect_lt(data.1.area, data.9.area)
  }

})

test_that(paste("stat_prediction_band correctly deals with grid_size,",
                "(kde, delta_ball)"),
          {
            #^ not convex hull doesn't really take grid_size
  for (pb_type in c("kde", "delta_ball")){
    # dealing with different grids:
    suppressWarnings(vis_pred_level.9.1 <- ggplot() +
                       stat_prediction_band(data = df_group,
                                            aes(x = S, y = I, z = R,
                                                sim_group = as.numeric(sim)),
                                            grid_size = rep(10,2),
                                            conf_level = .9, pb_type = pb_type) +
                       coord_tern())

    data.9.1 <- ggtern::layer_data(vis_pred_level.9.1)

    suppressWarnings(vis_pred_level.9 <- ggplot() +
                       stat_prediction_band(data = df_group,
                                            aes(x = S, y = I, z = R,
                                                sim_group = as.numeric(sim)),
                                            conf_level = .9, pb_type = pb_type) +
                       coord_tern())

    data.9 <- ggtern::layer_data(vis_pred_level.9)

  testthat::expect_lt(nrow(data.9.1), nrow(data.9))
}
})

# TODO make checks to show kde doesn't really need the sim_group?

## spherical (uses t) --------------

test_that(paste("geom_prediction_band correctly deals with conf_level,",
                "(spherical bands)"),
          {
            # different conf_level
            for (pb_type in c("spherical_ball")){
              vis_pred_level.9 <- ggplot() +
                geom_prediction_band(data = df_group,
                                     aes(x = S, y = I, z = R,
                                         t = as.numeric(t)),
                                     conf_level = .9, pb_type = pb_type) +
                coord_tern()

              data.9 <- ggtern::layer_data(vis_pred_level.9)

              vis_pred_level.1 <- ggplot() +
                geom_prediction_band(data = df_group,
                                     aes(x = S, y = I, z = R,
                                         t = as.numeric(t)),
                                     conf_level = .1, pb_type = pb_type) +
                coord_tern()

              data.1 <- ggtern::layer_data(vis_pred_level.1)

              data.1.area <- data.1 %>% get_area()
              data.9.area <- data.9 %>% get_area()

              testthat::expect_lt(data.1.area, data.9.area)
            }

          })

test_that(paste("geom_prediction_band correctly deals with grid_size,",
                "(spherical bands)"),
          {

            for (pb_type in c("spherical_ball")){

              # dealing with different grids:
              vis_pred_level.9.1 <- ggplot() +
                                 geom_prediction_band(data = df_group,
                                                      aes(x = S, y = I, z = R,
                                                          t = as.numeric(t)),
                                                      grid_size = rep(10,2),
                                                      conf_level = .9,
                                                      pb_type = pb_type) +
                                 coord_tern()

              data.9.1 <- ggtern::layer_data(vis_pred_level.9.1)

              vis_pred_level.9 <- ggplot() +
                                 geom_prediction_band(data = df_group,
                                                      aes(x = S, y = I, z = R,
                                                          t = as.numeric(t)),
                                                      conf_level = .9,
                                                      pb_type = pb_type) +
                                 coord_tern()

              data.9 <- ggtern::layer_data(vis_pred_level.9)

              testthat::expect_lt(nrow(data.9.1), nrow(data.9))
            }

          })

test_that(paste("stat_prediction_band correctly deals with conf_level,",
                "(spherical bands)"),
          {
            # different conf_level
            for (pb_type in c("spherical_ball")){
              vis_pred_level.9 <- ggplot() +
                stat_prediction_band(data = df_group,
                                     aes(x = S, y = I, z = R,
                                         t = as.numeric(t)),
                                     conf_level = .9,
                                     pb_type = pb_type) +
                coord_tern()

              data.9 <- ggtern::layer_data(vis_pred_level.9)

              vis_pred_level.1 <- ggplot() +
                stat_prediction_band(data = df_group,
                                     aes(x = S, y = I, z = R,
                                         t = as.numeric(t)),
                                     conf_level = .1, pb_type = pb_type) +
                coord_tern()

              data.1 <- ggtern::layer_data(vis_pred_level.1)

              data.1.area <- data.1 %>% get_area()
              data.9.area <- data.9 %>% get_area()

              testthat::expect_lt(data.1.area, data.9.area)
            }

          })

test_that(paste("stat_prediction_band correctly deals with grid_size,",
                "(spherical bands)"),
          {
            for (pb_type in c("spherical_ball")){
              # dealing with different grids:
              suppressWarnings(vis_pred_level.9.1 <- ggplot() +
                                 stat_prediction_band(data = df_group,
                                                      aes(x = S, y = I, z = R,
                                                          t = as.numeric(t)),
                                                      grid_size = rep(10,2),
                                                      conf_level = .9,
                                                      pb_type = pb_type) +
                                 coord_tern())

              data.9.1 <- ggtern::layer_data(vis_pred_level.9.1)

              suppressWarnings(vis_pred_level.1 <- ggplot() +
                                 stat_prediction_band(data = df_group,
                                                      aes(x = S, y = I, z = R,
                                                          t = as.numeric(t)),
                                                      conf_level = .9, pb_type = pb_type) +
                                 coord_tern())

              data.9 <- ggtern::layer_data(vis_pred_level.1)

              testthat::expect_lt(nrow(data.9.1), nrow(data.9))
            }
          })

# multiple colors ------------------

df_group_two <- df_group %>% mutate(class_type = as.numeric(.data$sim) > 5)

test_that(paste("geom_prediction_band correctly deals with multiple groups,",
                "(uniform bands, kde)"),
          {
            # standard
            for (pb_type in c("kde", "delta_ball", "convex_hull")){
              vis_pred_level.1 <- ggplot() +
                geom_prediction_band(data = df_group_two,
                                     aes(x = S, y = I, z = R,
                                         sim_group = as.numeric(sim),
                                         color = class_type),
                                     conf_level = .5, pb_type = pb_type) +
                coord_tern()

              data.1 <- ggtern::layer_data(vis_pred_level.1)

              number_groups <- data.1 %>% pull(colour) %>% unique() %>% length()

              testthat::expect_equal(number_groups, 2)
            }

            # split
            for (pb_type in c("kde", "delta_ball", "convex_hull")){
              vis_pred_level.1 <- ggplot() +
                geom_prediction_band(data = df_group_two %>% filter(t < 40 | t > 65),
                                     aes(x = S, y = I, z = R,
                                         sim_group = as.numeric(sim),
                                         color = class_type),
                                     conf_level = .1, pb_type = pb_type) +
                coord_tern()

              data.1 <- ggtern::layer_data(vis_pred_level.1)

              number_groups <- data.1 %>% pull(colour) %>% unique() %>% length()

              testthat::expect_equal(number_groups, 2)
            }
})

test_that("delta_ball, kde correctly seperates subsections", {
  new_data <- df_group %>% filter(t < 40 | t > 65)
  for (pb_type in c("delta_ball", "kde")) {
    vis_pred_level.1 <- ggplot() +
      geom_prediction_band(data = new_data,
                           aes(x = S, y = I, z = R,
                               sim_group = as.numeric(sim)),
                           conf_level = .9, pb_type = pb_type) +
      coord_tern()

    data.1 <- ggtern::layer_data(vis_pred_level.1)

    for (group_v in unique(data.1$group)){
      d_mat <- dist(data.1[data.1$group == group_v, c("x", "y", "z")]) %>%
        as.matrix()

      # grab off diagonal
      id_delta <- row(d_mat) - col(d_mat)

      delta <- get_delta(data.1[data.1$group == group_v, c("x", "y", "z")])$mm_delta

      testthat::expect_equal(sum(d_mat[id_delta == 1] > delta * 10),0)
    }
  }
})

test_that(paste("spherical_ball correctly seperates subsections,",
                "only looking at the 2 biggest parts"), {
  new_data <- df_group %>% filter(t < 40 | t > 65)
  for (pb_type in c("spherical_ball")) {
    vis_pred_level.1 <- ggplot() +
      geom_prediction_band(data = new_data,
                           aes(x = S, y = I, z = R,
                               t = as.numeric(t)),
                           conf_level = .9, pb_type = pb_type) +
      coord_tern()

    data.1 <- ggtern::layer_data(vis_pred_level.1)

    big_group_id <- data.1 %>% group_by(group) %>% summarize(n = n()) %>%
      arrange(desc(n)) %>% top_n(2) %>% pull(group)

    for (group_v in big_group_id){
      d_mat <- dist(data.1[data.1$group == group_v, c("x", "y", "z")]) %>%
        as.matrix()

      # grab off diagonal
      id_delta <- row(d_mat) - col(d_mat)

      delta <- get_delta(data.1[data.1$group == group_v, c("x", "y", "z")])$mm_delta

      testthat::expect_equal(sum(d_mat[id_delta == 1] > delta * 10),0)
    }
  }
})

test_that(paste("geom_prediction_band correctly deals with multiple groups,",
                "(spherical)"),
          {
            # different conf_level
            for (pb_type in c("spherical_ball")){
              vis_pred_level.9 <- ggplot() +
                geom_prediction_band(data = df_group_two,
                                     aes(x = S, y = I, z = R,
                                         t = as.numeric(t),
                                         color = class_type),
                                     conf_level = .9, pb_type = pb_type) +
                coord_tern()

              data.9 <- ggtern::layer_data(vis_pred_level.9)

              number_groups <- data.9 %>% pull(colour) %>% unique() %>% length()

              testthat::expect_equal(number_groups, 2)
            }
          })

# different length time series ----------------------

trans_mat <- matrix(c("X0 * (1 - X1 * par1 / N)", "X0 * X1  * par1 / N", "0",
                      "0", "X1 * (1 - par2)", "par2 * X1",
                      "0", "0", "X2"), byrow = TRUE, nrow = 3)
rownames(trans_mat) <- c("S", "I", "R")
init_vals <- c(187, 1, 0)
par_vals <- c("par1" = .2, "par2" = .1)
max_T <- 55
n_sims <- 20

B <- 5

sigma <- matrix(c(6.401576e-04, 2.706480e-15,
                  2.706480e-15, 7.437683e-05),
                nrow = 2, byrow = T)
mu <- c(0.362442, 0.126167)

par_val_mat <- MASS::mvrnorm(n = B, mu = mu, Sigma = sigma)


set.seed(11)
sim_list <- vector(mode = "list", length = B)
for(bb in 1:B){
  
  par_vals <-  c("par1" = par_val_mat[bb, 1],
                 "par2" = par_val_mat[bb, 2])
  
  
  abm <- simulate_agents(trans_mat = trans_mat,
                         init_vals = init_vals,
                         par_vals = par_vals,
                         max_T = max_T,
                         n_sims = 2,
                         verbose = FALSE)
  agg_model <- abm %>% dplyr::group_by(sim) %>% 
    agents_to_aggregate(states = c(I, R)) %>%
    ungroup()
  agg_model$batch <- bb
  agg_model$beta <- par_vals[1]
  agg_model$gamma <- par_vals[2]
  sim_list[[bb]] <- agg_model
  
}

sim_df <- dplyr::bind_rows(sim_list)
sim_df2 <- sim_df
#table(sim_df$batch, sim_df$sim) # shouldn't all be the same for check
sim_df$id <- paste0(sim_df$batch, ".",
                    sim_df$sim)

plot_df <- sim_df %>% dplyr::filter(t != 0) %>%
  dplyr::select(id, t, X0, X1, X2)


test_that(paste("'delta_ball' geom_prediction_band works with different length",
                "simulations."), {
  tab_sdf <- table(sim_df2$batch, sim_df2$sim)
  testthat::expect_gt(length(unique(tab_sdf)), 1)
  
  pb_type = c("delta_ball", "kde", "spherical_ball", "convex_hull")[1]
  ggplot() +
    geom_prediction_band(data = plot_df,
                         aes(x = X0, y = X1, z = X2, 
                             sim_group = as.numeric(id)), alpha = .5,
                         fill = "cornflowerblue",
                         pb_type = pb_type) +
    coord_tern() + theme_sir() +
    labs(title = "Prediction band for best parameters")   

})


test_that(paste("'kde' geom_prediction_band works with different length",
                "simulations."), {
  tab_sdf <- table(sim_df2$batch, sim_df2$sim)
  testthat::expect_gt(length(unique(tab_sdf)), 1)
  
  pb_type = c("delta_ball", "kde", "spherical_ball", "convex_hull")[2]
  ggplot() +
    geom_prediction_band(data = plot_df,
                         aes(x = X0, y = X1, z = X2, 
                             sim_group = as.numeric(id)), alpha = .5,
                         fill = "cornflowerblue",
                         pb_type = pb_type) +
    coord_tern() + theme_sir() +
    labs(title = "Prediction band for best parameters")
})


test_that(paste("'spherical_ball' geom_prediction_band works with different",
                "length simulations."), {
  tab_sdf <- table(sim_df2$batch, sim_df2$sim)
  testthat::expect_gt(length(unique(tab_sdf)), 1)
  
  pb_type = c("delta_ball", "kde", "spherical_ball", "convex_hull")[3]
  ggplot() +
    geom_prediction_band(data = plot_df,
                         aes(x = X0, y = X1, z = X2, 
                             t = as.numeric(t)), alpha = .5,
                         fill = "cornflowerblue",
                         pb_type = pb_type) +
    coord_tern() + theme_sir() +
    labs(title = "Prediction band for best parameters")
})

test_that(paste("'convex_hull' geom_prediction_band works with different",
                "length simulations."), {
  tab_sdf <- table(sim_df2$batch, sim_df2$sim)
  testthat::expect_gt(length(unique(tab_sdf)), 1)
    
  pb_type = c("delta_ball", "kde", "spherical_ball", "convex_hull")[4]
  ggplot() +
    geom_prediction_band(data = plot_df,
                         aes(x = X0, y = X1, z = X2, 
                             sim_group = as.numeric(id)), alpha = .5,
                         fill = "cornflowerblue",
                         pb_type = pb_type) +
    coord_tern() + theme_sir() +
    labs(title = "Prediction band for best parameters")
})




# 

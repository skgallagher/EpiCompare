context("containment and band creation tests")

library(tidyverse)

test_that("test filament_compression, no data_columns", {
  # basic check:
  t13compression <- EpiCompare::pomp_sir %>%
    dplyr::arrange(time) %>%
    dplyr::select(-H, -cases, -time) %>%
    dplyr::filter(.id <= 5) %>%
    dplyr::group_by(.id) %>%
    filament_compression()

  # correct number of rows
  testthat::expect_equal(nrow(t13compression), 13*5)

  testthat::expect_is(t13compression, "grouped_df")

  # cleaner check
  first_group <- data.frame(t = 0:16,
                            x = 0:16,
                            y = 0:16,
                            z = 0:16,
                            .id = 1)
  second_group <- data.frame(t = 0:16,
                             x = 0:16,
                             y = rep(0,17),
                             z = rep(0,17),
                             .id = 2)
  third_group <- data.frame(t = 0:16,
                            x = 0:16,
                            y = c(0:8,7:0),
                            z = rep(0,17),
                            .id = 3)

  grouped_df <- rbind(first_group, second_group, third_group) %>%
    group_by(.id)

  test_compression <- grouped_df %>% dplyr::arrange(t) %>%
    dplyr::select(-t) %>%
    dplyr::group_by(.id) %>%
    filament_compression(number_points = 9)

  unique_values_per <- test_compression %>%
    dplyr::summarize(count = n()) %>% dplyr::pull(count) %>% unique

  testthat::expect_equal(unique_values_per, 9)
  # group1:

  still_linear <- test_compression %>% dplyr::filter(.id == 1) %>%
    dplyr::ungroup(.id) %>% dplyr::select(-.id) %>%
    apply(1, function(row) length(unique(row)) == 1)
  testthat::expect_equal(still_linear, rep(TRUE, 9))

  # group2:
  xx2 <- test_compression %>% dplyr::filter(.id == 2) %>%
    dplyr::ungroup(.id) %>%
    dplyr::pull(x)
  testthat::expect_equal(xx2, (0:8)*2)

  # group3:
  xx3 <- test_compression %>% dplyr::filter(.id == 3) %>%
    dplyr::ungroup(.id) %>% pull(x)
  yy3 <- test_compression %>% dplyr::filter(.id == 3) %>%
    dplyr::ungroup(.id) %>% pull(y)

  testthat::expect_equal(xx2, xx3)
  testthat::expect_equal(yy3, c((0:4)*2,(3:0)*2))

})

test_that("test filament_compression, data_columns string", {
  # basic check:
  t13compression <- EpiCompare::pomp_sir %>%
    dplyr::arrange(time) %>%
    dplyr::select(-H, -cases, -time) %>%
    dplyr::filter(.id <= 5) %>%
    dplyr::group_by(.id) %>%
    filament_compression(data_columns = c("S", "I","R"))

  # correct number of rows
  testthat::expect_equal(nrow(t13compression), 13*5)

  testthat::expect_is(t13compression, "grouped_df")

  # cleaner check
  first_group <- data.frame(t = 0:16,
                            x = 0:16,
                            y = 0:16,
                            z = 0:16,
                            .id = 1)
  second_group <- data.frame(t = 0:16,
                             x = 0:16,
                             y = rep(0,17),
                             z = rep(0,17),
                             .id = 2)
  third_group <- data.frame(t = 0:16,
                            x = 0:16,
                            y = c(0:8,7:0),
                            z = rep(0,17),
                            .id = 3)

  grouped_df <- rbind(first_group, second_group, third_group) %>%
    dplyr::group_by(.id)

  test_compression <- grouped_df %>% dplyr::arrange(t) %>%
    dplyr::select(-t) %>%
    dplyr::group_by(.id) %>%
    filament_compression(number_points = 9,
                         data_columns = c("x","y","z"))

  unique_values_per <- test_compression %>%
    dplyr::summarize(count = n()) %>% dplyr::pull(count) %>% unique

  testthat::expect_equal(unique_values_per, 9)
  # group1:

  still_linear <- test_compression %>% dplyr::filter(.id == 1) %>%
    dplyr::ungroup(.id) %>% dplyr::select(-.id) %>%
    apply(1, function(row) length(unique(row)) == 1)
  testthat::expect_equal(still_linear, rep(TRUE, 9))

  # group2:
  xx2 <- test_compression %>% dplyr::filter(.id == 2) %>%
    dplyr::ungroup(.id) %>%
    dplyr::pull(x)
  testthat::expect_equal(xx2, (0:8)*2)

  # group3:
  xx3 <- test_compression %>% dplyr::filter(.id == 3) %>%
    dplyr::ungroup(.id) %>% dplyr::pull(x)
  yy3 <- test_compression %>% dplyr::filter(.id == 3) %>%
    dplyr::ungroup(.id) %>% dplyr::pull(y)

  testthat::expect_equal(xx2, xx3)
  testthat::expect_equal(yy3, c((0:4)*2,(3:0)*2))

})

test_that("test filament_compression, data_columns tidified", {
  # basic check:
  t13compression <- EpiCompare::pomp_sir %>%
    dplyr::arrange(time) %>%
    dplyr::select(-H, -cases, -time) %>%
    dplyr::filter(.id <= 5) %>%
    dplyr::group_by(.id) %>%
    filament_compression(data_columns = c(S,I,R))

  # correct number of rows
  testthat::expect_equal(nrow(t13compression), 13*5)

  testthat::expect_is(t13compression, "grouped_df")

  # cleaner check
  first_group <- data.frame(t = 0:16,
                            x = 0:16,
                            y = 0:16,
                            z = 0:16,
                            .id = 1)
  second_group <- data.frame(t = 0:16,
                             x = 0:16,
                             y = rep(0,17),
                             z = rep(0,17),
                             .id = 2)
  third_group <- data.frame(t = 0:16,
                            x = 0:16,
                            y = c(0:8,7:0),
                            z = rep(0,17),
                            .id = 3)

  grouped_df <- rbind(first_group, second_group, third_group) %>%
    dplyr::group_by(.id)

  test_compression <- grouped_df %>% dplyr::arrange(t) %>%
    dplyr::select(-t) %>%
    dplyr::group_by(.id) %>%
    filament_compression(number_points = 9,
                         data_columns = c(x,y,z))

  unique_values_per <- test_compression %>%
    dplyr::summarize(count = n()) %>% dplyr::pull(count) %>% unique

  testthat::expect_equal(unique_values_per, 9)
  # group1:

  still_linear <- test_compression %>% dplyr::filter(.id == 1) %>%
    dplyr::ungroup(.id) %>% dplyr::select(-.id) %>%
    apply(1, function(row) length(unique(row)) == 1)
  testthat::expect_equal(still_linear, rep(TRUE, 9))

  # group2:
  xx2 <- test_compression %>% filter(.id == 2) %>%
    dplyr::ungroup(.id) %>%
    dplyr::pull(x)
  testthat::expect_equal(xx2, (0:8)*2)

  # group3:
  xx3 <- test_compression %>% dplyr::filter(.id == 3) %>%
    dplyr::ungroup(.id) %>% dplyr::pull(x)
  yy3 <- test_compression %>% dplyr::filter(.id == 3) %>%
    dplyr::ungroup(.id) %>% dplyr::pull(y)

  testthat::expect_equal(xx2, xx3)
  testthat::expect_equal(yy3, c((0:4)*2,(3:0)*2))

})

test_that("filament_distance_depth correct depth, no data_columns",{
  dd_pomp_df <-  EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>% dplyr::select(-time, -H, -cases) %>%
    filament_distance_depth()

  testthat::expect_equal(length(dd_pomp_df),
                         10)


  # different lengths:
  first_group <- data.frame(t = 0:15,
                            x = 0:15,
                            y = 0:15,
                            z = 0:15,
                            .id = 1)
  second_group <- data.frame(t = 0:16,
                             x = 0:16,
                             y = rep(0,17),
                             z = rep(0,17),
                             .id = 2)
  third_group <- data.frame(t = 0:8,
                            x = 0:8,
                            y = 0:8,
                            z = rep(0,9),
                            .id = 3)

  grouped_df <- rbind(first_group, second_group, third_group) %>%
    group_by(.id)

  testthat::expect_message(grouped_df %>% dplyr::select(-t) %>%
                             filament_distance_depth())

  depth_3 <- suppressMessages(grouped_df %>% dplyr::select(-t) %>%
                                filament_distance_depth())

  # global distance-depth expectation:
  testthat::expect_equal(depth_3 %in% c(0,1), rep(TRUE, 3))
  testthat::expect_equal(sum(depth_3), 1)

})

test_that("filament_distance_depth correct depth, string",{
  dd_pomp_df <-  EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>%
    filament_distance_depth(data_columns =c("S","I","R"))

  testthat::expect_equal(length(dd_pomp_df),
                         10)


  # different lengths:
  first_group <- data.frame(t = 0:15,
                            x = 0:15,
                            y = 0:15,
                            z = 0:15,
                            .id = 1)
  second_group <- data.frame(t = 0:16,
                             x = 0:16,
                             y = rep(0,17),
                             z = rep(0,17),
                             .id = 2)
  third_group <- data.frame(t = 0:8,
                            x = 0:8,
                            y = 0:8,
                            z = rep(0,9),
                            .id = 3)

  grouped_df <- rbind(first_group, second_group, third_group) %>%
    group_by(.id)

  testthat::expect_message(grouped_df %>%
                             filament_distance_depth(
                               data_columns =c("x","y","z")))

  depth_3 <- suppressMessages(grouped_df %>%
                                filament_distance_depth(
                                  data_columns =c("x","y","z")))

  # global distance-depth expectation:
  testthat::expect_equal(depth_3 %in% c(0,1), rep(TRUE, 3))
  testthat::expect_equal(sum(depth_3), 1)

})

test_that("filament_distance_depth correct depth, tidified",{
  dd_pomp_df <-  EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>%
    filament_distance_depth(data_columns =c(S,I,R))

  testthat::expect_equal(length(dd_pomp_df),
                         10)


  # different lengths:
  first_group <- data.frame(t = 0:15,
                            x = 0:15,
                            y = 0:15,
                            z = 0:15,
                            .id = 1)
  second_group <- data.frame(t = 0:16,
                             x = 0:16,
                             y = rep(0,17),
                             z = rep(0,17),
                             .id = 2)
  third_group <- data.frame(t = 0:8,
                            x = 0:8,
                            y = 0:8,
                            z = rep(0,9),
                            .id = 3)

  grouped_df <- rbind(first_group, second_group, third_group) %>%
    group_by(.id)

  testthat::expect_message(grouped_df %>%
                             filament_distance_depth(
                               data_columns =c(x,y,z)))

  depth_3 <- suppressMessages(grouped_df %>%
                                filament_distance_depth(
                                  data_columns =c(x,y,z)))

  # global distance-depth expectation:
  testthat::expect_equal(depth_3 %in% c(0,1), rep(TRUE, 3))
  testthat::expect_equal(sum(depth_3), 1)

})

test_that(paste0("test grab_top_depth_filaments (.remove_group = both",
          ", no data_columns)"), {
  top_filaments <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>% dplyr::select(-time, -H, -cases) %>%
    grab_top_depth_filaments(conf_level = .5,
                             .remove_group = FALSE)

  testthat::expect_equal(length(unique(top_filaments$.id)), 5)

  top_filaments_points <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>% dplyr::select(-time, -H, -cases) %>%
    grab_top_depth_filaments(conf_level = .5)

  testthat::expect_equivalent(top_filaments_points,
                              top_filaments %>% dplyr::select(-.id))


  all_but_exteme_filaments <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>% dplyr::select(-time, -H, -cases) %>%
    grab_top_depth_filaments(conf_level = 1,
                             .remove_group = FALSE)

  testthat::expect_equal(length(unique(all_but_exteme_filaments$.id)),
                         8)

  testthat::expect_error(EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
                           dplyr::filter(.id <= 10) %>% 
                           dplyr::select(-time, -H, -cases) %>%
                           grab_top_depth_filaments(conf_level = 0))

})

test_that(paste0("test grab_top_depth_filaments (.remove_group = both",
          ", string)"), {
  top_filaments <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>%
    grab_top_depth_filaments(data_columns =c("S","I","R"),
                             conf_level = .5,
                             .remove_group = FALSE)

  testthat::expect_equal(length(unique(top_filaments$.id)), 5)

  top_filaments_points <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>%
    grab_top_depth_filaments(data_columns =c("S","I","R"),
                             conf_level = .5)

  testthat::expect_equivalent(top_filaments_points,
                              top_filaments %>% dplyr::select(-.id))


  all_but_exteme_filaments <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>%
    grab_top_depth_filaments(data_columns = c("S","I","R"),
                             conf_level = 1,
                             .remove_group = FALSE)

  testthat::expect_equal(length(unique(all_but_exteme_filaments$.id)),
                         8)

  testthat::expect_error(EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
                           dplyr::filter(.id <= 10) %>%
                           grab_top_depth_filaments(
                             data_columns =c("S","I","R"),
                             conf_level = 0))

})

test_that(paste0("test grab_top_depth_filaments (.remove_group = both",
                ", tidified)"), {
  top_filaments <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>%
    grab_top_depth_filaments(data_columns =c(S,I,R),
                             conf_level = .5,
                             .remove_group = FALSE)

  testthat::expect_equal(length(unique(top_filaments$.id)), 5)

  top_filaments_points <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>%
    grab_top_depth_filaments(data_columns =c(S,I,R),
                             conf_level = .5)

  testthat::expect_equivalent(top_filaments_points,
                              top_filaments %>% dplyr::select(-.id))


  all_but_exteme_filaments <- EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
    dplyr::filter(.id <= 10) %>%
    grab_top_depth_filaments(data_columns = c(S,I,R),
                             conf_level = 1,
                             .remove_group = FALSE)

  testthat::expect_equal(length(unique(all_but_exteme_filaments$.id)),
                         8)

  testthat::expect_error(EpiCompare::pomp_df %>% dplyr::group_by(.id) %>%
                           dplyr::filter(.id <= 10) %>%
                           grab_top_depth_filaments(
                             data_columns =c(S,I,R),
                             conf_level = 0))

})


test_that(paste0("test grab_top_depth_filaments (.remove_group = both",
                 "all column options), error when none returned."), {
                   # no column names
                   testthat::expect_error(
                     top_filaments <- EpiCompare::pomp_df %>%
                       dplyr::group_by(.id) %>%
                       dplyr::filter(.id <= 10) %>% 
                       dplyr::select(-time, -H, -cases) %>%
                       grab_top_depth_filaments(conf_level = 0,
                                                .remove_group = FALSE))

                   testthat::expect_error(
                     top_filaments_points <- EpiCompare::pomp_df %>%
                       dplyr::group_by(.id) %>%
                       dplyr::filter(.id <= 10) %>% 
                       dplyr::select(-time, -H, -cases) %>%
                       grab_top_depth_filaments(conf_level = 0))

                   # string column names
                   testthat::expect_error(
                     top_filaments <- EpiCompare::pomp_df %>%
                       dplyr::group_by(.id) %>%
                       dplyr::filter(.id <= 10) %>% 
                       dplyr::select(-time, -H, -cases) %>%
                       grab_top_depth_filaments(conf_level = 0,
                                                .remove_group = FALSE,
                                                data_columns = c("S","I","R")
                                                ))

                   testthat::expect_error(
                     top_filaments_points <- EpiCompare::pomp_df %>%
                       dplyr::group_by(.id) %>%
                       dplyr::filter(.id <= 10) %>% 
                       dplyr::select(-time, -H, -cases) %>%
                       grab_top_depth_filaments(conf_level = 0,
                                                data_columns = c("S","I","R")))
                   # promise column names
                   testthat::expect_error(
                     top_filaments <- EpiCompare::pomp_df %>%
                       dplyr::group_by(.id) %>%
                       dplyr::filter(.id <= 10) %>% 
                       dplyr::select(-time, -H, -cases) %>%
                       grab_top_depth_filaments(conf_level = 0,
                                                .remove_group = FALSE,
                                                data_columns = c(S,I,R)
                       ))

                   testthat::expect_error(
                     top_filaments_points <- EpiCompare::pomp_df %>%
                       dplyr::group_by(.id) %>%
                       dplyr::filter(.id <= 10) %>% 
                       dplyr::select(-time, -H, -cases) %>%
                       grab_top_depth_filaments(conf_level = 0,
                                                data_columns = c(S,I,R)))
                 })


test_that(paste("test create_delta_ball_structure",
                "- little test because it's a wrapper",
                "(.lower_simplex_project = FALSE, no data_columns)"), {
                  set.seed(1)
                  data_points <- data.frame(matrix(rnorm(100), ncol = 4))

                  db_structure <- data_points %>%
                    create_delta_ball_structure(.lower_simplex_project = FALSE)
                  expected_delta <- get_delta(db_structure)$mm_delta

                  testthat::expect_equal(attr(db_structure, "delta"), expected_delta)

                  testthat::expect_equal(attr(db_structure, "A"), diag(4))

                  # repeat points (shouldn't do anything)
                  data_double <- rbind(data_points, data_points)
                  db_structure2 <- data_double %>%
                    create_delta_ball_structure(.lower_simplex_project = FALSE)

                  testthat::expect_equal(db_structure2, db_structure)
                })

test_that(paste("test create_delta_ball_structure",
                "- little test because it's a wrapper",
                "(.lower_simplex_project = FALSE, string)"), {
  set.seed(1)
  data_points <- data.frame(matrix(rnorm(100), ncol = 4))

  db_structure <- data_points %>%
    create_delta_ball_structure(data_columns = paste0("X",1:4),
                                .lower_simplex_project = FALSE)
  expected_delta <- get_delta(db_structure)$mm_delta

  testthat::expect_equal(attr(db_structure, "delta"), expected_delta)

  testthat::expect_equal(attr(db_structure, "A"), diag(4))

  # repeat points (shouldn't do anything)
  data_double <- rbind(data_points, data_points)
  db_structure2 <- data_double %>%
    create_delta_ball_structure(data_columns = paste0("X",1:4),
                                .lower_simplex_project = FALSE)

  testthat::expect_equal(db_structure2, db_structure)
})

test_that(paste("test create_delta_ball_structure",
                "- little test because it's a wrapper",
                "(.lower_simplex_project = FALSE, tidyified)"), {
                  set.seed(1)
                  data_points <- data.frame(matrix(rnorm(100), ncol = 4))

                  db_structure <- data_points %>%
                    create_delta_ball_structure(data_columns = c(X1, X2, X3, X4),
                                                .lower_simplex_project = FALSE)
                  expected_delta <- get_delta(db_structure)$mm_delta

                  testthat::expect_equal(attr(db_structure, "delta"), expected_delta)

                  testthat::expect_equal(attr(db_structure, "A"), diag(4))

                  # repeat points (shouldn't do anything)
                  data_double <- rbind(data_points, data_points)
                  db_structure2 <- data_double %>%
                    create_delta_ball_structure(data_columns = c(X1, X2, X3, X4),
                                                .lower_simplex_project = FALSE)

                  testthat::expect_equal(db_structure2, db_structure)
                })

test_that(paste("test create_delta_ball_structure",
                "- little test because it's a wrapper",
                "(.lower_simplex_project = TRUE)"), {
            set.seed(1)
            data_points <- data.frame(matrix(rnorm(100)^2, ncol = 4))

            db_structure <- data_points %>%
              create_delta_ball_structure(data_columns = paste0("X",1:4))
            expected_delta <- get_delta(db_structure)$mm_delta

            testthat::expect_equal(attr(db_structure, "delta"),
                                   expected_delta)
            testthat::expect_equal(attr(db_structure, "A"),
                                   simplex_project_mat(4))
            # repeat points (shouldn't do anything)
            data_double <- rbind(data_points, data_points)
            db_structure2 <- data_double %>%
              create_delta_ball_structure(data_columns = paste0("X",1:4))

            testthat::expect_equal(db_structure2, db_structure)
                })

test_that(paste("test create_delta_ball_structure",
                "- little test because it's a wrapper",
                "(.lower_simplex_project = TRUE, tidyified)"), {
                  set.seed(1)
                  data_points <- data.frame(matrix(rnorm(100)^2, ncol = 4))

                  db_structure <- data_points %>%
                    create_delta_ball_structure(data_columns = c(X1,X2,X3,X4))
                  expected_delta <- get_delta(db_structure)$mm_delta

                  testthat::expect_equal(attr(db_structure, "delta"),
                                         expected_delta)
                  testthat::expect_equal(attr(db_structure, "A"),
                                         simplex_project_mat(4))
                  # repeat points (shouldn't do anything)
                  data_double <- rbind(data_points, data_points)
                  db_structure2 <- data_double %>%
                    create_delta_ball_structure(data_columns = c(X1,X2,X3,X4))

                  testthat::expect_equal(db_structure2, db_structure)
                })

test_that(paste("test create_convex_hull_structure",
                "(.lower_simplex_project = FALSE, no data_column)"), {
                  box_data <- data.frame(x = c(0,0,1,1,.5),
                                         y = c(0,1,0,1,.5))
                  just_box1 <- create_convex_hull_structure(box_data,
                                                            .lower_simplex_project = FALSE)
                  testthat::expect_equal(attr(just_box1, "A"), diag(2))

                  # error associated with simplex projection if .lower_simplex_project = TRUE
                  testthat::expect_error(create_convex_hull_structure(box_data))

                  just_box2 <- create_convex_hull_structure(box_data,
                                                            .lower_simplex_project = FALSE)

                  testthat::expect_equivalent(just_box1, just_box2)
                  testthat::expect_equivalent(just_box1 %>% dplyr::arrange(x,y),
                                              box_data[-5,] %>% dplyr::arrange(x,y))

                })

test_that(paste("test create_convex_hull_structure",
                "(.lower_simplex_project = FALSE, string)"), {
                  box_data <- data.frame(x = c(0,0,1,1,.5),
                                         y = c(0,1,0,1,.5))
                  just_box1 <- create_convex_hull_structure(box_data,
                                                            data_columns = c("x","y"),
                                                            .lower_simplex_project = FALSE)
                  testthat::expect_equal(attr(just_box1, "A"), diag(2))

                  # error associated with simplex projection if .lower_simplex_project = TRUE
                  testthat::expect_error(create_convex_hull_structure(box_data))

                  just_box2 <- create_convex_hull_structure(box_data,
                                                            data_columns = c("x", "y"),
                                                            .lower_simplex_project = FALSE)

                  testthat::expect_equivalent(just_box1, just_box2)
                  testthat::expect_equivalent(just_box1 %>% dplyr::arrange(x,y),
                                              box_data[-5,] %>% dplyr::arrange(x,y))

                })

test_that(paste("test create_convex_hull_structure",
                "(.lower_simplex_project = FALSE, tidyify)"), {
                  box_data <- data.frame(x = c(0,0,1,1,.5),
                                         y = c(0,1,0,1,.5))
                  just_box1 <- create_convex_hull_structure(box_data,
                                                            data_columns = c(x,y),
                                                            .lower_simplex_project = FALSE)
                  testthat::expect_equal(attr(just_box1, "A"), diag(2))

                  # error associated with simplex projection if .lower_simplex_project = TRUE
                  testthat::expect_error(create_convex_hull_structure(box_data))

                  just_box2 <- create_convex_hull_structure(box_data,
                                                            data_columns = c(x,y),
                                                            .lower_simplex_project = FALSE)

                  testthat::expect_equivalent(just_box1, just_box2)
                  testthat::expect_equivalent(just_box1 %>% dplyr::arrange(x,y),
                                              box_data[-5,] %>% dplyr::arrange(x,y))

                })

test_that(paste("test create_convex_hull_structure",
                "(.lower_simplex_project = TRUE)"), {
          cubish_data <- data.frame(x = c(0,1,1,0,0,1,1,.5),
                                  y = c(1,0,1,0,1,0,1,.5),
                                  z = c(0,0,0,1,1,1,1,.5))
          cubish_ch <- create_convex_hull_structure(cubish_data)

          # dimension of projected space correct
          testthat::expect_equal(ncol(cubish_ch), 2)

          # should be the full triangle:
          tri_data <- data.frame(x = c(1,0,0),
                                 y = c(0,1,0),
                                 z = c(0,0,1))
          tri_ch <- create_convex_hull_structure(tri_data)
          testthat::expect_equivalent(cubish_ch, tri_ch)

          # A correct
          testthat::expect_equal(attr(cubish_ch, "A"), simplex_project_mat(3))

          # with data_columns specified
          cubish_ch2 <- create_convex_hull_structure(cubish_data,
                                       data_columns = c("x", "y", "z"))

          testthat::expect_equivalent(cubish_ch, cubish_ch2)

          ## new example (baby triangle)
          baby_tri_data <- data.frame(x = c(1,0,0),
                                      y = c(0,1,0),
                                      z = c(0,0,1)) * .5
          baby_tri_ch <- create_convex_hull_structure(baby_tri_data)
          testthat::expect_equivalent(baby_tri_ch, tri_ch)

          ## smart baby tri
          smart_baby_tri <- data.frame(x = c(.5, .5, 0),
                                       y = c(.5, 0, .5),
                                       z = c(0, .5, .5))

          smart_baby_tri_ch <- create_convex_hull_structure(smart_baby_tri)

          testthat::expect_equal(length(unique(dist(smart_baby_tri_ch))), 1)
          })

test_that("test hausdorff_dist (.lower_simplex_project = FALSE)", {
  # erroring (different structures)
  box_data <- data.frame(x = c(0,0,1,1,.5),
                         y = c(0,1,0,1,.5))
  ch_box <- create_convex_hull_structure(box_data,
                                         .lower_simplex_project = FALSE)
  db_box <- create_delta_ball_structure(box_data[-5,],
                                        .lower_simplex_project = FALSE)

  testthat::expect_error(hausdorff_dist(ch_box, db_box))

  # point outside
  ch_points <- rbind(data.frame(x = c(0), y = c(2)),
                     box_data)
  class(ch_points) <- c("convex_hull_structure", class(ch_points))

  db_points <- rbind(data.frame(x = c(0), y = c(2)),
                     box_data[-5,])
  class(db_points) <- c("delta_ball_structure", class(db_points))
  attr(db_points, "delta") <- 0

  testthat::expect_equal(hausdorff_dist(ch_points, ch_box), 1)
  testthat::expect_equal(hausdorff_dist(db_points, db_box), 1)

  attr(db_points, "delta") <- 1
  testthat::expect_equal(hausdorff_dist(db_points, db_box), 0)


  # point inside
  ch_point <- data.frame(x = c(.5), y = c(.5))
  class(ch_point) <- c("convex_hull_structure", class(ch_point))

  db_points <- box_data
  class(db_points) <- c("delta_ball_structure", class(db_points))
  attr(db_points, "delta") <- 0

  testthat::expect_equal(hausdorff_dist(ch_point, ch_box), sqrt(.5))
  testthat::expect_equal(hausdorff_dist(db_points, db_box), sqrt(.5))


  # 2 ch boxes
  box2 <- box_data + c(.1,.1)
  ch_box2 <- box2 %>%
    create_convex_hull_structure(.lower_simplex_project = FALSE)
  testthat::expect_true(all.equal(hausdorff_dist(ch_box2, ch_box),
                                  sqrt(.1^2+.1^2)))
  # 2 db boxes
  box2 <- box_data[-5,] + c(3,3)
  db_box2 <- box2 %>%
    create_delta_ball_structure(.lower_simplex_project = FALSE)
  testthat::expect_true(all.equal(hausdorff_dist(db_box2, db_box),
                                  sqrt(3^2+3^2) - attr(db_box,"delta")))
})

test_that("test hausdorff_dist (.lower_simplex_project = TRUE)", {
  # erroring (different structures)
  smart_baby_tri <- data.frame(x = c(.5, .5, 0),
                               y = c(.5, 0, .5),
                               z = c(0, .5, .5))
  ch_tri <- create_convex_hull_structure(smart_baby_tri)
  db_tri <- create_delta_ball_structure(smart_baby_tri)

  testthat::expect_error(hausdorff_dist(ch_box, db_box))


  # point "inside" - not distance won't be zero since ch_points
  # isn't actually the extreme points of the convex hovering of the points

  ch_points <- rbind(data.frame(V1 = 0, V2 = 0), # center
                     ch_tri)
  class(ch_points) <- c("convex_hull_structure", class(ch_points))

  db_points <- rbind(data.frame(V1 = 0, V2 = 0), # center
                     db_tri)
  class(db_points) <- c("delta_ball_structure", class(db_points))
  attr(db_points, "delta") <- 0

  testthat::expect_equal(hausdorff_dist(ch_points, ch_tri), .5)
  testthat::expect_equal(hausdorff_dist(db_points, db_tri), .5)

  attr(db_points, "delta") <- attr(db_tri,"delta")
  testthat::expect_equal(hausdorff_dist(db_points, db_tri), 0)

})

test_that(paste("test contained.delta_ball_structure",
                "(.lower_simplex_project = FALSE)"), {
  box_data <- data.frame(x = c(0,0,1,1,.5),
                         y = c(0,1,0,1,.5))
  db_box <- create_delta_ball_structure(box_data,
                                        .lower_simplex_project = FALSE)
  # point outside
  point <- data.frame(x = c(0), y = c(2))

  testthat::expect_false(contained(db_box, point,
                                   .lower_simplex_project = FALSE))

  # point inside
  point <- data.frame(x = 0, y = .5)
  testthat::expect_true(contained(db_box, point,
                                  .lower_simplex_project = FALSE))
})

test_that(paste("test contained.delta_ball_structure",
                "(.lower_simplex_project = TRUE)"),{
            smart_baby_tri <- data.frame(x = c(.5, .5, 0),
                                         y = c(.5, 0, .5),
                                         z = c(0, .5, .5))
            db_tri <- create_delta_ball_structure(smart_baby_tri)
            # point inside
            point <- data.frame(x = c(0), y = c(0))

            testthat::expect_true(contained(db_tri, point,
                                             .lower_simplex_project = FALSE))

            point3d <- data.frame(x = 1, y = 1, z = 1)
            testthat::expect_true(contained(db_tri, point3d))

            # point inside (just barely)
            point <- data.frame(x = 0, y = 1, z = 0)
            testthat::expect_true(contained(db_tri, point))

            # point outside
            point <- data.frame(x = 0, y = 1, z = 0)
            p_baby_tri <- data.frame(x = c(1, 1, .9),
                                     y = c(1, .9, 1),
                                     z = c(.9, 1, 1))
            p_db_tri <- create_delta_ball_structure(p_baby_tri)

            testthat::expect_false(contained(p_db_tri, point))
                })

test_that(paste("test contained.convex_hull_structure",
          "(.lower_simplex_project = FALSE)"), {
  box_data <- data.frame(x = c(0,0,1,1),
                         y = c(0,1,0,1))
  ch_box <- create_convex_hull_structure(box_data,
                                         .lower_simplex_project = FALSE)
  # point outside
  point <- data.frame(x = c(0), y = c(2))

  testthat::expect_false(contained(ch_box, point,
                                   .lower_simplex_project = FALSE))

  # point inside
  point <- data.frame(x = .5, y = .5)
  testthat::expect_true(contained(ch_box, point,
                                  .lower_simplex_project = FALSE))
})

test_that(paste("test contained.convex_hull_structure",
                "(.lower_simplex_project = TRUE)"), {
            smart_baby_tri <- data.frame(x = c(.5, .5, 0),
                                         y = c(.5, 0, .5),
                                         z = c(0, .5, .5))
            ch_tri <- create_convex_hull_structure(smart_baby_tri)

            # point inside
            point <- data.frame(x = c(0), y = c(0))

            testthat::expect_true(contained(ch_tri, point,
                                            .lower_simplex_project = FALSE))

            point_3d <- data.frame(x = 1, y = 1, z = 1)
            testthat::expect_true(contained(ch_tri, point_3d))

            # point outside
            point <- data.frame(x = 0, y = 1, z = 0)
            testthat::expect_false(contained(ch_tri, point))
          })


test_that("test simplex_proj_mat (dim in [3, 20])", {
  A4 <- simplex_project_mat(4)
  # expected for dim 4 (for examples on wikipedia)
  A4_expected <- matrix(c(1,0,0,
                          -1/3, sqrt(8)/3,0,
                          -1/3,-sqrt(2)/3, sqrt(2/3),
                          -1/3, -sqrt(2)/3, -sqrt(2/3)),
                        ncol = 4)

  testthat::expect_equal(A4, A4_expected)

  # checks for any dim:
  for (. in 1:5){
    p <- sample(3:20, size = 1)
    Ap <- simplex_project_mat(p)
    testthat::expect_equal(diag(t(Ap) %*% (Ap)), rep(1,p))
    testthat::expect_equal(t(Ap) %*% (Ap) + diag(-p/(p-1), nrow = p, ncol = p),
                           matrix(- 1/(p-1), nrow = p, ncol = p))
  }
})

test_that("test to_lower_simplex", {
  # just getting the same thing back
  for (. in 1:5){
    dim <- sample(3:20, size = 1)

    df <- diag(dim)
    df_project <- to_lower_simplex(df)
    testthat::expect_equivalent(df_project %>% t,
                                simplex_project_mat(ncol(df)))
  }

  # just scale (with a fake A)
  df <- data.frame(matrix(rnorm(100)^2, ncol = 4))
  names(df) <- paste("V", 1:4)
  A_fake <- diag(4)
  df_scale <- to_lower_simplex(df, A_fake)

  testthat::expect_equivalent(df_scale, df / rowSums(df))

  # error
  df <- rbind(x = -1, y = 0, z = 0, w = 0)
  testthat::expect_error(to_lower_simplex(df, A_fake))
})

test_that("print for delta_ball_structure and convex_hull_structure",{
  smart_baby_tri <- data.frame(x = c(.5, .5, 0),
                               y = c(.5, 0, .5),
                               z = c(0, .5, .5))
  ch_tri <- create_convex_hull_structure(smart_baby_tri)
  db_tri <- create_delta_ball_structure(smart_baby_tri)

  testthat::expect_output(print(ch_tri))
  testthat::expect_output(print(db_tri))

})

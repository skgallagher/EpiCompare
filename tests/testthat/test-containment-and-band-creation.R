context("containment and band creation tests")

test_that("test filament_compression", {
  # basic check:
  t13compression <- timeternR::pomp_sir %>%
    arrange(time) %>%
    select(-H, -cases, -time) %>%
    filter(.id <= 5) %>%
    group_by(.id) %>%
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

  test_compression <- grouped_df %>% arrange(t) %>%
    select(-t) %>%
    group_by(.id) %>%
    filament_compression(number_points = 9)

  unique_values_per <- test_compression %>%
    summarize(count = n()) %>% pull(count) %>% unique

  testthat::expect_equal(unique_values_per, 9)
  # group1:

  still_linear <- test_compression %>% filter(.id == 1) %>%
    ungroup(.id) %>% select(-.id) %>%
    apply(1, function(row) length(unique(row)) == 1)
  testthat::expect_equal(still_linear, rep(TRUE, 9))

  # group2:
  xx2 <- test_compression %>% filter(.id == 2) %>%
    ungroup(.id) %>%
    pull(x)
  testthat::expect_equal(xx2, (0:8)*2)

  # group3:
  xx3 <- test_compression %>% filter(.id == 3) %>%
    ungroup(.id) %>% pull(x)
  yy3 <- test_compression %>% filter(.id == 3) %>%
    ungroup(.id) %>% pull(y)

  testthat::expect_equal(xx2, xx3)
  testthat::expect_equal(yy3, c((0:4)*2,(3:0)*2))

})

test_that("filament_distance_depth correct depth",{
  dd_pomp_df <-  timeternR::pomp_df %>% group_by(.id) %>%
    filter(.id <= 10) %>%
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

test_that("test grab_top_depth_filaments", {
  top_filaments <- timeternR::pomp_df %>% group_by(.id) %>%
    filter(.id <= 10) %>%
    grab_top_depth_filaments(data_columns =c("S","I","R"),
                             alpha_level = .5)

  testthat::expect_equal(length(unique(top_filaments$.id)), 5)

  all_but_exteme_filaments <- timeternR::pomp_df %>% group_by(.id) %>%
    filter(.id <= 10) %>%
    grab_top_depth_filaments(data_columns = c("S","I","R"),
                             alpha_level = 0)

  testthat::expect_equal(length(unique(all_but_exteme_filaments$.id)),
                         8)

  testthat::expect_error(timeternR::pomp_df %>% group_by(.id) %>%
                           filter(.id <= 10) %>%
                           grab_top_depth_filaments(
                             data_columns =c("S","I","R"),
                             alpha_level = 1))

})

test_that("test create_delta_ball_structure (little because it's a wrapper)", {
  set.seed(1)
  data_points <- data.frame(matrix(rnorm(100), ncol = 4))

  db_structure <- data_points %>%
    create_delta_ball_structure(data_columns = paste0("X",1:4))
  expected_delta <- get_delta(db_structure)$mm_delta

  testthat::expect_equal(attr(db_structure, "delta"), expected_delta)

  # repeat points (shouldn't do anything)
  data_double <- rbind(data_points, data_points)
  db_structure2 <- data_double %>%
    create_delta_ball_structure(data_columns = paste0("X",1:4))

  testthat::expect_equal(db_structure2, db_structure)

})

test_that("test create_convex_hull_structure", {
  box_data <- data.frame(x = c(0,0,1,1,.5),
                         y = c(0,1,0,1,.5))
  just_box1 <- create_convex_hull_structure(box_data)

  just_box2 <- create_convex_hull_structure(box_data,
                                            data_columns = c("x", "y"))

  testthat::expect_equivalent(just_box1, just_box2)
  testthat::expect_equivalent(just_box1 %>% arrange(x,y),
                              box_data[-5,] %>% arrange(x,y))

})

test_that("test hausdorff_dist", {
  # erroring (different structures)
  box_data <- data.frame(x = c(0,0,1,1,.5),
                         y = c(0,1,0,1,.5))
  ch_box <- create_convex_hull_structure(box_data)
  db_box <- create_delta_ball_structure(box_data[-5,])

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
  ch_box2 <- box2 %>% create_convex_hull_structure()
  testthat::expect_true(all.equal(hausdorff_dist(ch_box2, ch_box),
                                  sqrt(.1^2+.1^2)))
  # 2 db boxes
  box2 <- box_data[-5,] + c(3,3)
  db_box2 <- box2 %>% create_delta_ball_structure()
  testthat::expect_true(all.equal(hausdorff_dist(db_box2, db_box),
                                  sqrt(3^2+3^2) - attr(db_box,"delta")))
})

test_that("test contained.delta_ball_structure", {
  box_data <- data.frame(x = c(0,0,1,1,.5),
                         y = c(0,1,0,1,.5))
  db_box <- create_delta_ball_structure(box_data)
  # point outside
  point <- data.frame(x = c(0), y = c(2))

  testthat::expect_false(contained(db_box, point))

  # point inside
  point <- data.frame(x = 0, y = .5)
  testthat::expect_true(contained(db_box, point))
})

test_that("test contained.convex_hull_structure", {
  box_data <- data.frame(x = c(0,0,1,1),
                         y = c(0,1,0,1))
  ch_box <- create_convex_hull_structure(box_data)
  # point outside
  point <- data.frame(x = c(0), y = c(2))

  testthat::expect_false(contained(ch_box, point))

  # point inside
  point <- data.frame(x = .5, y = .5)
  testthat::expect_true(contained(ch_box, point))
})

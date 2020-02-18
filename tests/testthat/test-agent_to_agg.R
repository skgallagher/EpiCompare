context("tests for agent_go_agg")

test_that("check_min_max_time", {
  # correct formats
  mm_t <- c(0, 1)
  testthat::expect_true(check_min_max_time(mm_t))
  mm_t <- c(1, NA)
  testthat::expect_true(check_min_max_time(mm_t))

  # incorrect formats
  mm_t <- c(NA, 1)
  testthat::expect_error(check_min_max_time(mm_t))
  mm_t <- c(1, 0)
  testthat::expect_error(check_min_max_time(mm_t))
  mm_t <- 1
  testthat::expect_error(check_min_max_time(mm_t))
  mm_t <- 1:3
  testthat::expect_error(check_min_max_time(mm_t))
})



test_that("check_ordered", {
  # error in ordering
  df_not_ordered <- data.frame(group1 = 1:5,
                               group2 = c(2:5,1))
  states <- c("group1", "group2")
  testthat::expect_error(check_ordered(df_not_ordered, states))

  output <- check_ordered(df_not_ordered, states,assert_error = F)
  testthat::expect_equal(output$summary_df,
                         data.frame(ordering = c("group1 <= group2",
                                                 "group2 <= group1"),
                                    error = c(F, T),
                                    count = c(4L, 1L)))
  testthat::expect_equal(output$ordering_df,
                         data.frame(id = factor(1:5),
                                    error = c(rep(FALSE, 4), TRUE),
                                    ordering =  c(rep("group1 <= group2", 4),
                                                  "group2 <= group1")))

  # no error in ordering
  df_ordered <- data.frame(group1 = 1:5,
                           group2 = c(2:6))
  states <- c("group1", "group2")
  testthat::expect_true(check_ordered(df_ordered, states))
  testthat::expect_true(check_ordered(df_ordered, states,
                                      assert_error = F))

})



test_that("expanding_info", {
  # standard case
  df <- data.frame(state = rep(1:3, each = 2),
                   t = c(-1,0,2,3,3,4),
                   count = c(2,1,1,2,1,2))
  t_min <- -1
  t_max <- 4
  K <- 3

  expanded_df <- expanding_info(df ,t_min, t_max, K)
  testthat::expect_equal(unique(table(expanded_df$t)),
                         length(unique(expanded_df$state)))

  df %>% dplyr::left_join(expanded_df %>% dplyr::mutate(t = as.numeric(t)),
                          by = c("t", "state")) %>%
    dplyr::mutate(similar = count.x == count.y) %>%
    dplyr::pull(similar) %>% all %>%
    testthat::expect_true()

  # special case when only 1 time point
  df <- data.frame(state = 1:2,
                   t = rep(0,2),
                   count = rep(12,2))
  testthat::expect_equal(expanding_info(df, t_min = 0, t_max = 0, K = 2),
                         df %>% dplyr::select(t, state, count) %>%
                           dplyr::mutate(t = as.character(t),
                                         state = as.numeric(state)))
})

test_that("raw_agents_to_aggregate, data_example_s example",{
  data_example_s <- data.frame(group1 = c(.1,0),
                               group2 = c(2.1,2),
                               group3 = c(3.1,3))
  ## three classes
  s_output3 <- raw_agents_to_aggregate(agents = data_example_s,
                                       states = c("group2", "group3"))

  # if start time is 0, should start with 2 group1, and non of the other groups
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 2, X1 = 0, X2 = 0))

  t_start <- sample(s_output3$t,size = 1)
  s_output3.1 <- raw_agents_to_aggregate(agents = data_example_s,
                                         states = c("group2", "group3"),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])



  ## four classes
  s_output4 <- raw_agents_to_aggregate(agents = data_example_s,
                                       states = c("group1","group2", "group3"))

  # if start time is 0, should start with 1 group0, and 1 in group1
  testthat::expect_equivalent(s_output4 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 0, X3 = 0))

  t_start <- sample(s_output4$t,size = 1)
  s_output4.1 <- raw_agents_to_aggregate(agents = data_example_s,
                                         states = c("group1", "group2", "group3"),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  s_output4._neg1 <- raw_agents_to_aggregate(agents = data_example_s,
                                             states = c("group1", "group2",
                                                        "group3"),
                                             min_max_time = c(-1L,NA))

  # if start time is -1, should start with 2 in group1
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 2, X1 = 0, X2 = 0, X3 = 0))

  testthat::expect_equal(s_output4._neg1[2:nrow(s_output4._neg1),],
                         s_output4)
})

test_that("raw_agents_to_aggregate, data_example_e example", {
  data_example_e <- data.frame(group1 = c(-1.1, -.1,-1),
                               group2 = c(2.1,2.1,2),
                               group3 = c(3.1,3.1,3))

  ## three classes
  s_output3 <- raw_agents_to_aggregate(agents = data_example_e,
                                       states = c("group2", "group3"))

  # if start time is 0, should start with 3 group0,
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 3, X1 = 0, X2 = 0))
  testthat::expect_equivalent(s_output3[s_output3$t == 2,] %>%
                                dplyr::select(dplyr::matches("X")),
                              data.frame(X0 = 2, X1 = 1, X2 = 0))
  testthat::expect_equivalent(s_output3[s_output3$t == 3,] %>%
                                dplyr::select(dplyr::matches("X")),
                              data.frame(X0 = 0, X1 = 2, X2 = 1))

  t_start <- sample(s_output3$t,size = 1)
  s_output3.1 <- raw_agents_to_aggregate(agents = data_example_e,
                                         states = c("group2", "group3"),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])

  # four classes

  s_output4 <- raw_agents_to_aggregate(agents = data_example_e,
                                       states = c("group1","group2","group3"),
                                       min_max_time = c(0,NA))

  # if start time is 0, should start with 3 group1,
  testthat::expect_equivalent(s_output4 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 3, X2 = 0, X3 = 0))

  t_start <- sample(s_output4$t,size = 1)
  s_output4.1 <- raw_agents_to_aggregate(agents = data_example_e,
                                         states = c("group1", "group2",
                                                    "group3"),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  #if start time is -1, start with 2 group1, 1 group0
  s_output4._neg1 <- raw_agents_to_aggregate(agents = data_example_e,
                                             states = c("group1","group2",
                                                        "group3"),
                                             min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 2, X2 = 0, X3 = 0))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)

})

test_that("raw_agents_to_aggregate, data_example_i example", {
  data_example_i <- data.frame(group1 = c(NA, -2.1, NA, -2.1, NA, -2),
                               group2 = c(-1.1,-1.1,-.1,-.1,-1,-1),
                               group3 = c(3.1,3.1,3.1,3.1,3,3))

  ## three classes
  s_output3 <- raw_agents_to_aggregate(agents = data_example_i,
                                       states = c("group2", "group3"))

  # if start time is 0, should start with all 6 X1,
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 6, X2 = 0))
  testthat::expect_equivalent(s_output3[s_output3$t == 3,] %>%
                                dplyr::select(dplyr::matches("X")),
                              data.frame(X0 = 0, X1 = 4, X2 = 2))

  t_start <- sample(s_output3$t,size = 1)
  s_output3.1 <- raw_agents_to_aggregate(agents = data_example_i,
                                         states = c("group2", "group3"),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])

  # four classes
  s_output4 <- raw_agents_to_aggregate(agents = data_example_i,
                                       states = c("group1","group2","group3"),
                                       min_max_time = c(0,NA))

  # if start time is 0, should start with 6 X2,
  testthat::expect_equivalent(s_output4 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 0, X2 = 6, X3 = 0))
  # t == 3: 4 X2, 2 X3
  testthat::expect_equivalent(s_output4[s_output4$t == 3,] %>%
                                dplyr::select(dplyr::matches("X")),
                              data.frame(X0 = 0, X1 = 0, X2 = 4, X3 = 2))
  # t == 4: 6 X3
  testthat::expect_equivalent(s_output4[s_output4$t == 4,] %>%
                                dplyr::select(dplyr::matches("X")),
                              data.frame(X0 = 0, X1 = 0, X2 = 0, X3 = 6))

  t_start <- sample(s_output4$t,size = 1)
  s_output4.1 <- raw_agents_to_aggregate(agents = data_example_i,
                                         states = c("group1", "group2",
                                                    "group3"),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  # if start time is -1, start with 2 X2, 2 X1, 2 X0
  # as data_example_i[3,] => X0
  # [4,] => X1
  # rest X2
  s_output4._neg1 <- raw_agents_to_aggregate(agents = data_example_i,
                                             states = c("group1","group2",
                                                        "group3"),
                                             min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 4, X3 = 0))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)
})

test_that("raw_agents_to_aggregate, data_example_r example", {
  data_example_r_cont <- data.frame(group1 = c(-3.1, -3.1, NA , NA  , -3.1, -3.1, NA  , NA),
                                    group2 = c(-2.1, -2.1,-2.1, -2.1,   NA, NA  , NA  , NA),
                                    group3 = c(-1.1, -.1, -1.1, -.1 , -1.1, -.1 , -1.1, -.1))

  data_example_r_disc <- data.frame(group1 = c(-3, NA, -3, NA),
                                    group2 = c(-2, -2, NA, NA),
                                    group3 = c(-1, -1, -1, -1))

  data_example_r <- rbind(data_example_r_cont, data_example_r_disc)

  ## three classes
  s_output3 <- raw_agents_to_aggregate(agents = data_example_r,
                                       states = c("group2", "group3"))

  # if start time is 0, should start with all 12 X2,
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 0, X2 = 12))
  testthat::expect_equivalent(nrow(s_output3),1)

  # four classes
  s_output4 <- raw_agents_to_aggregate(agents = data_example_r,
                                       states = c("group1","group2","group3"),
                                       min_max_time = c(0,NA))

  # if start time is 0, should start with 6 X2,
  testthat::expect_equivalent(s_output4 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 0, X2 = 0, X3 = 12))

  # if start time is -1, start with 8 X3, 2 X2, 1 X1, 1 X0
  # rest in X3
  # data_example([c(1,3,5,7,9:12),]) => X3
  # data_example_r[c(2,4),] => X2
  # data_example_r[6,] => X1
  # data_example_r[8] => X0
  s_output4._neg1 <- raw_agents_to_aggregate(agents = data_example_r,
                                             states = c("group1","group2",
                                                        "group3"),
                                             min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 2, X3 = 8))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)
})

test_that("raw_agents_to_aggregate, births", {
  data_example_s <- data.frame(group1 = c(.1,0),
                               group2 = c(2.1,2),
                               group3 = c(3.1,3))

  data_example_e <- data.frame(group1 = c(-1.1, -.1,-1),
                               group2 = c(2.1,2.1,2),
                               group3 = c(3.1,3.1,3))

  data_example_i <- data.frame(group1 = c(NA, -2.1, NA, -2.1, NA, -2),
                               group2 = c(-1.1,-1.1,-.1,-.1,-1,-1),
                               group3 = c(3.1,3.1,3.1,3.1,3,3))

  data_example_r_cont <- data.frame(group1 = c(-3.1, -3.1, NA , NA  , -3.1, -3.1, NA  , NA),
                                    group2 = c(-2.1, -2.1,-2.1, -2.1,   NA, NA  , NA  , NA),
                                    group3 = c(-1.1, -.1, -1.1, -.1 , -1.1, -.1 , -1.1, -.1))

  data_example_r_disc <- data.frame(group1 = c(-3, NA, -3, NA),
                                    group2 = c(-2, -2, NA, NA),
                                    group3 = c(-1, -1, -1, -1))

  all_data <- rbind(data_example_s,
                    data_example_e,
                    data_example_i,
                    data_example_r_cont,
                    data_example_r_disc) %>%
    dplyr::mutate(birth = rep(c(0,1,NA), length = 23))

  all_out <- raw_agents_to_aggregate(agents = all_data,
                                     birth = "birth",
                                     states = c("group1","group2", "group3"))
  testthat::expect_equal(all_out %>%
                           dplyr::select(dplyr::matches("X")) %>% apply(1, sum),
                         c(23-8,rep(23,4)))

  all_out_no_birth <- raw_agents_to_aggregate(agents = all_data,
                                              states = c("group1","group2",
                                                         "group3"))
  # babies born at t == 0
  # obs 2: g1
  # obs 5: g1
  # obs 8: g2
  # obs 11:g2
  # obs 14:g3
  # obs 17:g3
  # obs 20:g3
  # obs 23:g3
  testthat::expect_equal((all_out_no_birth - all_out)[1,],
                         data.frame(t = 0, X0 = 0, X1 = 2, X2 = 2, X3 = 4))
})


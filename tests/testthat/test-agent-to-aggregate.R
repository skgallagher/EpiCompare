context("tests for agents_to_aggregate")

library(dplyr)
library(tidyr)

test_that("checking agents_to_aggregate works for SI model",{
  
  agents <- data.frame(agent_id = 1:5,
                       S = c(1, 1, 1, 1, NA),
                       I = c(NA, NA, NA, 6, 10))
  
  exp_agg_df <- agents_to_aggregate(agents,
                                    states = "I")
  expect_equal(dim(exp_agg_df), c(11, 3))
  
  
  
  
})

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
  testthat::expect_equivalent(output$summary_df,
                              data.frame(ordering = c("group1 <= group2",
                                                      "group2 <= group1"),
                                         error = c(F, T),
                                         count = c(4L, 1L)))
  df <-  data.frame(id = as.character(1:5),
             error = c(rep(FALSE, 4), TRUE),
             ordering =  c(rep("group1 <= group2", 4),
                           "group2 <= group1"))
  testthat::expect_equal(output$ordering_df,
                         df)

  # no error in ordering
  df_ordered <- data.frame(group1 = 1:5,
                           group2 = c(2:6))
  states <- c("group1", "group2")
  testthat::expect_true(check_ordered(df_ordered, states))
  testthat::expect_true(check_ordered(df_ordered, states,
                                      assert_error = F))

})

test_that("check_ordered, dealing with NAs", {
  # NAs and an error inside
  test_na <- data.frame(sim = 1, agent_id = 1:5,
                        S1 = rep(1,5),
                        I1 = c( 4, 10,  9,  2,  9),
                        R1 = c( 6, 11, 12,  3, 18),
                        S2 = c(16, 13, 48, 57, 46),
                        I2 = c(85, 15, 92, 84, 48),
                        R2 = c(88, 22, 94, 87, 50),
                        S3 = c(NA, 50, NA, 91, 65),
                        I3 = c(NA, NA, NA, NA, 96),
                        R3 = c(NA, NA, NA, NA, 100),
                        S4 = c(NA, NA, NA, 90, NA)
  )
  
  test_na_co_output <- check_ordered(test_na, 
                                     states = paste0(c("S", "I", "R"),
                                                     c(rep(1:3, each = 3),4)),
                                     assert_error = F)
  # this should error
  testthat::expect_true(is.list(test_na_co_output))
  
  
  # ordering df errors have no NAs
  testthat::expect_equal(test_na_co_output$ordering_df$error,
                         c(F,F,F,T,F)) 
  testthat::expect_true(sum(is.na(test_na_co_output$ordering_df$error)) == 0)
  
  
  # summary_df errors have no NAs
  error_same <- test_na_co_output$summary_df %>% 
    dplyr::left_join(data.frame(count = c(4,1), 
                                error = c(F, T)), 
                     by = "count") 
  testthat::expect_equal(error_same$error.x, error_same$error.y) 
  testthat::expect_true(sum(is.na(error_same$error.x)) == 0)
  
  
  # NAs and a completely NA individual
  test_na_full_row <- data.frame(sim = 1, agent_id = 1:6,
                                 S1 = c(rep(1,5), NA),
                                 I1 = c( 4, 10,  9,  2,  9, NA),
                                 R1 = c( 6, 11, 12,  3, 18, NA),
                                 S2 = c(16, 13, 48, 57, 46, NA),
                                 I2 = c(85, 15, 92, 84, 48, NA),
                                 R2 = c(88, 22, 94, 87, 50, NA),
                                 S3 = c(NA, 50, NA, 91, 65, NA),
                                 I3 = c(NA, NA, NA, NA, 96, NA),
                                 R3 = c(NA, NA, NA, NA, 100, NA),
                                 S4 = c(NA, NA, NA, 90, NA, NA)
  )
  test_na_full_row_co_output <- 
    check_ordered(test_na_full_row, 
                  states = paste0(c("S", "I", "R"), 
                                  c(rep(1:3, each = 3),4)),
                  assert_error = F)
  
  testthat::expect_true(is.na(test_na_full_row_co_output$ordering_df[6,]$error)) 
  #^ if NAs we want ordering to be NA
  
  
})


test_that(paste("check_ordered works with NAs in tibbles,",
                "data.frames and grouped_df"),{
          test_df <- data.frame(agent_id = factor(1:5),
                                sim = 1,
                                tI = c(19, 18, NA, NA, NA),
                                tR = c(35, 27, NA, NA, NA))
          testthat::expect_true(check_ordered(test_df, c("tI", "tR")))


          ## tbl_df version
          test_df <- data.frame(agent_id = factor(1:5),
                                sim = 1,
                                tI = c(19, 18, NA, NA, NA),
                                tR = c(35, 27, NA, NA, NA))
          if (utils::packageVersion("dplyr") >= '1.0.0'){
            test_tbl <- tibble::as_tibble(test_df)
          } else {
            test_tbl <- dplyr::tbl_df(test_df)
          }
          testthat::expect_true(check_ordered(test_tbl, c("tI", "tR")))


          ## grouped_df version
          test_df <- data.frame(agent_id = factor(1:5),
                                sim = rep(factor(c(1:2)), each = 5),
                                tI = c(19, 18, NA, NA, NA),
                                tR = c(35, 27, NA, NA, NA))
          if (utils::packageVersion("dplyr") >= '1.0.0'){
            test_tbl <- tibble::as_tibble(test_df) %>% dplyr::group_by(sim)
          } else {
            test_tbl <- dplyr::tbl_df(test_df) %>% dplyr::group_by(sim)
          }
          
          testthat::expect_true(check_ordered(test_tbl, c("tI", "tR")))

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
  testthat::expect_equivalent(expanding_info(df, t_min = 0, t_max = 0, K = 2),
                              df %>% dplyr::select(t, state, count) %>%
                                 dplyr::mutate(t = as.character(t),
                                               state = as.numeric(state)))
})

test_that("distinct_time", {
  # something interesting
  df <- data.frame(t = 1:100, x = rep(c(1,1,2,2,3,3),
                                      length = 100),
                   y = rep(c(1,1,1,2,2,2,3,3,3),
                           length = 100))
  
  df_filtered <- distinct_time(df, 1)
  testthat::expect_equal(dim(df_filtered), c(67,3))                     
  testthat::expect_equal(df_filtered, 
                         df[!(1:100 %in% cumsum(rep(c(2,4), length = 33))),])
  
  df_all <- distinct_time(df, NULL)
  testthat::expect_equal(df_all, df)
  
  df_x <- distinct_time(df, c(1,3))
  testthat::expect_equal(df_x, df[1:100 %% 2 == 1,])
  
  df_y <- distinct_time(df, c(1,2))
  testthat::expect_equal(df_y, df[1:100 %% 3 == 1,])
  
  # nothing interesting
  df <- data.frame(t =1:100, x = rep(1:3, length = 100),
                   y = rep(1:4, length = 100))
  
  df_filtered <- distinct_time(df, 1)
  testthat::expect_equal(df_filtered, df)
  
  df_all <- distinct_time(df, NULL)
  testthat::expect_equal(df_all, df)
  
  df_x <- distinct_time(df, c(1,3))
  testthat::expect_equal(df_x, df)
  
  df_y <- distinct_time(df, c(1,2))
  testthat::expect_equal(df_y, df)
  
  
  df_single <- distinct_time(df, 1:3)
  testthat::expect_equal(nrow(df_single), 1)
  testthat::expect_equal(df_single, df[1,])
})

test_that("agents_to_aggregate.data.frame, data_example_s example",{
  data_example_s <- data.frame(group1 = c(.1,0),
                               group2 = c(2.1,2),
                               group3 = c(3.1,3))
  ## three classes
  s_output3 <- agents_to_aggregate(agents = data_example_s,
                                       states = c(group2, group3))

  # if start time is 0, should start with 2 group1, and non of the other groups
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 2, X1 = 0, X2 = 0))

  t_start <- sample(s_output3$t,size = 1)
  s_output3.1 <- agents_to_aggregate(agents = data_example_s,
                                         states = c(group2, group3),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])



  ## four classes
  s_output4 <- agents_to_aggregate(agents = data_example_s,
                                       states = c(group1, group2, group3))

  # if start time is 0, should start with 1 group0, and 1 in group1
  testthat::expect_equivalent(s_output4 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 0, X3 = 0))

  t_start <- sample(s_output4$t,size = 1)
  s_output4.1 <- agents_to_aggregate(agents = data_example_s,
                                         states = c(group1, group2, group3),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  s_output4._neg1 <- agents_to_aggregate(agents = data_example_s,
                                             states = c(group1, group2 ,
                                                        group3),
                                             min_max_time = c(-1L,NA))

  # if start time is -1, should start with 2 in group1
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 2, X1 = 0, X2 = 0, X3 = 0))

  testthat::expect_equal(s_output4._neg1[2:nrow(s_output4._neg1),],
                         s_output4)
})

test_that("agents_to_aggregate.data.frame - string, data_example_s example",{
  data_example_s <- data.frame(group1 = c(.1,0),
                               group2 = c(2.1,2),
                               group3 = c(3.1,3))
  ## three classes
  s_output3 <- agents_to_aggregate(agents = data_example_s,
                                   states = c("group2", "group3"))

  # if start time is 0, should start with 2 group1, and non of the other groups
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 2, X1 = 0, X2 = 0))

  t_start <- sample(s_output3$t,size = 1)
  s_output3.1 <- agents_to_aggregate(agents = data_example_s,
                                     states = c("group2", "group3"),
                                     min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])



  ## four classes
  s_output4 <- agents_to_aggregate(agents = data_example_s,
                                   states = c("group1","group2", "group3"))

  # if start time is 0, should start with 1 group0, and 1 in group1
  testthat::expect_equivalent(s_output4 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 0, X3 = 0))

  t_start <- sample(s_output4$t,size = 1)
  s_output4.1 <- agents_to_aggregate(agents = data_example_s,
                                     states = c("group1", "group2", "group3"),
                                     min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  s_output4._neg1 <- agents_to_aggregate(agents = data_example_s,
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

test_that("agents_to_aggregate.data.frame, data_example_e example", {
  data_example_e <- data.frame(group1 = c(-1.1, -.1,-1),
                               group2 = c(2.1,2.1,2),
                               group3 = c(3.1,3.1,3))

  ## three classes
  s_output3 <- agents_to_aggregate(agents = data_example_e,
                                       states = c(group2, group3))

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
  s_output3.1 <- agents_to_aggregate(agents = data_example_e,
                                         states = c(group2, group3),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])

  # four classes

  s_output4 <- agents_to_aggregate(agents = data_example_e,
                                       states = c(group1, group2, group3),
                                       min_max_time = c(0,NA))

  # if start time is 0, should start with 3 group1,
  testthat::expect_equivalent(s_output4 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 3, X2 = 0, X3 = 0))

  t_start <- sample(s_output4$t,size = 1)
  s_output4.1 <- agents_to_aggregate(agents = data_example_e,
                                         states = c(group1, group2,
                                                    group3),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  #if start time is -1, start with 2 group1, 1 group0
  s_output4._neg1 <- agents_to_aggregate(agents = data_example_e,
                                             states = c(group1, group2,
                                                        group3),
                                             min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 2, X2 = 0, X3 = 0))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)

})

test_that("agents_to_aggregate.data.frame - string, data_example_e example", {
  data_example_e <- data.frame(group1 = c(-1.1, -.1,-1),
                               group2 = c(2.1,2.1,2),
                               group3 = c(3.1,3.1,3))

  ## three classes
  s_output3 <- agents_to_aggregate(agents = data_example_e,
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
  s_output3.1 <- agents_to_aggregate(agents = data_example_e,
                                     states = c("group2", "group3"),
                                     min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])

  # four classes

  s_output4 <- agents_to_aggregate(agents = data_example_e,
                                   states = c("group1","group2","group3"),
                                   min_max_time = c(0,NA))

  # if start time is 0, should start with 3 group1,
  testthat::expect_equivalent(s_output4 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 3, X2 = 0, X3 = 0))

  t_start <- sample(s_output4$t,size = 1)
  s_output4.1 <- agents_to_aggregate(agents = data_example_e,
                                     states = c("group1", "group2",
                                                "group3"),
                                     min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  #if start time is -1, start with 2 group1, 1 group0
  s_output4._neg1 <- agents_to_aggregate(agents = data_example_e,
                                         states = c("group1","group2",
                                                    "group3"),
                                         min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 2, X2 = 0, X3 = 0))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)

})

test_that("agents_to_aggregate.data.frame, data_example_i example", {
  data_example_i <- data.frame(group1 = c(NA, -2.1, NA, -2.1, NA, -2),
                               group2 = c(-1.1,-1.1,-.1,-.1,-1,-1),
                               group3 = c(3.1,3.1,3.1,3.1,3,3))

  ## three classes
  s_output3 <- agents_to_aggregate(agents = data_example_i,
                                       states = c(group2, group3))

  # if start time is 0, should start with all 6 X1,
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 6, X2 = 0))
  testthat::expect_equivalent(s_output3[s_output3$t == 3,] %>%
                                dplyr::select(dplyr::matches("X")),
                              data.frame(X0 = 0, X1 = 4, X2 = 2))

  t_start <- sample(s_output3$t,size = 1)
  s_output3.1 <- agents_to_aggregate(agents = data_example_i,
                                         states = c(group2, group3),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])

  # four classes
  s_output4 <- agents_to_aggregate(agents = data_example_i,
                                       states = c(group1, group2, group3),
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
  s_output4.1 <- agents_to_aggregate(agents = data_example_i,
                                         states = c(group1, group2,
                                                    group3),
                                         min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  # if start time is -1, start with 2 X2, 2 X1, 2 X0
  # as data_example_i[3,] => X0
  # [4,] => X1
  # rest X2
  s_output4._neg1 <- agents_to_aggregate(agents = data_example_i,
                                             states = c(group1, group2,
                                                        group3),
                                             min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 4, X3 = 0))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)
})

test_that("agents_to_aggregate.data.frame - string, data_example_i example", {
  data_example_i <- data.frame(group1 = c(NA, -2.1, NA, -2.1, NA, -2),
                               group2 = c(-1.1,-1.1,-.1,-.1,-1,-1),
                               group3 = c(3.1,3.1,3.1,3.1,3,3))

  ## three classes
  s_output3 <- agents_to_aggregate(agents = data_example_i,
                                   states = c("group2", "group3"))

  # if start time is 0, should start with all 6 X1,
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 6, X2 = 0))
  testthat::expect_equivalent(s_output3[s_output3$t == 3,] %>%
                                dplyr::select(dplyr::matches("X")),
                              data.frame(X0 = 0, X1 = 4, X2 = 2))

  t_start <- sample(s_output3$t,size = 1)
  s_output3.1 <- agents_to_aggregate(agents = data_example_i,
                                     states = c("group2", "group3"),
                                     min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output3.1, s_output3[s_output3$t >= t_start,])

  # four classes
  s_output4 <- agents_to_aggregate(agents = data_example_i,
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
  s_output4.1 <- agents_to_aggregate(agents = data_example_i,
                                     states = c("group1", "group2",
                                                "group3"),
                                     min_max_time = c(t_start,NA))
  testthat::expect_equal(s_output4.1, s_output4[s_output4$t >= t_start,])

  # if start time is -1, start with 2 X2, 2 X1, 2 X0
  # as data_example_i[3,] => X0
  # [4,] => X1
  # rest X2
  s_output4._neg1 <- agents_to_aggregate(agents = data_example_i,
                                         states = c("group1","group2",
                                                    "group3"),
                                         min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 4, X3 = 0))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)
})

test_that("agents_to_aggregate.data.frame, data_example_r example", {
  data_example_r_cont <- data.frame(group1 = c(-3.1, -3.1, NA , NA  , -3.1, -3.1, NA  , NA),
                                    group2 = c(-2.1, -2.1,-2.1, -2.1,   NA, NA  , NA  , NA),
                                    group3 = c(-1.1, -.1, -1.1, -.1 , -1.1, -.1 , -1.1, -.1))

  data_example_r_disc <- data.frame(group1 = c(-3, NA, -3, NA),
                                    group2 = c(-2, -2, NA, NA),
                                    group3 = c(-1, -1, -1, -1))

  data_example_r <- rbind(data_example_r_cont, data_example_r_disc)

  ## three classes
  s_output3 <- agents_to_aggregate(agents = data_example_r,
                                       states = c(group2, group3))

  # if start time is 0, should start with all 12 X2,
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 0, X2 = 12))
  testthat::expect_equivalent(nrow(s_output3),1)

  # four classes
  s_output4 <- agents_to_aggregate(agents = data_example_r,
                                       states = c(group1, group2, group3),
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
  s_output4._neg1 <- agents_to_aggregate(agents = data_example_r,
                                             states = c(group1, group2,
                                                        group3),
                                             min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 2, X3 = 8))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)
})

test_that("agents_to_aggregate.data.frame - string, data_example_r example", {
  data_example_r_cont <- data.frame(group1 = c(-3.1, -3.1, NA , NA  , -3.1, -3.1, NA  , NA),
                                    group2 = c(-2.1, -2.1,-2.1, -2.1,   NA, NA  , NA  , NA),
                                    group3 = c(-1.1, -.1, -1.1, -.1 , -1.1, -.1 , -1.1, -.1))

  data_example_r_disc <- data.frame(group1 = c(-3, NA, -3, NA),
                                    group2 = c(-2, -2, NA, NA),
                                    group3 = c(-1, -1, -1, -1))

  data_example_r <- rbind(data_example_r_cont, data_example_r_disc)

  ## three classes
  s_output3 <- agents_to_aggregate(agents = data_example_r,
                                   states = c("group2", "group3"))

  # if start time is 0, should start with all 12 X2,
  testthat::expect_equivalent(s_output3 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 0, X1 = 0, X2 = 12))
  testthat::expect_equivalent(nrow(s_output3),1)

  # four classes
  s_output4 <- agents_to_aggregate(agents = data_example_r,
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
  s_output4._neg1 <- agents_to_aggregate(agents = data_example_r,
                                         states = c("group1","group2",
                                                    "group3"),
                                         min_max_time = c(-1,NA))
  testthat::expect_equivalent(s_output4._neg1 %>%
                                dplyr::select(dplyr::matches("X")) %>% .[1,],
                              data.frame(X0 = 1, X1 = 1, X2 = 2, X3 = 8))

  testthat::expect_equal(s_output4._neg1[s_output4._neg1$t > -1,],
                         s_output4)
})

test_that("agents_to_aggregate.data.frame, births", {
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

  all_out <- agents_to_aggregate(agents = all_data,
                                     birth = birth,
                                     states = c(group1, group2, group3))
  testthat::expect_equal(all_out %>%
                           dplyr::select(dplyr::matches("X")) %>% apply(1, sum),
                         c(23-8,rep(23,4)))

  all_out_no_birth <- agents_to_aggregate(agents = all_data,
                                              states = c(group1, group2,
                                                         group3))
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

test_that("agents_to_aggregate.data.frame - string, births", {
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

  all_out <- agents_to_aggregate(agents = all_data,
                                 birth = "birth",
                                 states = c("group1","group2", "group3"))
  testthat::expect_equal(all_out %>%
                           dplyr::select(dplyr::matches("X")) %>% apply(1, sum),
                         c(23-8,rep(23,4)))

  all_out_no_birth <- agents_to_aggregate(agents = all_data,
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

test_that("agents_to_aggregate.group_df passes basic checks", {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(dplyr, quietly = TRUE)
    ))
  # similar test as in the example string
  max_time <- 100
  new_raw <- hagelloch_raw %>%
    dplyr::mutate(AGE2 = as.numeric(cut(AGE,3)))
  agents_g <- new_raw %>% dplyr::group_by(AGE2)

  sir_group <- agents_to_aggregate(agents_g, states = c(tI, tR),
                                   min_max_time = c(0, max_time))
  agents <- agents_g %>%
    dplyr::filter(AGE2 == 1) %>% dplyr::ungroup()
  sir_group1 <- agents_to_aggregate(agents, states = c(tI, tR),
                                   min_max_time = c(0, max_time))
  sir_group_1 <- sir_group %>% dplyr::filter(AGE2 == 1)
  testthat::expect_equal(sir_group1,
                         dplyr::select(dplyr::ungroup(sir_group_1),
                                       t, X0, X1, X2))
})

test_that("agents_to_aggregate.group_df -string passes basic checks", {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(dplyr, quietly = TRUE)
    ))
  # similar test as in the example string
  max_time <- 100
  new_raw <- hagelloch_raw %>%
    dplyr::mutate(AGE2 = as.numeric(cut(AGE,3)))
  agents_g <- new_raw %>% dplyr::group_by(AGE2)

  sir_group <- agents_to_aggregate(agents_g, states = c("tI", "tR"),
                                   min_max_time = c(0, max_time))
  agents <- agents_g %>%
    dplyr::filter(AGE2 == 1) %>% dplyr::ungroup()
  sir_group1 <- agents_to_aggregate(agents, states = c("tI", "tR"),
                                    min_max_time = c(0, max_time))
  sir_group_1 <- sir_group %>% dplyr::filter(AGE2 == 1)
  testthat::expect_equal(sir_group1,
                         dplyr::select(dplyr::ungroup(sir_group_1),
                                       t, X0, X1, X2))
})

test_that("agents_to_aggregate.group_df, max_t = NA basic checks", {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(dplyr, quietly = TRUE)
    ))
  # similar test as in the example string
  new_raw <- hagelloch_raw %>%
    mutate(AGE2 = as.numeric(cut(AGE,3)))
  agents_g <- new_raw %>% dplyr::group_by(AGE2)

  max_time_each <- agents_g %>% 
    dplyr::summarize(nmax = ceiling(max(tR)))

  max_time <- max_time_each %>% pull(nmax) %>% max()

  sir_group <- agents_to_aggregate(agents_g, states = c(tI, tR),
                                   min_max_time = c(0, NA))

  # should all share max value if min_max_time[2] = NA
  testthat::expect_equal(sum((sir_group %>% 
                                dplyr::summarize(maxt = max(t)) %>%
                                dplyr::pull(maxt)) == max_time), 3)

  for (age2_value in 1:3){
    agents <- agents_g %>%
      dplyr::filter(AGE2 == age2_value) %>% dplyr::ungroup()
    sir_group_sub_id <- agents_to_aggregate(agents, states = c(tI, tR),
                                            min_max_time = c(0, NA))
    sir_group_sub_g <- sir_group %>% filter(AGE2 == age2_value)

    individual_max_t <- max_time_each$nmax[max_time_each$AGE2 == age2_value]
    testthat::expect_equal(sir_group_sub_id %>% pull(t) %>% max,
                           individual_max_t)

    testthat::expect_equal(sir_group_sub_id,
                           sir_group_sub_g %>% ungroup %>%
                             dplyr::select(t, X0, X1, X2) %>%
                             filter(t <= individual_max_t))

    if (individual_max_t < max_time){
      unique_rows_after_max <- sir_group_sub_g %>% ungroup %>%
        filter(t > individual_max_t) %>%
        dplyr::select(X0, X1, X2)  %>% distinct() %>% nrow()
      testthat::expect_equal(unique_rows_after_max, 1)
    }
  }

})

test_that("agents_to_aggregate.group_df, max_t = NA basic checks, strings", {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(dplyr, quietly = TRUE)
    ))
  # similar test as in the example string
  new_raw <- hagelloch_raw %>%
    mutate(AGE2 = as.numeric(cut(AGE,3)))
  agents_g <- new_raw %>% group_by(AGE2)

  max_time_each <- agents_g %>% summarize(nmax = ceiling(max(tR)))

  max_time <- max_time_each %>% pull(nmax) %>% max()

  sir_group <- agents_to_aggregate(agents_g, states = c("tI", "tR"),
                                   min_max_time = c(0, NA))

  # should all share max value if min_max_time[2] = NA
  testthat::expect_equal(sum((sir_group %>% summarize(maxt = max(t)) %>%
                                pull(maxt)) == max_time), 3)

  for (age2_value in 1:3){
    agents <- agents_g %>%
      filter(AGE2 == age2_value) %>% ungroup()
    sir_group_sub_id <- agents_to_aggregate(agents, states = c("tI", "tR"),
                                            min_max_time = c(0, NA))
    sir_group_sub_g <- sir_group %>% filter(AGE2 == age2_value)

    individual_max_t <- max_time_each$nmax[max_time_each$AGE2 == age2_value]
    testthat::expect_equal(sir_group_sub_id %>% pull(t) %>% max,
                           individual_max_t)

    testthat::expect_equal(sir_group_sub_id,
                           sir_group_sub_g %>% ungroup %>%
                             dplyr::select(t, X0, X1, X2) %>%
                             filter(t <= individual_max_t))

    if (individual_max_t < max_time){
      unique_rows_after_max <- sir_group_sub_g %>% ungroup %>%
        filter(t > individual_max_t) %>%
        dplyr::select(X0, X1, X2)  %>% distinct() %>% nrow()
      testthat::expect_equal(unique_rows_after_max, 1)
    }
  }

})

test_that("agents_to_aggregate + integer_time_expansion = FALSE -- more basic",{
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
  
  mult <- 3
  all_data <- rbind(data_example_s,
                    data_example_e,
                    data_example_i,
                    data_example_r_cont,
                    data_example_r_disc) %>%
    dplyr::mutate(birth = rep(c(0,1,NA), length = 23)) * mult
  
  all_out <- agents_to_aggregate(agents = all_data,
                                 birth = birth,
                                 states = c(group1, group2, group3))
  
  all_out_filter <- agents_to_aggregate(agents = all_data,
                                        birth = birth,
                                        states = c(group1, group2, group3),
                                        integer_time_expansion = FALSE)
  testthat::expect_equal(all_out_filter, all_out[!(all_out$t %in% c(2,4,5,8)),])

})

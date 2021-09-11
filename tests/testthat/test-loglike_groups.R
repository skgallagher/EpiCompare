test_that("get_infector_group", {
  agents_df <- data.frame(agent_id = 1:3,
                          group = c("cat",
                                    "dog", "dog"),
                          infector_id = c(2, 2, 1))
  
  out <- get_infector_group(agents_df)
  expect_equal(out,
               c("dog", "dog", "cat"))
  
  #################
  data("hagelloch_raw2")
  library(dplyr)
  library(tidyr)
  
  hag_df <- hagelloch_raw2 %>%
    select(PN, tI, tR, CL, IFTO)
  hag_df$IFTO[is.na(hag_df$IFTO)] = 3 ## arbitrary assignment
  hag_df <- hag_df %>% rename(infector_id = IFTO,
                              agent_id = PN,
                              group = CL)
  hag_df$infector_group <- get_infector_group(hag_df)
  expect_equal(hag_df$infector_group[1],
                 hag_df$group[45])
  expect_equal(hag_df$infector_group[121], 
               hag_df$group[11])
})


test_that("get_prob_of_inf_3groups",{
  data("hagelloch_raw")
  library(dplyr)
  library(tidyr)
  
  hag_df <- hagelloch_raw %>%
    select(PN, tI, tR, CL, IFTO)
  hag_df$IFTO[is.na(hag_df$IFTO)] = 3 ## arbitrary assignment
  hag_df <- hag_df %>% rename(infector_id = IFTO,
                              agent_id = PN,
                              group = CL)
  
  
  ## Getting aggregate data
  aggregate_hag <- hag_df %>%
    mutate(class = ifelse(group == "1st class",
                          1,
                          ifelse(group == "2nd class",
                                 2, 0))) %>%
    group_by(class) %>%
    agents_to_aggregate(states = c(tI, tR),
                        min_max_time = c(0, 55)) %>%
    rename(group = class) %>%
    select(t, group, X0, X1, X2)
  t0_df <- aggregate_hag %>% filter(t == 0)
  expect_equal(sum(t0_df$X2), 0)
  
  ## testing pivot_wider
  agg_hag_wide <- pivot_wider(aggregate_hag,
                              id_cols = t,
                              names_from = group,
                              values_from = c(X0, X1, X2),
                              names_sep = "")
  par <- c(.2,.1, .05,
           .19, .09, .04,
           .18, .08, .03,
           .02)
  names(par) <- c("beta11", "beta12", "beta13",
                  "beta21", "beta22", "beta23",
                  "beta31", "beta32", "beta33",
                  "gamma")
  out <- get_prob_of_inf_3groups(par,
                                 aggregate_hag)
  expect_equal(nrow(out), 9 * nrow(aggregate_hag) / 3 )
                              
})



test_that("loglike_agent",{   
   ## SO many cases
   ## 
   ## #############################
   ## max T == 5
   ## START SUSCEPTIBLE, IMMEDIATE I, IMMEDIATE R
   t <- 0:5
   tI <- 1
   tR <- 2
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(
     pI[1], pR[2]
   )))

   expect_equal(out, exp_out)
## ###################
   ## #############################
   ## max T == 5
   ## START SUSCEPTIBLE, NO IMMEDIATE I, NO IMMEDIATE R
   t <- 0:5
   tI <- 3
   tR <- 5
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(1-pI[1], 1-pI[2],
     pI[3], 1-pR[4], pR[5]
   )))
   
   expect_equal(out, exp_out)
   ## ###################    
   ## max T == 5, tR > max T
   ## START SUSCEPTIBLE, NO IMMEDIATE I, NO IMMEDIATE R
   t <- 0:5
   tI <- 3
   tR <- 6
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(1-pI[1], 1-pI[2],
                        pI[3], 1-pR[4], 1-pR[5]
   )))
   
   expect_equal(out, exp_out)
   ## ###################  
   ## 
   ## 
   ## 
   expect_equal(out, exp_out)
   ## ###################    
   ## max T == 5, tI = maxT
   ## START SUSCEPTIBLE, NO IMMEDIATE I, NO IMMEDIATE R
   t <- 0:5
   tI <- 5
   tR <- 6
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(1-pI[1], 1-pI[2],
                        1-pI[3], 1-pI[4], pI[5]
   )))
   
   expect_equal(out, exp_out)
   ## ################### 
   ##     ## ###################    
   ## max T == 5, tI > maxT
   ## START SUSCEPTIBLE, NO IMMEDIATE I, NO IMMEDIATE R
   t <- 0:5
   tI <- 6
   tR <- 7
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(1-pI[1], 1-pI[2],
                        1-pI[3], 1-pI[4], 1-pI[5]
   )))
   
   expect_equal(out, exp_out)
   ## ###################  
   ## 
   ## ################### 
   ##     ## ###################    
   ## max T == 5
   ## START SUSCEPTIBLE, IMMEDIATE I, NO IMMEDIATE R
   t <- 0:5
   tI <- 1
   tR <- 3
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(pI[1], 1-pR[2],
                        pR[3]
   )))
   
   expect_equal(out, exp_out)
   ## ###################  
   ##     ## ###################    
   ## max T == 5
   ## START INFECTIOUS  IMMEDIATE R
   t <- 0:5
   tI <- -12
   tR <- 1
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(pR[1]
     )))
   
   expect_equal(out, exp_out)
   ## ###################  
   ## ###################  
   ##     ## ###################    
   ## max T == 5
   ## START INFECTIOUS NO IMMEDIATE R
   t <- 0:5
   tI <- -12
   tR <- 3
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(1 - pR[1],
                        1 - pR[2],
                        pR[3]
   )))
   
   expect_equal(out, exp_out)
   ## ################### 
   ##     ## ###################    
   ## max T == 5
   ## START INFECTIOUS tR = 5
   t <- 0:5
   tI <- -12
   tR <- 5
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(1 - pR[1],
                        1 - pR[2],
                        1-pR[3],
                        1-pR[4],
                        pR[5]
   )))
   
   expect_equal(out, exp_out)
   ## ################### 
   ## ################### 
   ##     ## ###################    
   ## max T == 5
   ## START INFECTIOUS tR = 6
   t <- 0:5
   tI <- -12
   tR <- 6
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- sum(log(c(1 - pR[1],
                        1 - pR[2],
                        1-pR[3],
                        1-pR[4],
                        1-pR[5]
   )))
   
   expect_equal(out, exp_out)
   ## ################### 
   ## ################### 
   ##     ## ###################    
   ## max T == 5
   ## START RECOVERED
   t <- 0:5
   tI <- -12
   tR <- 0
   pI <- seq(.20, .5, length = length(t))
   pR <- seq(.10, .25, length = length(t))
   out <- loglike_agent(t,
                        tI,
                        tR,
                        pI,
                        pR)
   exp_out <- 0
   
   expect_equal(out, exp_out)

})

test_that("loglike_3groups_internal",{
  data("hagelloch_raw")
  library(dplyr)
  library(tidyr)
  
  hag_df <- hagelloch_raw %>%
    select(PN, tI, tR, CL, IFTO)
  hag_df$IFTO[is.na(hag_df$IFTO)] = 3 ## arbitrary assignment
  hag_df <- hag_df %>% rename(infector_id = IFTO,
                              agent_id = PN,
                              group = CL) %>%
    mutate(group = ifelse(group == "1st class",
                          1,
                          ifelse(group == "2nd class",
                                 2, 0)))
  
  
  ## Getting aggregate data
  aggregate_hag <- hag_df %>%
    group_by(group) %>%
    agents_to_aggregate(states = c(tI, tR),
                        min_max_time = c(0, 55)) %>%
    select(t, group, X0, X1, X2)
  t0_df <- aggregate_hag %>% filter(t == 0)
  expect_equal(sum(t0_df$X2), 0)
  
  par <- c(.2,.1, .05,
           .19, .09, .04,
           .18, .08, .03,
           .02)
  names(par) <- c("beta11", "beta12", "beta13",
                  "beta21", "beta22", "beta23",
                  "beta31", "beta32", "beta33",
                  "gamma")
  
  out <- EpiCompare:::loglike_3_groups_internal(par,
                                   agents_df = hag_df,
                                   agg_df = aggregate_hag)
  
  expect_true(out > 0)
  
  # 
  # opt <- optim(par = par,
  #              loglike_3_groups_internal,
  #              agents_df = hag_df,
  #              agg_df = aggregate_hag,
  #              method = "L-BFGS-B",
  #              lower = .01, upper = .999)
  # 
  # betas <- matrix(opt$par[-10], nrow =3, byrow = TRUE)
  # gamma <- opt$par[10]
  
  
})




       
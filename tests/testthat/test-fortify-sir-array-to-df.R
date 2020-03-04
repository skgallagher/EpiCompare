
test_that("sim_arr_to_df", {

    n_agents <- 4
    n_sims <- 2
    k <- 3
    arr <- array(0, dim = c(n_sims, k, n_agents))
    dimnames(arr) <-  list(sim = 1:n_sims,
                                agents_stat = c("init_state", "tI",
                                                "tR"),
                                agent_id = 1:n_agents)

    ## agent 1
    arr[,1,1] <- 1 # init state
    arr[,2,1] <- c(0, 0) #tI
    arr[,3,1] <- c(NA, 3) #tR
    ## agent 2
    k <- 2
    arr[,1,k] <- 0 # init state
    arr[,2,k] <- c(2, 1) #tI
    arr[,3,k] <- c(3, 3) #tR
    ## agent 3
    k <- 3
    arr[,1,k] <- 0 # init state
    arr[,2,k] <- c(NA, 2) #tI
    arr[,3,k] <- c(NA, 4) #tR
    ## agent 4
    k <- 4
    arr[,1,k] <- 0 # init state
    arr[,2,k] <- c(1, 2) #tI
    arr[,3,k] <- c(3, 4) #tR
    

    out <- sim_arr_to_df(arr)
    expect_equal(sum(is.na(out$tI)), 1)
    expect_equal(sum(is.na(out$tR)), 2)
    expect_true(with(out, all(is.na(tI) | is.na(tR) | (tI <= tR))))

    



})



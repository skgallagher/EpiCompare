## TESTING for simulate-agents

test_that("simulate_agents", {
    ## This is the SIR representation
    trans_mat <- matrix(c("X0 * (1 - X1 * par1 / N)", "X0 * X1  * par1 / N", "0",
                      "0", "X1 * (1 - par2)", "par2 * X1",
                      "0", "0", "X2"), byrow = TRUE, nrow = 3)
    rownames(trans_mat) <- c("S", "I", "R")
    init_vals <- c(950, 50, 0)
    par_vals <- c(par1 = .1, par2 = .03)
    max_T <- 2
    n_sims <- 5

    out <- simulate_agents(trans_mat,
                           init_vals,
                           par_vals,
                           max_T,
                           n_sims,
                           verbose = FALSE)

    expect_true(ncol(out) >= 5)
    
    

})



test_that("initialize_agent_array",{
    
    n_sims <- 5
    max_T <- 3
    init_vals <- c(2, 4)
    birth_dates <- NULL
    birth_states = NULL

    out <- initialize_agent_array(init_vals,
                                  max_T,
                                  n_sims,
                                  birth_dates,
                                  birth_states)
    expect_equal(out[1,,], out[2,,])
    expect_equal(out[3,,], out[2,,])
    expect_equal(out[3,,], out[4,,])
    expect_equal(out[5,,], out[4,,])
    expect_equal(out[1,,1], c(0,0, 1, 1, 1,1))

    ##
    birth_dates <- c(0, 0, 1)
    birth_states <- c(0, 0, 1)
    n_sims <- 2
    n_agents <- length(birth_dates)
    max_T <- 3
    out <- initialize_agent_array(NULL,
                                  max_T,
                                  n_sims,
                                  birth_dates,
                                  birth_states)

    expect_equal(out[1,,], out[2,,])
    expect_equal(out[1,,], matrix(c(0, -1, -1, -1,
                                    0, -1, -1, -1,
                                    -1, 1, -1, -1),
                                  byrow = TRUE, ncol = 4))
    
    

})


test_that("get_previous_counts", {
    agent_states <- c(-1, 1, 2, 5, 2, 2)
    n_states <- 6
    out <- get_previous_counts(agent_states, n_states)
    expect_equal(out, c(0, 1, 3, 0, 0, 1))
    ## #############################3
    agent_states <- c(-1, -1, 0, 0, 1, 1)
    n_states <- 2
    out <- get_previous_counts(agent_states, n_states)
    expect_equal(out, c(2, 2))
    ## #################################
    agent_states <- c(-1, -1)
    n_states <- 2
    out <- get_previous_counts(agent_states, n_states)
    expect_equal(out, c(0, 0))
    ###################################
    

})


test_that("draws_to_states", {

    agent_states <- c(-1, -1, 0, 0, 1)
    draw_mat <- matrix(c(0, 2,
                         0, 1), byrow = TRUE, nrow = 2)
    out <- draws_to_states(agent_states, draw_mat)
    expect_equal(out, c(-1, -1, 1, 1, 1))
    ## ################
    agent_states <- c(-1, -1, 0, 0, 1)
    draw_mat <- matrix(c(1, 1,
                         0, 1), byrow = TRUE, nrow = 2)
    out <- draws_to_states(agent_states, draw_mat)
    expect_true(all(out == c(-1, -1, 1, 0, 1)) |
                all(out == c(-1, -1, 0, 1, 1)))
    ## ################

    
})

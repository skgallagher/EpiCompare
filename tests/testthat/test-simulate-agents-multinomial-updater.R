## SKG
## May 15, 2020



test_that("multinomial_updater", {
    ## This is the SIR representation
    D_mat <- matrix(c("X0 * (1 - X1 * par1 / N)", "X0 * X1  * par1 / N", "0",
                      "0", "X1 * (1 - par2)", "par2 * X1",
                      "0", "0", "0"), byrow = TRUE, nrow = 3)
    K <- 3
    n_pars <- 2
    ii <- 2
    test_list <- trans_mat_to_D_fxn(D_mat, K = 3, n_pars = 2)
    test_mat <- matrix(0, nrow =  K, ncol = K)
    X0 <- 9
    X1 <- 1
    N <- 10
    state_counts <- c(X0, X1, 0)
    par1 <- 0
    par2 <- 1
    par_vals <- c(par1, par2)
  
    out <- multinomial_updater(state_counts,
                               par_vals,
                               test_list)

      
    expect_equal(out[1,1], 9)
    expect_equal(out[2, 3], 1)

    ## ## DOUBLE SIR with pooled infections
    ## X0 - S1, X1 - S2
    ## X2 - I
    ## X3 - R
    D_mat <- matrix(c("X0 * (1 - X2 * par1 / N)", "0", "X0 * X2  * par1 / N", "0",
                      "0", "X1 * (1 - X2 * par2 / N)", "X1 * X2 * par2 / N",  "0",
                      "0", "0", "X2 * (1 - par3)", "par3 * X2",
                      "0", "0", "0", "0"), byrow = TRUE, nrow = 4)
    K <- 4
    n_pars <- 3
    test_list <- trans_mat_to_D_fxn(D_mat, K = K, n_pars = n_pars)
    test_mat <- matrix(0, nrow =  K, ncol = K)
    X0 <- 5
    X1 <- 4
    X2 <- 1
    X3 <- 0
    N <- 10
    par1 <- 0
    par2 <- 0
    par3 <- 1
    state_counts <- c(X0, X1, X2, X3)
    par_vals <- c(par1, par2, par3)
  
    out <- multinomial_updater(state_counts,
                               par_vals,
                               test_list)

    expect_equal(out[1,1], 5)
    expect_equal(out[2, 2], 4)
    expect_equal(out[3,4], 1)


    ## #######################
    ## arbitrary beta gamma
    ## This is the SIR representation
    D_mat <- matrix(c("X0 * (1 - X1 * par1 / N)", "X0 * X1  * par1 / N", "0",
                      "0", "X1 * (1 - par2)", "par2 * X1",
                      "0", "0", "0"), byrow = TRUE, nrow = 3)
    K <- 3
    n_pars <- 2
    ii <- 2
    test_list <- trans_mat_to_D_fxn(D_mat, K = 3, n_pars = 2)
    test_mat <- matrix(0, nrow =  K, ncol = K)
    X0 <- 990
    X1 <- 10
    N <- 1000
    state_counts <- c(X0, X1, 0)
    par1 <- .4
    par2 <- .2
    par_vals <- c(par1, par2)
  
    out <- multinomial_updater(state_counts,
                               par_vals,
                               test_list)
    expect_equal(rowSums(out), state_counts)

})

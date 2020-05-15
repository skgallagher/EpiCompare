## SKG
## May 15, 2020


test_that("trans_mat_to_D_fxn", {
    ## This is the SIR representation
    D_mat <- matrix(c("0", "X0 * X1  * par1 / N", "0",
                      "0", "0", "par2 * X1",
                      "0", "0", "0"), byrow = TRUE, nrow = 3)
    K <- 3
    n_pars <- 2
    ii <- 2
    test_list <- trans_mat_to_D_fxn(D_mat, K = 3, n_pars = 2)
    test_mat <- matrix(0, nrow =  K, ncol = K)
    X0 <- 9
    X1 <- 1
    N <- 10
    par1 <- .5
    par2 <- .25
    for(ii in 1:K){
        for(jj in 1:K){
            arg_list <- list(X0, X1,  par1, par2, N)
            names(arg_list) <- c("X0", "X1", "par1", "par2", "N")
            f <- test_list[[K * (ii-1) + jj]]
            test_mat[ii,jj] <- do.call(f, arg_list)
            
        }
    }
    expect_equal(test_mat[1, 2], 9 * .5 /10)
    expect_equal(test_mat[2,3], .25)

    ## ## DOUBLE SIR with pooled infections
    ## X0 - S1, X1 - S2
    ## X2 - I
    ## X3 - R
    D_mat <- matrix(c("0", "0", "X0 * X2  * par1 / N", "0",
                      "0", "0", "X1 * X2 * par2 / N",  "0",
                      "0", "0", "0", "par3 * X2",
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
    par1 <- .5
    par2 <- .8
    par3 <- .1
    arg_list <- list(X0, X1, X2, X3,  par1, par2, par3, N)
    names(arg_list) <- c("X0", "X1", "X2", "X3",
                         "par1", "par2", "par3",
                         "N")
    for(ii in 1:K){
        for(jj in 1:K){

            f <- test_list[[K * (ii-1) + jj]]
            test_mat[ii,jj] <- do.call(f, arg_list)
            
        }
    }
    expect_equal(test_mat[1, 3], .5 * 5  /10)
    expect_equal(test_mat[2,3], .8 * 4 / 10)


})

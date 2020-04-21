## SKG
## April 16, 2020
## Tests for chain binomial SIR imputation

test_that("cases_to_SIR", {
    df <- data.frame(t = 0:4,
                     confirmed = c(0, 1, 3, 9, 9),
                     N = 10)
    out <- cases_to_SIR(data = df,
                        par = 1)
    exp_S <- c(10, 9, 7, 1, 1)
    exp_I <- c(0, 1, 2, 6, 0)
    exp_R <- c(0, 0, 1, 3, 9)
    expect_equal(out$X0, exp_S)
    expect_equal(out$X1, exp_I)
    expect_equal(out$X2, exp_R)
    expect_equal(out$X2 + out$X1, out$confirmed)
    expect_true(all(out$X1 >= 0))
    expect_true(all(out$X2 >= 0))
    expect_true(all((out$X2 - dplyr::lag(out$X2) >= 0) |
                    is.na(dplyr::lag(out$X2))))
    ## ########################################
    df <- data.frame(t = 0:4,
                     confirmed = c(0, 1, 3, 9, 9),
                     N = 10)
    out <- cases_to_SIR(data = df,
                        par = .8)
    expect_equal(out$X2 + out$X1, out$confirmed)
    expect_true(all(out$X1 >= 0))
    expect_true(all(out$X2 >= 0))
    expect_true(all((out$X2 - dplyr::lag(out$X2) >= 0) |
                    is.na(dplyr::lag(out$X2))))
    ## GROUPED DF ##########
     df1 <- data.frame(t = 0:4,
                     confirmed = c(0, 1, 3, 9, 9),
                     N = 10,
                     group = 1)
    df2 <- data.frame(t = 0:4,
                      confirmed = c(0, 3, 3, 4, 4),
                      N = 9, group = 2)
    df <- dplyr::bind_rows(df1, df2) %>% dplyr::group_by(group)
    out <- cases_to_SIR(df, par = 1)
    expect_equal(out$X2 + out$X1, out$confirmed)
    expect_true(all(out$X1 >= 0))
    expect_true(all(out$X2 >= 0))
    expect_equal(out$X0 + out$X1 + out$X2, out$N)
    
})

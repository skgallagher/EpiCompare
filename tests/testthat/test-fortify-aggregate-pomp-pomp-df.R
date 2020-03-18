test_that("fortify_aggregate.pomp_df()", {

    pomp_output <- pomp_pomp
    df <- as.data.frame(pomp_output)
    class(df) <- c("pomp_df", class(df))
    out <- fortify_aggregate.pomp_df(df, states = NULL)
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2", "X3"))
###
    pomp_output <- pomp_pomp
    df <- as.data.frame(pomp_output)
    class(df) <- c("pomp_df", class(df))
    out <- fortify_aggregate.pomp_df(df, states = c("S", "I", "R"))
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    ##
    pomp_output <- pomp_pomp
    df <- as.data.frame(pomp_output)
    class(df) <- c("pomp_df", class(df))
    out2 <- fortify_aggregate.pomp_df(df, states = c(S, I, R))
    expect_equal(colnames(out2), c("t", "sim", "X0", "X1", "X2"))
    expect_equal(out, out2)
})

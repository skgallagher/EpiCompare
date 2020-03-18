test_that("fortify_aggregate.data.frame", {

    df <- pomp_df
    out <- fortify_aggregate(df, states = NULL, package_source = "pomp")
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2", "X3"))
    ##
    out <- fortify_aggregate(df, states = c("S", "I", "R"), package_source= "pomp")
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    ##
    out2 <- fortify_aggregate(df, states = c("S", "I", "R"), package_source= "pomp")
    expect_equal(colnames(out2), c("t", "sim", "X0", "X1", "X2"))
    expect_equal(out, out2)

})

test_that("fortify_aggregate_ext.data.frame", {

    df <- pomp_df
    out <- fortify_aggregate_ext(df, states = NULL, package_source = "pomp")
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2", "X3"))
    ##
    out <- fortify_aggregate_ext(df, states = c("S", "I", "R"), package_source= "pomp")
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))

})

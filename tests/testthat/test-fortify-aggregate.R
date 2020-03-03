test_that("fortify_aggregate_ext", {
    ## ICM
    out <- fortify_aggregate_ext(EpiModel_icm)
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    ## DCM

})

test_that("fortify_aggregate", {
    ## ICM
    out <- fortify_aggregate(EpiModel_icm)
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    ## DCM
    out3 <- fortify_aggregate(EpiModel_det)
    expect_equal(colnames(out3), c("t", "sim", "X0", "X1", "X2"))
    expect_equal(rowSums(out3[, 3:5]),  rep(1000.0, 300))
    ## POMP (POMP_LIST)
    out <- fortify_aggregate(pomp_pomp, states = NULL)
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2", "X3"))
    ##
    out <- fortify_aggregate(pomp_pomp, states = c("S", "I", "R"))
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    ## POMP

})

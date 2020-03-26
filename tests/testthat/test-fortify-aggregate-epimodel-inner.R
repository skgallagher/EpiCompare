test_that("fortify_aggregate.epimodel_inner", {

    out <- fortify_aggregate(EpiModel_icm)
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    expect_true(all(rowSums(out[, 3:5]) == 1000))

    ############3
    out2 <- fortify_aggregate(EpiModel_agg_bd)
    expect_equal(colnames(out2), c("t", "sim", "X0", "X1", "X2"))

    ########
    out3 <- fortify_aggregate.icm(EpiModel_det)
    expect_equal(colnames(out3), c("t", "sim", "X0", "X1", "X2"))
    expect_equal(rowSums(out3[, 3:5]),  rep(1000.0, 300))
    #############
    #############
    out <- fortify_aggregate(EpiModel_icm, states = c("s.num", "i.num"))
    expect_equal(colnames(out), c("t", "sim", "X0", "X1"))
    out2 <- fortify_aggregate(EpiModel_icm, states = c(s.num, i.num))
    expect_equal(colnames(out2), c("t", "sim", "X0", "X1"))
    expect_equal(out, out2)

})

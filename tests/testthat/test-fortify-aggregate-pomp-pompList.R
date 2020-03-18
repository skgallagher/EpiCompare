test_that("fortify_aggregate.pompList()", {

    out <- fortify_aggregate(pomp_pomp, states = NULL)
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2", "X3"))
    ##
    out <- fortify_aggregate(pomp_pomp, states = c("S", "I", "R"))
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    ##
    out2 <- fortify_aggregate(pomp_pomp, states = c(S, I, R))
    expect_equal(colnames(out2), c("t", "sim", "X0", "X1", "X2"))
    expect_equal(out, out2)

})

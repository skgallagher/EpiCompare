test_that("fortify_aggregate.list", {

    my_list <- pomp_arr
    out <- fortify_aggregate(my_list, states = NULL, package_source = "pomp")
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2", "X3"))
    ##
    out <- fortify_aggregate(my_list, states = c("S", "I", "R"), package_source= "pomp")
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    ##
    out2 <- fortify_aggregate(my_list, states = c(S, I, R), package_source= "pomp")
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    expect_equal(out, out2)

})

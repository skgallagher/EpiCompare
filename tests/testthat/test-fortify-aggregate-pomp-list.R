test_that("fortify_aggregate.pomp_list", {

    my_list <- pomp_arr
    class(my_list) <- c("pomp_list", class(my_list))
    out <- fortify_aggregate(my_list, states = NULL)
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2", "X3"))
    ##
    out <- fortify_aggregate(my_list, states = c("S", "I", "R"))
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2"))
    ##
    out2 <- fortify_aggregate(my_list, states = c(S, I, R))
    expect_equal(colnames(out2), c("t", "sim", "X0", "X1", "X2"))
    expect_equal(out, out2)

})

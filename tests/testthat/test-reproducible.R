
test_that("reproducible example for Ben (df version)", {

    test_df <- data.frame(agent_id = factor(1:5),
                          sim = 1,
                          tI = c(19, 18, NA, NA, NA),
                          tR = c(35, 27, NA, NA, NA))
    out <- agents_to_aggregate(test_df, c(tI, tR))
    expect_true("X0" %in% colnames(out))

})



test_that("reproducible example for Ben (tbl_df version)", {

        test_df <- data.frame(agent_id = factor(1:5),
                          sim = rep(factor(c(1:2)), each = 5),
                          tI = c(19, 18, NA, NA, NA),
                          tR = c(35, 27, NA, NA, NA))
        test_tbl <- dplyr::tbl_df(test_df)
        out <- agents_to_aggregate(test_tbl, c(tI, tR))
        expect_true("X0" %in% colnames(out))

})


test_that("reproducibile example for Ben (grouped_df version)", {

    test_df <- data.frame(agent_id = factor(1:5),
                          sim = rep(factor(c(1:2)), each = 5),
                          tI = c(19, 18, NA, NA, NA),
                          tR = c(35, 27, NA, NA, NA))
    test_tbl <- dplyr::tbl_df(test_df) %>% dplyr::group_by("sim")
    out <- agents_to_aggregate(test_tbl, c(tI, tR))
    expect_true("X0" %in% colnames(out))


})

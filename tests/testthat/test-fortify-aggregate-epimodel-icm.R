test_that("fortify_aggregate.icm", {
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

test_that("get_epimodel_icm_states() works", {
    data <- list("epi" = NULL)
    data$epi <- list("cat" = 7,
                 "a.num" = 14,
                 "ds.num" = 3)
    out <- get_epimodel_icm_states(data)
    expect_equal(out, c("a.num"))

    data <- list("epi" = NULL)
    data$epi <- list("cat" = 7,
                     "s.num" = 14,
                     "i.num" = 3,
                     "r.num" = 7)
    out <- get_epimodel_icm_states(data)
    expect_equal(out, c("s.num",
                        "i.num", "r.num"))
})


test_that("extract_icm_cols", {
    out <- extract_icm_cols(nm = "s.num",
                            ii = 0, EpiModel_icm$epi)
    expect_equal(colnames(out), c("sim", "X0"))

    ## Testing out the gather function
    mat <- EpiModel_icm$epi[[1]]
    ii <- 0
    df <- tidyr::gather(as.data.frame(mat),
                             key = "sim") %>%
        dplyr::rename(!!paste0("X", ii) := value)
    expect_equal(dim(df), dim(out))
})



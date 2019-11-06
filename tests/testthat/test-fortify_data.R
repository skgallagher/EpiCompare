test_that("fortify_pomp()", {
  data("pomp_df")
  out <- fortify_pomp(pomp_df)
  expect_equal(ncol(out), 5)
  expect_true(all(c("t", "S", "I", "R", "sim") %in% colnames(out)))
#######
  data("pomp_pomp")
  out2 <- fortify_pomp(pomp_pomp)
  expect_equal(ncol(out2), 5)
  expect_true(all(c("t", "S", "I", "R", "sim") %in% colnames(out2)))
####
  data("pomp_arr")
  out3 <- fortify_pomp(pomp_arr)
  expect_equal(ncol(out3), 5)
  expect_true(all(c("t", "S", "I", "R", "sim") %in% colnames(out3)))


})


test_that("fortify_EpiModel()", {

  data(EpiModel_det)

  out <- fortify_EpiModel(EpiModel_det)
  expect_equal(class(out), "data.frame")
  expect_equal(dim(out), c(300,4))

  ## THE ICM
  data(EpiModel_icm)
  out <- fortify_EpiModel(EpiModel_icm)
  expect_equal(class(out), "data.frame")
  expect_equal(dim(out), c(3000,5))


})


test_that("fortify_EpiModel()", {

  data(EpiModel_det)

  out <- fortify_EpiModel(EpiModel_det)
  expect_true("data.frame" %in% class(out))
  expect_equal(dim(out), c(300,4))

  ## THE ICM
  data(EpiModel_icm)
  out <- fortify_EpiModel(EpiModel_icm)
  expect_true("data.frame" %in% class(out))
  expect_equal(dim(out), c(3000,5))


})



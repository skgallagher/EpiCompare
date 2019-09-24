test_that("fortify_pomp()", {
  data("pomp_sir")

  expect_equal(1,1)
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

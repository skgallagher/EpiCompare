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

###
  out71 <- fortify_pomp(pomp_pomp)
  expect_equal(ncol(out71), 5)
  expect_true(all(c("t", "S", "I", "R", "sim") %in% colnames(out71)))


})

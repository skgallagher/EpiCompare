context("test for utility functions")

testthat::test_that("test check_character_percent", {
  testthat::expect_equal(check_character_percent("100%", "x"),1)
  testthat::expect_equal(check_character_percent("99.99%", "x"),.9999)
  testthat::expect_error(check_character_percent("0%", "x"))
  testthat::expect_error( # both error and warning
    testthat::expect_warning(check_character_percent(".99.99%", "x")))
})

testthat::test_that("test is.wholenumber", {
  testthat::expect_true(is.wholenumber(1))
  testthat::expect_false(is.wholenumber(.5))
  testthat::expect_true(all(is.wholenumber(rbinom(100,10,.5))))
  testthat::expect_equal(is.wholenumber((1:10)/2),rep(c(F,T), 5))
})

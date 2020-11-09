context("test for utility functions")

testthat::test_that("check_character_percent", {
  testthat::expect_equal(check_character_percent("100%", "x"),1)
  testthat::expect_equal(check_character_percent("99.99%", "x"),.9999)
  testthat::expect_error(check_character_percent("0%", "x"))
  testthat::expect_error( # both error and warning
    testthat::expect_warning(check_character_percent(".99.99%", "x")))
})
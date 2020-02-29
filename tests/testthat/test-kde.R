context("test for kde functions")


test_that("test kde_from_list - checking against a list", {
  data_list <- lapply(1:5, function(x) data.frame(x = 2^rnorm(100),
                                                  y = rnorm(100)))

  contour_list <- kde_from_list(data_list, alpha = .1)

  data_single <- data_list %>% do.call(rbind, .)
  kde_single <- ks::kde(data_single, gridsize = rep(1000,2))
  contour_single_df<- extract_contour(kde_single, alpha = .1)

  testthat::expect_equal(contour_list, contour_single_df)
})


test_that("test fit_kde_object - basic static example", {
  # basic example - 1 point
  set.seed(8192)
  x <- 2^rnorm(100)
  y <- rnorm(100)
  dfmat <- cbind(x,y)

  kde_object <- fit_kde_object(dfmat)

  # just checking attributes relative to parameter inputs
  # (this function is really just a wrapper of ks...)
  testthat::expect_equal(kde_object$eval.points[[1]] %>% length(), 1000)
  testthat::expect_equal(kde_object$x, dfmat)
  testthat::expect_equal(kde_object$H, ks::Hpi(dfmat))

})

test_that("test extract_contour - basic static example", {
  # basic example - 1 point
  df <- data.frame(x = c(0),
                   y = c(0))
  kde_object <- ks::kde(df,H = diag(2))
  cont <- extract_contour(kde_object, .05)

  cont_df <- as.data.frame(cont[[1]][c("x","y")])

  testthat::expect_equal(length(cont), 1) # single component
  testthat::expect_equivalent(cont_df[1,], cont_df[5,]) # full contour loop
  testthat::expect_true(all.equal(cont_df[1:4,] %>%
                                    sapply(mean) %>% as.vector(),
                                  c(0,0))) # center at 0

  # gaussian data, contour levels check
  x1 <- 2^rnorm(100)
  y1 <- rnorm(100)
  dfmat <- cbind(x1,y1)
  kde_object <- ks::kde(dfmat)

  cont_05 <- extract_contour(kde_object, .05)
  cont_1 <- extract_contour(kde_object, .1)
  cont_2 <- extract_contour(kde_object, .2)

  # note that kde objects store the values relative to an increasing value
  expect_equal(cont_05[[1]]$level, as.numeric(kde_object$cont["95%"]))
  expect_equal(cont_1[[1]]$level, as.numeric(kde_object$cont["90%"]))
  expect_equal(cont_2[[1]]$level, as.numeric(kde_object$cont["80%"]))


})


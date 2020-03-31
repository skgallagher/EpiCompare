## Test for geom_aggregate
## Think there is an issue with NA



test_that("NAs for geom_aggregate", {

  library(ggplot2)
  df <- data.frame(agent_id = factor(1:5),
                   sim = factor(1),
                   tI = c(446, NA, 196, 465, NA),
                   tR = c(464, NA, 425, 476, NA))

  df %>% ggplot() +
    geom_aggregate(aes(y = tI, z = tR),
                   color = "blue") +
    coord_tern()

  expect_true(TRUE)

})



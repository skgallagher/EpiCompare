context("testing \"aaa\" functions (top of package)")

test_that("tests arrangeGrob, basic (ggplot)", {
  local_edition(3)
  set.seed(1)
  ## only ggplot()
  df <- data.frame(x = rnorm(50) + 5,
                   y = rnorm(50) + 5,
                   z = rnorm(50) + 5,
                   id = "1")
  
  df2 <- data.frame(x = rnorm(50) + 2,
                    y = rnorm(50) + 2,
                    z = rnorm(50) + 11,
                    id = "2")
  all_df <- rbind(df,df2)
  
  gg1 <- ggplot(df) +
    geom_point(aes(x=x,y=y))
  gg2 <- ggplot(df2) +
    geom_point(aes(x=x,y=y))
  gg_all <- ggplot(all_df) +
    geom_point(aes(x=x,y=y, color = id))
  a <- arrangeGrob(grobs = list(gg1,gg2,gg_all))
  testthat::expect_snapshot(a)
  testthat::expect_snapshot_output(grid.arrange(grobs = list(gg1,gg2,gg_all)))
  
  
  suppressWarnings(suppressMessages(gg1_3d <- ggplot(df) +
    geom_point(aes(x=x,y=y, z = z)) +
    coord_tern()))
  suppressWarnings(suppressMessages(gg2_3d <- ggplot(df2) +
    geom_point(aes(x=x,y=y, z = z)) +
    coord_tern()))
  suppressWarnings(suppressMessages(gg_all_3d <- ggplot(all_df) +
    geom_point(aes(x=x,y=y,  z = z, color = id)) +
    coord_tern()))
  a_3d <- arrangeGrob(grobs = list(gg1_3d,gg2_3d,gg_all_3d))
  testthat::expect_snapshot(a_3d)
  testthat::expect_snapshot_output(grid.arrange(grobs = list(gg1_3d,gg2_3d,gg_all_3d)))
  
  
  a_3d_and_2d <- arrangeGrob(grobs = list(gg1,gg2_3d,gg_all_3d))
  testthat::expect_snapshot(a_3d_and_2d)
  testthat::expect_snapshot_output(grid.arrange(grobs = list(gg1,gg2_3d,gg_all_3d)))
  
  
  
})


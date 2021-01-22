
library(tidyverse)
library(ks)
library(latex2exp)
# data ------
set.seed(1)
data <- data.frame(x = runif(10000),
                   y = runif(10000))

#plot(data$x, data$y)
#id_vals <- identify(data$x, data$y)
id_vals <- c(545, 800, 1090, 1659, 1750, 1886, 2374, 2692,
             2901, 3411, 3839, 4018, 5023, 5341, 5505,
             5523, 7983, 8273, 8706, 9025, 9401)

data_select <- data[id_vals,]
data_select$x[data_select$y < .55] <-
  data_select$x[data_select$y < .55] + .05

data_select$y[data_select$y > .5 & data_select$y < .65] <-
  data_select$y[data_select$y > .5 & data_select$y < .65] + .04

data_select$x[data_select$x < .3] <-
  data_select$x[data_select$x  < .3] + .05

data_select %>% ggplot() +
  geom_point(aes(x = x , y = y))

my_kde_similar_mass_kde2 <- function(data, h){
  x <- data$x 
  y <- data$y
  nx <- length(x)
  
  h <- rep(h, 2L)
  
  gx <- x
  gy <- y
  
  if (any(h <= 0)) {
    stop("bandwidths must be strictly positive")
  }
  h <- h/4
  
  ax <- outer(gx, x, "-")/h[1L]
  ay <- outer(gy, y, "-")/h[2L]
  z <- diag(tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(ay), , nx))/(nx * h[1L] * h[2L]))
  return(z)
}

my_kde_dist <- function(data, 
                        sigma = "20%"){
  dmat <- as.matrix(dist(data))
  pd_vec <- distance_psuedo_density_function(dmat, sigma = sigma, df_out = F)
  return(pd_vec)
}


dist_x <- dist(data_select %>% select(x,y)) %>% as.matrix()
rank_prob_range <- (1:nrow(data_select))/nrow(data_select) + .5/nrow(data_select)


# functions ------------------
split_threshold_sims <- function(probs, threshold = NA, rank_prob = NA){
  
  if (is.na(threshold) & is.na(rank_prob)){
    stop("either 'threshold' or 'rank_prop' must be specified")
  }
  
  if (is.na(threshold)){
    proportion_rank <- rank(probs)/ length(probs)
    id <- 2*(proportion_rank > rank_prob) - 1
  } else{
    id <- 2*(probs < threshold) - 1
  }
  
  return(id)
}

plus_update <- function(dist_x, group_id, r_n = 1){
  group_id_inner <- group_id
  plus_dist_to_minus <- dist_x[group_id == 1,][,group_id == -1]
  group_id_inner[group_id == 1] <- 2*(apply(plus_dist_to_minus, 1, min) > r_n) - 1
  return(group_id_inner)
}


contour_vis_df <- function(correct_plus_data, r_n = 1, over_delta = .1, grid_size = c(200,200)){
  # code similar to EpiCompare::StatPredBandDeltaBall's compute_group function
  data_deep_points <- correct_plus_data
  
  delta_info <- EpiCompare::delta_structure(data_deep_points, xy_columns = c("x", "y"))
  
  
  structure <- delta_info$structure
  
  inner_df <- dplyr::setdiff(data_deep_points %>%
                               dplyr::select(x,y),
                             structure %>%
                               dplyr::select(x,y))
  
  border_points <- structure %>% dplyr::select(x,y)
  inner_points <- inner_df
  
  xrange <- seq(min(border_points$x) - over_delta,
                max(border_points$x) + over_delta,
                length.out = grid_size[1])
  
  yrange <- seq(min(border_points$y) - over_delta,
                max(border_points$y) + over_delta,
                length.out = grid_size[2])
  
  updated_gridpoints <- EpiCompare:::get_closest(border_points, inner_points,
                                                 r_n,
                                                 xrange = xrange,
                                                 yrange = yrange,
                                                 gridbreaks = NULL)
  
  
  update_gridpoints_mat <- tidyr::pivot_wider(updated_gridpoints,
                                              names_from = "y",
                                              values_from = "z") %>%
    dplyr::select(-x) %>% as.matrix
  
  
  cl <- grDevices::contourLines(x = xrange, y = yrange,
                                z = update_gridpoints_mat,levels = c(2))
  
  lengths <- vapply(cl, function(x) length(x$x), integer(1))
  xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
  pieces <- rep(seq_along(cl), lengths)
  
  vis_df <- data.frame(
    x = xs,
    y = ys,
    piece = pieces)
  
  return(vis_df)
}


contour_density_vis_df <- function(data, h = .1, 
                                   over_delta = .1, 
                                   grid_size = c(200,200),
                                   lambda = 1){
  
  
  kde_estimate <- MASS::kde2d(x = data$x, y = data$y, h = h, n = grid_size, 
                              lims = c(min(data$x) - over_delta, 
                                       max(data$x) + over_delta,
                                       min(data$y) - over_delta,
                                       max(data$y) + over_delta))
  
  cl <- grDevices::contourLines(x = kde_estimate$x, y = kde_estimate$y,
                                z = kde_estimate$z,levels = c(lambda))
  
  lengths <- vapply(cl, function(x) length(x$x), integer(1))
  xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
  pieces <- rep(seq_along(cl), lengths)
  
  vis_df <- data.frame(
    x = xs,
    y = ys,
    piece = pieces)
  
  return(vis_df)
}


# visualize -------------
arrangeGrob2 <- gridExtra::arrangeGrob
h <- .1
n <- nrow(data_select)
prop_cutoff <- c(seq(.5/n, 1 +  .5/n, by =1/n )[4:18],
                 seq(.5/n, 1 +  .5/n, by =1/n )[18:8],
                 rep(seq(.5/n, 1 +  .5/n, by =1/n )[8], 23),
                 seq(.5/n, 1 +  .5/n, by =1/n )[8:4]) # come back to .35

# 80% select
pd <- my_kde_dist(data_select[,c("x","y")])
data_select$pd <- pd
top_df <- data_select %>% filter(pd > quantile(pd, probs = .2))
delta_radius <- simulationBands::get_delta_flex(top_df[,c("x","y")])

radius <- c(rep(.035, 26) , seq(.035, .02, by = -.005),
            seq(.02, .1, by = .005),
            seq(.1, .035, by = -.05), rep(.035, 5))

prop_cutoff_level <- c(seq(.5/n, 1 +  .5/n, by = 1/n )[4:18],
                       seq(.5/n, 1 +  .5/n, by = 1/n )[18:4])

prop_cutoff_level_discrete <- seq(.5/n, 1 +  .5/n, by = 1/n )[seq(4,18, by = 4)]
val_cutoff_level_discrete <- seq(0, n, by = 1 )[seq(4,18, by = 4)]


radius_level_discrete <- rep(delta_radius, #.035, 
                    length(prop_cutoff_level_discrete))

#radius_rad <- c(seq(.1, .02, by = -.005), seq(.02,.1, by = .005))
#prop_cutoff_rad <- rep(seq(.5/n, 1 +  .5/n, by =1/n )[8], length(radius_rad))

parameters_list_level_discrete <- lapply(1:length(radius_level_discrete), 
                                function(idx) list("r_n" = radius_level_discrete[idx],
                                                   "rank_prob" = prop_cutoff_level_discrete[idx]))

my_plot_func <- function(my_data_select,
                         input, # rank_prop (proportion of points selected)
                         sigma = "20%"){
  my_prob <- my_kde_dist(my_data_select, sigma = sigma)
  
  my_data_select$my_prob <- my_prob
  my_data_select$rank <- rank(my_data_select$my_prob)
  
  rank_prob <- input$rank_prob
  r_n <- input$r_n
  probs <- my_data_select$my_prob
  
  
  group_id <- split_threshold_sims(my_data_select$my_prob, 
                                   rank_prob = rank_prob)
  #group_id_clean <- plus_update(dist_x, group_id, r_n = r_n) # for subtraction for defining set
  
  data_select_inner <- my_data_select %>%
    mutate(raw_id = group_id)
  
  lambda <- min(probs[group_id == 1])

  vis_df <- contour_vis_df(data_select_inner[group_id == 1,], r_n = r_n)
  
  vis1 <- vis_df %>%
    ggplot() +
    geom_polygon(aes(x = x , y = y, group = piece), fill = NA, color = "black") +
    geom_point(data = data_select_inner %>%
                 mutate(raw_id = factor(raw_id, levels = c("-1", "1"),
                                        labels = c("inside", "outside"))), 
                        aes(x = x , y = y, shape = factor(raw_id)), color = "black") +
    theme_minimal() +
    theme(aspect.ratio = 1) +
    labs(#title = "Data Visualization",
         #subtitle = "ranked by estimated probability value",
         y = TeX("$y_2$ (dimension 2)"),
         x = TeX("$y_1$ (dimension 1)"),
         color = TeX("In defining set ($\\hat{f}(\\hat{y}_i^b) \\geq \\lambda$)")) +
    xlim(c(.15, .65)) +
    ylim(c(.3,.9)) +
    scale_shape_manual(values = c(1,16)) +
    theme(legend.position = "none")
  
  return(vis1)
}

lower_prop_func <- function(n1, n){
  inner_df <- data.frame(x = factor(c(0,1), levels = c(1,0)),
                         quantity = c(n1, n - n1))
  vis <- inner_df %>%
    ggplot() +
    geom_bar(aes(x = quantity, y = "", fill = x),
             stat="identity", width=1,color = "black") +
    geom_label(x = n1, y = "", label = n-n1) + 
    scale_fill_manual(values = c("black", "white")) +
    theme_void() +
    theme(legend.position = "none") 
  
  return(vis)
}

ggvis <- lapply(parameters_list_level_discrete,
                function(l) my_plot_func(data_select, l, sigma = "20%"))


ggvis_below <- lapply(val_cutoff_level_discrete,
                      function(n1) lower_prop_func(n1, n = n))


vis <- grid.arrange(grobs = c(ggvis, ggvis_below),
                    layout_matrix = matrix(c(
                      rep(c(rep(1,4), rep(2,4), rep(3, 4), rep(4,4)), 4),
                      sapply(5:8, function(x) rep(x,4))),
                      byrow = T, ncol = 16), 
                    bottom = grid::textGrob("Proportion of simulation points used",
                                      gp = grid::gpar(fontsize = 12)),
                    top = grid::textGrob("Simulation-based nested set",
                                         gp = grid::gpar(fontsize = 25, 
                                                         fontface = 'bold')))
ggsave(vis, filename = "~/Desktop/nested_set_vis.png", width = 4*6+1, height = 6*(5/4)+1)

## SKG
## Oct. 1, 2020
## Prediction band plotting issues


test_that("problem with prediction band plotting", {
  


  ## This is the SIR representation
  trans_mat <- matrix(c("X0 * (1 - X1 * par1 / N)", "X0 * X1  * par1 / N", "0",
                        "0", "X1 * (1 - par2)", "par2 * X1",
                        "0", "0", "X2"), byrow = TRUE, nrow = 3)
  rownames(trans_mat) <- c("S", "I", "R")
  init_vals <- c(187, 1, 0)
  par_vals <- c("par1" = .2, "par2" = .1)
  max_T <- 55
  n_sims <- 20
  
  B <- 5
  
  set.seed(1)
  sim_list <- vector(mode = "list", length = B)
  for(bb in 1:B){
    
    par_vals <-  c("par1" = par_val_mat[bb, 1],
                   "par2" = par_val_mat[bb, 2])
    
    
    abm <- simulate_agents(trans_mat = trans_mat,
                           init_vals = init_vals,
                           par_vals = par_vals,
                           max_T = max_T,
                           n_sims = 2,
                           verbose = FALSE)
    agg_model <- abm %>% dplyr::group_by(sim) %>% 
      agents_to_aggregate(states = c(I, R)) %>%
      ungroup()
    agg_model$batch <- bb
    agg_model$beta <- par_vals[1]
    agg_model$gamma <- par_vals[2]
    sim_list[[bb]] <- agg_model
    
  }
  
  sim_df <- dplyr::bind_rows(sim_list)
  sim_df$id <- paste0(sim_df$batch, "-",
                      sim_df$sim)
  

  
  plot_df <- sim_df %>% dplyr::filter(t != 0) %>%
    dplyr::select(id, t, X0, X1, X2)
  
  
  ggplot() +
    geom_prediction_band(data = plot_df,
                         aes(x = X0, y = X1, z = X2, 
                             sim_group = id), alpha = .5,
                         fill = "cornflowerblue") +
    coord_tern() + theme_sir() +
    geom_point(data = aggregate_hag,
               aes(x = S, y = I, z =R), col = "red") +
    labs(title = "Prediction band for best parameters and original data")
  

  
  
  
})
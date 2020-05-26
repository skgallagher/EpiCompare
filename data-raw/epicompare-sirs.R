library(tidyverse)
library(gridExtra)
devtools::load_all()


## SIRS

## This is the SIRS representation
trans_mat <- matrix(c("X0 * (1 - (X1 + X4) * par1 / N)",
                      "X0 * (X1 + X4)  * par1 / N", 
                      "0",  "0", "0", "0", ## ROW 1 DONE
                      "0", "X1 * (1 - par2)", "par2 * X1", "0", "0", "0",  ## ROW 2 DONE
                      "0", "0", "X2 * (1 - par3)", "X2 * par3", "0", "0", ## ROW 3 DONE
                      "0", "0",
                      "0", "X3 * (1 - (X1 + X4) * par3 / N)",
                      "X3 * (X1 + X4) * par4 / N", "0", ## ROW 4 DONE
                      "0", "0", "0",
                      "0", "X4 * (1 - par5)", "par5 * X4", ## ROW 5 DONE
                      "0", "0", "0", "0", "0", "X5"
                      ), byrow = TRUE, nrow = 6)
rownames(trans_mat) <- c("S1", "I1", "R1", "S2", "I2", "R2")
colnames(trans_mat) <- c("S1", "I1", "R1", "S2", "I2", "R2")
init_vals <- c(950, 50, 0, 100, 50, 0)
par_vals <- c(par1 = .5, par2 = .4,  #beta1, gamma1
              par3 = .08, # transition from R1 to S2
              par4 = .08, par5 = .06) #beta2, gamma2
max_T <- 300
n_sims <- 25




## Warning:  will take about a minute.  Set `verbose = TRUE` to see progress
set.seed(2020)
abm <- simulate_agents(trans_mat,
                       init_vals,
                       par_vals,
                       max_T,
                       n_sims,
                       verbose = FALSE,
                       out_format = "wide")

agg_model <- abm %>% group_by(sim) %>%
  agents_to_aggregate(states = c(I1, R1, S2, I2, R2))
colnames(agg_model) <- c("sim", "t",
                         "S1", "I1", "R1",
                         "S2", "I2", "R2")


g0 <- ggplot(data = agg_model, aes(x = t, y = S1 + S2, group = sim )) +
  geom_line()
g1 <- ggplot(data = agg_model, aes(x = t, y = I1 + I2, group = sim )) +
  geom_line()
g2 <- ggplot(data = agg_model, aes(x = t, y = R1 + R2, group = sim)) +
  geom_line()

grid.arrange(g0, g1, g2, ncol = 1)

sirs_data <- agg_model


usethis::use_data(sirs_data, overwrite = TRUE)

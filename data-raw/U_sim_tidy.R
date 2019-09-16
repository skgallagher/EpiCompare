library(tidyverse)
devtools::load_all()


set.seed(2019)
n_sims <- 50
n_time_steps <- 50
beta <- .2
gamma <- .06
init_SIR <- c(187, 1, 0)
output_format <- "array"

U_sims <- simulate_SIR_agents(n_sims = n_sims,
                           n_time_steps = n_time_steps,
                           beta = beta, gamma = gamma,
                           init_SIR = init_SIR,
                           output_format = output_format)
usethis::use_data(U_sims, overwrite = TRUE)






dimnames(my_sims) <- list(paste(1:50),
                          c("init_state", "SMax", "IMax"),
                          paste(1:188))
U_sims_tidy <- my_sims %>% as.data.frame.table %>% spread(Var2, Freq) %>%
  rename(sim = "Var1",
         individual = "Var3")

usethis::use_data(U_sims_tidy, overwrite = TRUE)

library(dplyr)
library(tidyr)
library(ggtern)
devtools::load_all()


set.seed(2019)
n_sims <- 50
n_time_steps <- 50
#beta <- .4
#gamma <- .12
beta <- .2
gamma <- .06
init_SIR <- c(187, 1, 0)
output_format <- "array"

agents_sims <- simulate_SIR_agents(n_sims = n_sims,
                           n_time_steps = n_time_steps,
                           beta = beta, gamma = gamma,
                           init_SIR = init_SIR,
                           output_format = "data.frame")

all_sims_agg <- agents_sims %>%
 group_by(sim) %>%
    timeternR::agents_to_aggregate_SIR(ind = 1:3)

library(ggplot2)
ggplot(data = all_sims_agg, aes(x = t, y = S, col = as.numeric(sim),
                                group = sim)) +
                            geom_line()



usethis::use_data(agents_sims, overwrite = TRUE)


dimnames(agents_sims) <- list(paste(1:50),
                          c("init_state", "SMax", "IMax"),
                          paste(1:188))

if (timeternR:::tidyr_new_interface()){
  agents_sims_tidy <- agents_sims %>% as.data.frame.table %>%
    tidyr::pivot_wider(names_from = Var2, values_from = Freq) %>%
    dplyr::rename(sim = "Var1",
           individual = "Var3")
} else {
  agents_sims_tidy <- agents_sims %>% as.data.frame.table %>%
    tidyr::spread(Var2, Freq) %>%
    dplyr::rename(sim = "Var1",
                  individual = "Var3")
}

usethis::use_data(agents_sims_tidy, overwrite = TRUE)

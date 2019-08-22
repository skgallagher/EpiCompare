library(tidyverse)
my_sims <- timeternR::U_sims

dimnames(my_sims) <- list(paste(1:50),
                          c("init state", "SMax", "IMax"),
                          paste(1:188))
U_sims_tidy <- my_sims %>% as.data.frame.table %>% spread(Var2, Freq) %>%
  rename(sim = "Var1",
         individual = "Var3")

usethis::use_data(U_sims_tidy, overwrite = TRUE)

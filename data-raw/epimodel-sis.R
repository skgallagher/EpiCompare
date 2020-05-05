## SKG
## May 5, 2020
## Getting data for a SIS model from EpiModel
## Woo
## See http://statnet.org/tut/BasicDCMs.html#sis_model_with_sensitivity_analyses

library(EpiModel)
devtools::load_all()

param <- param.dcm(inf.prob = 0.2,  rec.rate = 0.05)
init <- init.dcm(s.num = 499, i.num = 1)
control <- control.dcm(type = "SIS", nsteps = 100)
mod <- dcm(param, init, control)
plot(mod)

sis_data <- mod
usethis::use_data(sis_data, overwrite = TRUE)

sis_data_f <- fortify_aggregate(mod)
head(sis_data_f)  


usethis::use_data(sis_data_f, overwrite = TRUE)

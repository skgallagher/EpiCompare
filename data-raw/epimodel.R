## SKG EpiModel

library(EpiModel)

## http://statnet.github.io/tut/BasicICMs.html

param <- param.dcm(inf.prob = 0.2, act.rate = 0.8, rec.rate = 1/50,
                   a.rate = 0, ds.rate = 0, di.rate = 0, dr.rate = 0)
init <- init.dcm(s.num = 900, i.num = 100, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 300)
EpiModel_det <- dcm(param, init, control)

usethis::use_data(EpiModel_det, overwrite = TRUE)


set.seed(2014)
param <- param.icm(inf.prob = 0.2, act.rate = 0.8, rec.rate = 1/50,
                   a.rate = 0,  ds.rate = 0, di.rate = 0,
                   dr.rate = 0)
init <- init.icm(s.num = 900, i.num = 100, r.num = 0)
control <- control.icm(type = "SIR", nsteps = 300, nsims = 10)
EpiModel_icm <- icm(param, init, control)

usethis::use_data(EpiModel_icm, overwrite = TRUE)


## ICM with birth and death
set.seed(2014)
param <- param.icm(inf.prob = 0.2, act.rate = 0.8, rec.rate = 1/50,
                   a.rate = .02,  ds.rate = .02, di.rate = .02,
                   dr.rate = .02)
init <- init.icm(s.num = 900, i.num = 100, r.num = 0)
control <- control.icm(type = "SIR", nsteps = 300, nsims = 1)
EpiModel_icm <- icm(param, init, control)


EpiModel_agg_bd <- as.data.frame(EpiModel_icm)
usethis::use_data(EpiModel_agg_bd, overwrite = TRUE)

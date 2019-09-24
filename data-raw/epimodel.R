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



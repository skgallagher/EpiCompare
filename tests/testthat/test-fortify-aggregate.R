test_that("fortify_aggregate", {
    ## ICM
    out <- fortify_aggregate(EpiModel_icm)
    expect_equal(colnames(out), c("t", "orig_t", "sim", "X0", "X1", "X2"))
    ## DCM
    out3 <- fortify_aggregate(EpiModel_det)
    expect_equal(colnames(out3), c("t", "orig_t", "sim", "X0", "X1", "X2"))
    expect_equal(rowSums(out3[, 4:6]),  rep(1000.0, 300))
    ## POMP (POMP_LIST)
    out <- fortify_aggregate(pomp_pomp, states = NULL)
    expect_equal(colnames(out), c("t", "sim", "X0", "X1", "X2", "X3"))
    ##
    out <- fortify_aggregate(pomp_pomp, states = c("S", "I", "R"))
    expect_equal(colnames(out), c("t",  "sim", "X0", "X1", "X2"))
    ## POMP

})


test_that("more epiModels", {

    library(EpiModel)

    ## Making sure all the vignettes from the website can be converted
    ## http://statnet.org/tut/BasicDCMs.html
    
    ## SI model
    param <- param.dcm(inf.prob = 0.2, act.rate = 0.25)
    init <- init.dcm(s.num = 500, i.num = 1)
    control <- control.dcm(type = "SI", nsteps = 500)
    mod <- dcm(param, init, control)
    out <- fortify_aggregate(data = mod,
                             states = c("s.num", "i.num"))
    expect_equal(500, nrow(out))
    out2 <- fortify_aggregate(data = mod)
    expect_equal(out, out2)
    mod <- dcm(param, init, control)
    out <- fortify_aggregate(data = mod,
                             states = c(s.num, i.num))
    expect_equal(out, out2)    

    

})


test_that("sir model with demography -- epimodel", {
    library(EpiModel)
    param <- param.dcm(inf.prob = 0.2, act.rate = 1, rec.rate = 1/20,
                       a.rate = 1/95, ds.rate = 1/100, di.rate = 1/80, dr.rate = 1/100)
    init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0)
    control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)
    mod <- dcm(param, init, control)
    out <- fortify_aggregate(mod)
    expect_equal(nrow(out),
                 floor(mod$control$nsteps / mod$control$dt) -1)
})


test_that("sis model -- epimodel",{
    library(EpiModel)
    param <- param.dcm(inf.prob = 0.2, act.rate = seq(0.25, 0.5, 0.05), rec.rate = 0.02)
    init <- init.dcm(s.num = 500, i.num = 1)
    control <- control.dcm(type = "SIS", nsteps = 350)
    mod <- dcm(param, init, control)
    out <- fortify_aggregate(mod)
    expect_equal(nrow(out), 350 * 6)
})


test_that("more extensive icms -- epimodel", {
   
    library(EpiModel)
    ## http://statnet.org/tut/BasicICMs.html
    ## SI
    param <- param.icm(inf.prob = 0.2, act.rate = 0.25)
    init <- init.icm(s.num = 500, i.num = 1)
    control <- control.icm(type = "SI", nsims = 10, nsteps = 300)
    mod <- icm(param, init, control)
    out <- fortify_aggregate(mod)
    expect_equal(nrow(out), 300 * 10)


    ## SIR
    param <- param.dcm(inf.prob = 0.2, act.rate = 0.8, rec.rate = 1/50,
                       a.rate = 1/100, ds.rate = 1/100, di.rate = 1/90, dr.rate = 1/100)
    init <- init.dcm(s.num = 900, i.num = 100, r.num = 0)
    control <- control.dcm(type = "SIR", nsteps = 300)
    det <- dcm(param, init, control)
    out <- fortify_aggregate(det, states = c(s.num, r.num))
    expect_equal(nrow(out), 300)
    
})


## test_that("epimodel--netsim", {
##     ## WARNING:
##     ## EpiModel::netest is deprecated 
##     library(EpiModel)
##     nw <- network.initialize(n = 1000, directed = FALSE)
##     nw <- set.vertex.attribute(nw, "race", rep(0:1, each = 500))
##     formation <- ~edges + nodefactor("race") + nodematch("race") + concurrent
##     target.stats <- c(250, 375, 225, 100)
##     coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 25)
##     est1 <- netest(nw, formation, target.stats, coef.diss, edapprox = TRUE)
    
##     param <- param.net(inf.prob = 0.1, act.rate = 5, rec.rate = 0.02)
##     status.vector <- c(rbinom(500, 1, 0.1), rep(0, 500))
##     status.vector <- ifelse(status.vector == 1, "i", "s")
##     init <- init.net(status.vector = status.vector)
##     control <- control.net(type = "SIS", nsteps = 500,
##                            nsims = 10, epi.by = "race")
##     sim1 <- netsim(est1, param, init, control)

##     o
  ##  ut <- fortify_aggregate(sim1)
 ##   expect_equal(nrow(out), 5000)

##})

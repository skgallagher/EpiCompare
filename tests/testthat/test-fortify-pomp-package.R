context("Testing a variety of models from the pomp package")

test_that("Seasonal SIR", {
    ## https://kingaa.github.io/pomp/vignettes/oaxaca.html#a_more_complex_example:_a_stochastic,_seasonal_sir_model
    library(pomp)


    rmeas <- "
  cases = rnbinom_mu(theta, rho * H);
"

    dmeas <- "
  lik = dnbinom_mu(cases, theta, rho * H, give_log);
"

    sir.step <- "
  double rate[6];
  double dN[6];
  double P;
  P = S + I + R;
  rate[0] = mu * P;       // birth
  rate[1] = Beta * I / P; // transmission
  rate[2] = mu;           // death from S
  rate[3] = gamma;        // recovery
  rate[4] = mu;           // death from I
  rate[5] = mu;           // death from R
  dN[0] = rpois(rate[0] * dt);
  reulermultinom(2, S, &rate[1], dt, &dN[1]);
  reulermultinom(2, I, &rate[3], dt, &dN[3]);
  reulermultinom(1, R, &rate[5], dt, &dN[5]);
  S += dN[0] - dN[1] - dN[2];
  I += dN[1] - dN[3] - dN[4];
  R += dN[3] - dN[5];
  H += dN[1];
"

    sir1 <- simulate(
        times = seq(0, 10, by = 1/52),
        t0 = -1/52, 
        dmeasure = Csnippet(dmeas),
        rmeasure = Csnippet(rmeas), 
        rprocess = euler(step.fun = Csnippet(sir.step), delta.t = 1/52/20),
        obsnames="cases",
        statenames = c("S", "I", "R", "H"),
        paramnames = c("gamma", "mu", "theta", "Beta", "popsize",
                       "rho", "S.0", "I.0", "R.0"), 
        accumvars = "H",
        rinit = Csnippet("
    double sum = S_0 + I_0 + R_0;
    S = nearbyint(popsize * S_0 / sum);
    I = nearbyint(popsize * I_0 / sum);
    R = nearbyint(popsize * R_0 / sum);
    H = 0;
    "),
    params = c(popsize = 500000, Beta = 400, gamma = 26,
               mu = 1/50, rho = 0.1, theta = 100, S.0 = 26/400,
               I.0 = 0.002, R.0 = 1),
    seed = 1914679908L,
    format = "pomp") -> sir1

    out <- fortify_aggregate(sir1)
    expect_true(all(c("X0", "X1", "X2", "X3") %in% colnames(out)))

    
})

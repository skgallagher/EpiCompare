library(pomp)

## From the pomp example from: https://kingaa.github.io/pomp/vignettes/oaxaca.html
## Makes a seasonal SIR model



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
  rate[0] = 0;       // birth
  rate[1] = Beta * I / P; // transmission
  rate[2] = 0;           // death from S
  rate[3] = gamma;        // recovery
  rate[4] = 0;           // death from I
  rate[5] = 0;           // death from R
  dN[0] = rpois(rate[0] * dt);
  reulermultinom(2, S, &rate[1], dt, &dN[1]);
  reulermultinom(2, I, &rate[3], dt, &dN[3]);
  reulermultinom(1, R, &rate[5], dt, &dN[5]);
  S += dN[0] - dN[1] - dN[2];
  I += dN[1] - dN[3] - dN[4];
  R += dN[3] - dN[5];
  H += dN[1];
"


pomp_df <- simulate(
  times = seq(0, 100, by = 1),
  t0 = 0,
  dmeasure = Csnippet(dmeas),
  rmeasure = Csnippet(rmeas),
  rprocess = euler(step.fun = Csnippet(sir.step), delta.t = 1),
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
  params = c(popsize = 1000, Beta = .1, gamma = .03,
             mu = 0, rho = 0, theta = 0, S.0 = 950,
             I.0 = 50, R.0 = 0),
  seed = 1914679908L,
  format = "data.frame",
  nsim = 100)
 
usethis::use_data(pomp_df, overwrite = TRUE)


### pomp object
pomp_pomp <- simulate(
  times = seq(0, 100, by = 1),
  t0 = 0,
  dmeasure = Csnippet(dmeas),
  rmeasure = Csnippet(rmeas),
  rprocess = euler(step.fun = Csnippet(sir.step), delta.t = 1),
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
  params = c(popsize = 1000, Beta = .1, gamma = .03,
             mu = 0, rho = 0, theta = 0, S.0 = 950,
             I.0 = 50, R.0 = 0),
  seed = 1914679908L,
  format = "pomp",
  nsim = 100)
 
usethis::use_data(pomp_pomp, overwrite = TRUE)




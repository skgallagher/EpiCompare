
library(pomp)

## From the pomp example from: https://kingaa.github.io/short-course/parest/odes.html#exercise-sirs-model
## Makes a seasonal SIR model

## Modified for a SIRS model like here: https://www.idmod.org/docs/typhoid/model-sir.html#sirs-model
## This is deterministic

sirs.ode <- Csnippet("
  DS = -Beta*S*I/N + eta * R;
  DI = Beta*S*I/N-gamma*I;
  DR = gamma*I - eta * R;
")

init2 <- Csnippet("
  S = S_0;
  I = I_0;
  R = N-S_0-I_0;
")

sirs <- pomp(data=data.frame(time=seq(0,100, by = 1),cases=NA),
             times="time",t0= 0,
             skeleton=vectorfield(sirs.ode),
             rinit=init2,
             statenames=c("S","I","R"),
             paramnames=c("Beta","gamma","eta","S_0","I_0","N")
)

params <- c(Beta = .2, gamma = .1, eta = .05,
            S_0 = 990, I_0 = 10, N = 1000)

sirs_data <- trajectory(sirs, params = params, format = "d")

library(ggplot2)
library(tidyr)
ggplot(data = sirs_data %>%
         tidyr::pivot_longer(-c(time, .id)), aes(x = time, y = value,
                                                 group = name,
                                                 col = name)) + geom_line()


usethis::use_data(sirs_data, overwrite = TRUE)
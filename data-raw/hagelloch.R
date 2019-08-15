## code to prepare `hagelloch` dataset goes here

## Raw Hagelloch data is from surveillance package

data("hagelloch", package = "surveillance")

## Original source and description here: https://rdrr.io/rforge/surveillance/man/hagelloch.html
head(hagelloch.df)  # This is the original data
hagelloch_orig <- rlang::duplicate(hagelloch.df)

usethis::use_data(hagelloch_raw, overwrite = TRUE)


## Make SIR format
N <- nrow(hagelloch.df) #+ 380 ## second term is remianing villagers
T <- 94

## Suff stats for agents

## install_github("shannong19/catalyst") # for Ben
## devtools::load_all("~/catalyst")
A0 <- rep(0, N)
inf_ind <- which.min(hagelloch.df$tI)
A0[inf_ind] <- 1
## round I and R time - going to use floor
SMax <- floor(hagelloch.df$tI) + 1
SMax <- ifelse(SMax > T-1, T-1, SMax)
IMax <- floor(hagelloch.df$tR) + 1
IMax <- ifelse(IMax > T-1, T-1, IMax)
hagelloch_agents <- data.frame(init_state = factor(A0), max_time_S = SMax, max_time_I = IMax)
hagelloch_sir <- UtoX_SIR(U, T = T) #

usethis::use_data(hagelloch_sir, overwrite = TRUE)

usethis::use_data(hagelloch_agents, overwrite = TRUE)


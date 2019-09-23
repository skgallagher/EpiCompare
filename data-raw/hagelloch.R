## code to prepare `hagelloch` dataset goes here

## Raw Hagelloch data is from surveillance package

data("hagelloch", package = "surveillance")

## Original source and description here: https://rdrr.io/rforge/surveillance/man/hagelloch.html

head(hagelloch.df)  # This is the original data

hagelloch_raw <- rlang::duplicate(hagelloch.df)

usethis::use_data(hagelloch_raw, overwrite = TRUE)

## making an updated hagelloch_raw with NAs in tR and tI

hagelloch_raw2 <- rlang::duplicate(hagelloch_raw)
set.seed(2019)
five_tI_na <- sample(nrow(hagelloch_raw2), size = 5)
five_tR_na <- sample(nrow(hagelloch_raw2), size = 5)

hagelloch_raw2[five_tI_na, c("tR", "tI")] <- NA
hagelloch_raw2[five_tR_na, "tR"] <- NA

usethis::use_data(hagelloch_raw2, overwrite = TRUE)

## Make SIR format
N <- nrow(hagelloch_raw) # second term is remaining villagers
max_time <- 94

## Suff stats for agents
A0 <- rep(0, N)
inf_ind <- which.min(hagelloch_raw$tI)
# ^ in general this should be:
#     intersect(which.min(raw_df[,time_col[1]]), which(raw_df[,time_col[1]] < 0))

A0[inf_ind] <- 1
## round I and R time - going to use floor
SMax <- floor(hagelloch_raw$tI) + 1
SMax <- ifelse(SMax > T-1, T-1, SMax)
IMax <- floor(hagelloch_raw$tR) + 1
IMax <- ifelse(IMax > T-1, T-1, IMax)
hagelloch_agents <- data.frame(init_state = factor(A0),
                               max_time_S = SMax,
                               max_time_I = IMax)
hagelloch_sir <- UtoX_SIR(hagelloch_agents, max_time = max_time)

usethis::use_data(hagelloch_sir, overwrite = TRUE)

usethis::use_data(hagelloch_agents, overwrite = TRUE)


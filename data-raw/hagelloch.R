## code to prepare `hagelloch` dataset goes here

## Raw Hagelloch data is from surveillance package

data("hagelloch", package = "surveillance")

## Original source and description here: https://rdrr.io/rforge/surveillance/man/hagelloch.html

# head(hagelloch.df)  # This is the original data

hagelloch_raw <- rlang::duplicate(hagelloch.df)
class(hagelloch_raw) <- c("agents", class(hagelloch_raw))

usethis::use_data(hagelloch_raw, overwrite = TRUE)

## making an updated hagelloch_raw with NAs in tR and tI

hagelloch_raw2 <- rlang::duplicate(hagelloch_raw)
set.seed(2019)
five_tI_na <- sample(nrow(hagelloch_raw2), size = 5)
five_tR_na <- sample(nrow(hagelloch_raw2), size = 5)
two_recovered <- sample(nrow(hagelloch_raw2), size = 2)

hagelloch_raw2[five_tI_na, c("tR", "tI")] <- NA
hagelloch_raw2[five_tR_na, "tR"] <- NA

hagelloch_raw2[two_recovered, "tR"] <- -3*runif(2)
hagelloch_raw2[two_recovered, "tI"] <- -3*runif(2) +
  hagelloch_raw2[two_recovered, "tR"]
hagelloch_raw2[two_recovered,
               c("tPRO", "tERU")] <- hagelloch_raw2[two_recovered, c("tI")] +
    sapply(two_recovered, function(idx) rep(diff(unlist(hagelloch_raw2[idx, c("tI", "tR")])),2)) * c(1/3, 2/3)
class(hagelloch_raw2) <- c(class(hagelloch_raw))


usethis::use_data(hagelloch_raw2, overwrite = TRUE)

## Make SIR format
N <- nrow(hagelloch_raw) # second term is remaining villagers
max_time <- 94

## Suff stats for agents
A0 <- rep(0, N)
initial_inf <- intersect(which(hagelloch_raw$tI < 0),
                         which(hagelloch_raw$tR >= 0))
initial_rec <- intersect(which(hagelloch_raw$tI < 0),
                         which(hagelloch_raw$tR < 0))
A0[initial_inf] <- 1
A0[initial_rec] <- 2

## round I and R time - going to use floor
SMax <- floor(hagelloch_raw$tI) + 1
SMax <- ifelse(SMax > max_time-1, max_time-1, SMax)
IMax <- floor(hagelloch_raw$tR) + 1
IMax <- ifelse(IMax > max_time-1, max_time-1, IMax)


hagelloch_agents <- data.frame(init_state = factor(A0),
                               max_time_S = SMax,
                               max_time_I = IMax)

hagelloch_agents[union(initial_inf, initial_rec),"max_time_S"] <- NA
hagelloch_agents[initial_rec,"max_time_I"] <- NA

class(hagelloch_agents) <- append("agents", class(hagelloch_agents))


hagelloch_sir <- agents_to_aggregate_SIR(hagelloch_agents, max_time = max_time)


usethis::use_data(hagelloch_sir, overwrite = TRUE)

usethis::use_data(hagelloch_agents, overwrite = TRUE)


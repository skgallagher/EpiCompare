## code to prepare `hagelloch` dataset goes here

## Raw Hagelloch data is from surveillance package

data("hagelloch", package = "surveillance")

## Original source and description here: https://rdrr.io/cran/outbreaks/man/measles_hagelloch_1861.html
head(hagelloch.df)  # This is the original data
hagelloch_orig <- rlang::duplicate(hagelloch.df)

usethis::use_data(hagelloch.df)


## Make SIR format
N <- nrow(hagelloch.df) #+ 380 ## second term is remianing villagers
T <- 90

## Suff stats for agents

## install_github("shannong19/catatlyst") # for Ben
## devtools::load_all("~/catalyst")
A0 <- rep(0, N)
inf_ind <- which.min(hagelloch.df$tI)
A0[inf_ind] <- 1
## round I and R time - going to use floor
SMax <- floor(hagelloch.df$tI) + 1
SMax <- ifelse(SMax > T-1, T-1, SMax)
IMax <- floor(hagelloch.df$tR) + 1
IMax <- ifelse(IMax > T-1, T-1, IMax)
U <- matrix(c(A0, SMax, IMax), byrow = TRUE, nrow = 3)
X <- UtoX_SIR(U, T = T) # may need to add catalyst:::




## install_github("shannong19/catalyst")
## devtools::load_all("~/catalyst")

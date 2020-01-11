## Making a data set of agents with births included
## Specifically, we add 5 agents who are born during the course of the Hagelloch epidemic

data("hagelloch_raw")

new_births <- data.frame(
    tBORN = c(1, 28, 12, 39, 52),
    tI = c(NA, 40, NA, 40, 52),  # note last one is born infected
    tR = c(NA, 44, NA, NA, 54),
    tDEAD = c(NA, NA, 29, 44, NA),  # note the third person is born and then dies but never is infected
    SEX = c("male", "female", "female", "male", "male"),
    AGE = c(0, 0, 0, 0, 0)
)

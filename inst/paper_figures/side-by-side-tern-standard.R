## SKG
## Jan 24, 2020


devtools::load_all()
library(dplyr)
library(tidyr)
library(gridExtra)


set.seed(2019)
n_sims <- 1
n_time_steps <- 25
beta <- .7
gamma <- .35
init_SIR <- c(950, 50, 0)
output_format <- "data.frame"

agents_sims <- simulate_SIR_agents(n_sims = n_sims,
                           n_time_steps = n_time_steps,
                           beta = beta, gamma = gamma,
                           init_SIR = init_SIR,
                           output_format = output_format)

agg1 <- agents_to_aggregate_SIR(agents_sims)
max_I_ind <- which.max(agg1$I)
agg1$max <- ifelse(1:nrow(agg1) == max_I_ind, 1, 0)
## STANDARD VIEW

df1 <- agg1 %>% pivot_longer(cols = S:R, names_to = "State")
df1$state <- factor(df1$State, levels = c("S", "I", "R"),
                    labels = c("Susceptible", "Infectious", "Recovered"))
df1$State <- ifelse(df1$State == "S", "Susceptible",
             ifelse(df1$State == "I", "Infectious",
                    "Recovered"))


g1 <- ggplot(data = df1, aes(x = t, y = value)) +
    geom_vline(xintercept = agg1$t[max_I_ind], col = "gray70") + 
    geom_line(size = 1) +
    geom_point( aes(col = factor(max)), size = 3) +
    facet_wrap(~state, nrow = 3) +
    theme_bw(base_size = 12) +
    scale_color_manual(values = c("black", "purple"), guide = FALSE) +
    labs(x = "Time",
         y = "Number in State",
         title = "Standard bivariate views of SIR data")
g1
ggsave("standard-ex.pdf", width = 6)

## Ternary view

## add those cute little =linesarrows
tstar <- which.max(agg1$I) - 1
Sstar <- agg1[tstar + 1, "S"]
Istar <- agg1[tstar + 1, "I"]
Rstar <- agg1[tstar + 1, "R"]
df <- data.frame(x = c(Sstar, Sstar, Sstar, 0, Sstar, N - Rstar) / N * 100,
                 y = c(N - Sstar, Istar, Istar, Istar, Istar, 0) / N * 100,
                 z = c(0, Rstar, Rstar, N - Istar, Rstar, Rstar) / N * 100,
                 group = c(1, 1, 2, 2, 3, 3))


g2 <- ggplot(data = agg1, aes(x = S, y= I, z = R)) +
    ggplot2::geom_path(data = df, aes(x = x, y = y, z = z, group = group),
                       linetype = 2, col = c("blue", "blue", "red", "red",
                                             "yellow3", "yellow3"), size = 2) +
    geom_line(size = 1) +
    geom_point(aes(col = factor(max)), size = 3) +
    coord_tern() +
    theme_bw(base_size = 12)  +
    scale_color_manual(values = c("black", "purple"), guide = FALSE)  +
    Tarrowlab("I") + Larrowlab("S") + Rarrowlab("R") + theme_showarrows() +
    labs(L = "", T = "", R = "") +
    geom_text(data = agg1[tstar + 1,], aes(x = S + 20, y = I  + 40, z = R - 60 ), label = "S = 52, I= 19, R= 29",
              family = "Palatino", size = 5) +
    labs(title = "Ternary view of SIR data")
g2
ggsave("ternary-ex.pdf", width = 6)

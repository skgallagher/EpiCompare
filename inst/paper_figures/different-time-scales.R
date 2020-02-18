## SKG
## Jan 24, 2020


devtools::load_all()
library(dplyr)
library(tidyr)
library(gridExtra)


set.seed(2020)
n_sims <- 1
n_time_steps <- 25
beta <- .1 * 7
r0 <- 2.8
gamma <- beta / r0
init_SIR <- c(950, 50, 0)
output_format <- "data.frame"
a <- .7


par_mat <- matrix(c(beta, gamma,
                    beta/a, gamma / a), byrow = TRUE, ncol = 2)
init_mat <- matrix(c(950, 50, 0,
                     950, 50, 0), byrow = TRUE, ncol = 3)


sims <- simulate_SIR_agents_groups(n_sims,
                                   n_time_steps,
                                   par_mat,
                                   init_mat)

agg <- sims %>% group_by(group) %>% agents_to_aggregate_SIR()

df <- agg %>% pivot_longer(cols = S:R, names_to = "State")

df$state <- factor(df$State, levels = c("S", "I", "R"),
                   labels = c("Susceptible", "Infectious", "Recovered"))
g1 <- ggplot(data = df, aes(x = t, y = value)) +
    geom_point( aes(col = factor(group)), size = 3) +
    facet_wrap(~state, nrow = 3) +
    theme_bw(base_size = 12) +
    scale_color_manual(values = c("black", "purple"), labels = c("Group 1", "Group 2"),
                       name = "Data") +
    labs(x = "Time",
         y = "Number in State",
         title = latex2exp::TeX("Same $R_0$, different time scales")) +
    theme(legend.position = "bottom")
g1
ggsave("diff-time-standard.pdf", width = 6)

g2 <- ggplot(data = agg, aes(x = S, y= I, z = R)) +
    geom_point(aes(col =  factor(group)), size = 3) +
    coord_tern() +
    theme_bw(base_size = 12)  +
    scale_color_manual(values = c("black", "purple"), labels = c("Group 1", "Group 2"),
                       name = "Data")+
    Tarrowlab("I") + Larrowlab("S") + Rarrowlab("R") + theme_showarrows() +
    labs(L = "", T = "", R = "") +
    labs(title = latex2exp::TeX("Same $R_0$, different time scales")) +
    theme(legend.position = "bottom")
g2
ggsave("diff-time-ternary.pdf", width = 6)

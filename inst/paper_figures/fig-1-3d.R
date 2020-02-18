## SKG
## Playing with SEIR/tetrahedron data
## 10/22/19


devtools::load_all()
library(plotly)
library(plot3D)
library(dplyr)



## look at data
data(agents_sims_tidy)
head(agents_sims_tidy)

agents1 <- agents_sims_tidy %>%
    dplyr::filter(sim == 1) %>%
    rename(max_time_S = SMax, max_time_I = IMax)

agg1 <- agents_to_aggregate_SIR(agents = agents1) %>%
    rename(x = S, y = I, z = R)





p <- plot_ly(agg1,  x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
             opacity = 1, line = list(width = 6, color = ~t, reverscale = FALSE),
             marker = list(color = ~t)) %>% add_markers() %>%
    layout(scene = list(xaxis = list(title = "# Susceptible"),
                        yaxis = list(title = "# Infectious"),
                        zaxis = list(title = "# Recovered"))
                                     
p

+
  plot_ly(data = verts)
chart_link = api_create(p, filename="scatter3d-basic")
chart_link
#########################
verts <- data.frame(name = c("top", "left", "right", "front"),
                    x = c(.5, 0, 1, .5),
                    y = c(sqrt(3)/2, 0, 0, sqrt(3)/6),
                    z = c(0, 0, 0, sqrt(6)/3))
tetrahedron <- verts[c(1, 2, 3, 1, 4, 2, 4, 3, 4),]
with(tetrahedron, scatter3D(x = x, y = y, z = z, type = "l",
                            col = "blue", lwd = 2,
          ticktype = "detailed", phi = 180, bty = "g"))
with(verts, text3D(x, y, z, labels = c("S", "E", "I", "R"), add = TRUE,
                   cex = 3))
data <- gg_df
scatter3D(x = data$x, y = data$y, z = data$z,
          col = "darkgreen", add = TRUE, pch = 16)

#### sim 2
parameters <- c(mu = 0 / (70 * 365), beta = 365 * 2 / 365,
                sigma = 1 / 14, gamma = 2 / 7)
initials <- c(S = 0.9, E = 1e-04, I = 1e-04, R = 1 - 0.9 - 1e-4 - 1e-4)

# Solve and plot.
seir2 <- SEIR(pars = parameters, init = initials, time = 0:(150))
df2 <- seir2$results %>% dplyr::rename(t = "time")
df2 <- SEIR_to_XYZ(df2)
scatter3D(x = df2$x, y = df2$y, z = df2$z,
          col = "purple", add = TRUE, pch = 16)
########## sim 3
parameters <- c(mu = 0 / (70 * 365), beta = 365 * 2 / 365,
                sigma = 1 / 100, gamma = 2 / 7)
initials <- c(S = 0.9, E = 1e-04, I = 1e-04, R = 1 - 0.9 - 1e-4 - 1e-4)

# Solve and plot.
seir3 <- SEIR(pars = parameters, init = initials, time = 0:(850))
df3 <- seir3$results %>% dplyr::rename(t = "time")
df3 <- SEIR_to_XYZ(df3)
scatter3D(x = df3$x, y = df3$y, z = df3$z,
          col = "red", add = TRUE, pch = 16)




#############################
#############################
## coloring by E
out <- SEIR_to_SIR_E(data = seir$results)
out %>% ggplot() +
  ggplot() +
    stat_sir(aes(x = SMax, y = IMax, init_state = init_state,
              col = group), alpha = .1,
             data_type = "fortified") +
 coord_tern() +
  labs(x = "S", y = "I", z = "R")

## SKG
## Playing with SEIR/tetrahedron data
## 10/22/19


devtools::load_all()
library(EpiDynamics)
library(plotly)
library(plot3D)

parameters <- c(mu = 0 / (70 * 365), beta = 365 / 365,
                sigma = 1 / 14, gamma = 1 / 7)
initials <- c(S = 0.9, E = 1e-04, I = 1e-04, R = 1 - 0.9 - 1e-4 - 1e-4)

# Solve and plot.
seir <- SEIR(pars = parameters, init = initials, time = 0:(150))
PlotMods(seir)

df <- seir$results
df <- df %>% dplyr::rename(t = "time")

gg_df <- SEIR_to_XYZ(data = df)

verts <- data.frame(name = c("top", "left", "right", "front"),
                    x = c(.5, 0, 1, .5),
                    y = c(sqrt(3)/2, 0, 0, sqrt(3)/6),
                    z = c(0, 0, 0, sqrt(6)/3))
tetrahedron <- verts[c(1, 2, 3, 1, 4, 2, 4, 3, 4),]
tet <- tetrahedron[, c("x", "y", "z")]
ggdf <- gg_df #%>% dplyr::select(-c(""))

p <- plot_ly(gg_df,  type = 'scatter3d', mode = 'lines',
             opacity = 1, line = list(width = 6, color = ~time, reverscale = FALSE)) +
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

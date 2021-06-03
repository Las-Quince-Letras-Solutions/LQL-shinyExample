
library('tidyverse')
library('rLQL')
library('smooth')
library('rlang')

set.seed(316)

regiones <- c('Valle de México', 'Bajío', 'Sureste', 'Norte')
regionesVec <- rep(regiones, each = 104)


# Simulaciones ------------------------------------------------------------
# Ventas
ventas <- matrix(nrow = 104, ncol = 4)
ventas[1,1:4] <- rnorm(4, 100000, 50000)

for(i in 2:104) {
  ventas[i,] <- ventas[i-1,]*rnorm(4, 1, 0.1)
}

# Distribución
dist <- matrix(nrow = 104, ncol = 4)

dist[,1] <- 100 * ventas[,1]/max(ventas[,1])
dist[,2] <- 100 * ventas[,2]/max(ventas[,2])
dist[,3] <- 100 * ventas[,3]/max(ventas[,3])
dist[,4] <- 100 * ventas[,4]/max(ventas[,4])

for(i in 1:104) {
  dist[i,] <- (dist[i,] + rnorm(4, 10, 5))*rnorm(4, 1, 0.05)
}

dist <- apply(dist, 2, function(x) { 100*x/max(x) })

# Precio
precio <- matrix(nrow = 104, ncol = 4)

precio[,1] <- rnorm(1, 25, 5) * ventas[,1]/max(ventas[,1])
precio[,2] <- rnorm(1, 25, 5) * ventas[,2]/max(ventas[,2])
precio[,3] <- rnorm(1, 25, 5) * ventas[,3]/max(ventas[,3])
precio[,4] <- rnorm(1, 25, 5) * ventas[,4]/max(ventas[,4])

for(i in 1:104) {
  precio[i,] <- (precio[i,] + rnorm(4, 2, 2))*rnorm(4, 1, 0.05)
}

precio <- max(precio) + 5 - precio

df <- data.frame(fecha = rep(seq(as.Date('2019-01-07'), as.Date('2020-12-28'), by = 'week'), times = 4),
                 region = factor(regionesVec),
                 ventas = gather(data.frame(ventas))$value,
                 dist = gather(data.frame(dist))$value,
                 precio = gather(data.frame(precio))$value)

var <- 'region'
var2 <- parse_expr(var)

df %>% 
  ggplot() +
  geom_line(aes(fecha, ventas, color = !!var2), size = 0.75, linetype = 9) +
  geom_smooth(aes(fecha, ventas, color = !!var2), 
              # color = 'grey30', 
              linetype = 1,
              size = 0.75, se = FALSE, method = 'gam') +
  scale_y_LQL('dollar', 1) +
  scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
  labsLQL(title = 'Ventas por región',
          y = 'Ventas') +
  themeLQL() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

df %>% 
  ggplot() +
  geom_line(aes(fecha, ventas, color = !!var2), size = 0.25, linetype = 1) +
  geom_smooth(aes(fecha, ventas, color = !!var2), 
              size = 0.75,
              linetype = 1,
              size = 0.75, se = FALSE, method = 'gam') +
  scale_y_LQL('dollar', 1) +
  scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
  labsLQL(title = 'Ventas por región',
          y = 'Ventas') +
  themeLQL() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

df %>% 
  ggplot() +
  geom_line(aes(fecha, dist, color = region), size = 0.75) +
  geom_smooth(aes(fecha, dist, color = region),
              size = 0.75, se = FALSE, method = 'gam') +
  scale_y_LQL('comma', 1) +
  scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
  labsLQL(title = 'Distribución por región',
          y = 'Puntos') +
  themeLQL() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

df %>% 
  ggplot() +
  geom_line(aes(fecha, precio, color = region), size = 0.75) +
  geom_smooth(aes(fecha, precio, color = region),
              size = 0.75, se = FALSE, method = 'gam') +
  scale_y_LQL('dollar', 1) +
  scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
  labsLQL(title = 'Precio por región',
          y = 'MXN') +
  themeLQL() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())


# Modelo ------------------------------------------------------------------

library('caret')
a <- dummyVars(~ ., data = df, fullRank = FALSE)
b <- predict(a, df) %>% data.frame()

mod.lm <- lm(ventas ~ .,
             data = select(b, -fecha))

summary(mod.lm)

resumen <- broom::tidy(mod.lm)

resumen %>% 
  mutate(significancia = ifelse(p.value < 0.05, '*', '')) %>% 
  na.omit()

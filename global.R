

library('shiny')
library('shinydashboard')
library('shinyWidgets')
# library("shinyMatrix")
library('shinythemes')


# library('tidyverse')
library('dplyr')
library('tidyr')
library('stringr')
library('ggplot2')
# library('smooth')
library('caret')
library('rlang')

library('shinyjs')
source("modules/module_login.R") 

set.seed(316)

regiones <- c('Valle de México', 'Bajío', 'Sureste', 'Norte')
regionesVec <- rep(regiones, each = 104)

varnames <- c('Ventas unitarias', 'Distribución', 'Precio unitario')
varshort <- c('ventas', 'dist', 'precio')

vars <- data.frame(full = varnames,
                   short = varshort)

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



# rLQL --------------------------------------------------------------------

stringEnRenglones <- function(x, charMax = 10){
  stopifnot(is.character(x))
  numPalabras <- lengths(gregexpr("[A-zÀ-ÿ]\\W+", x)) + 1L
  vecPalabras <- as.character(unlist(strsplit(x,"\\W+")))
  if(length(vecPalabras) > 1) {
    sizePalabras <- as.numeric(unlist(purrr::map(vecPalabras, stringi::stri_length)))
    charMax <- max(charMax, sizePalabras)
    stopifnot(max(sizePalabras) <= charMax)
    l <- 1
    i <- 1
    linestart <- integer()
    longitud <- integer()
    renglones <- character()
    while(i < numPalabras){
      linestart[[l]] <- i
      longitud[[l]] <- sizePalabras[[i]]
      while(i < numPalabras && (longitud[[l]] + sizePalabras[[i+1]]) <= charMax){
        longitud[[l]] <- longitud[[l]] + sizePalabras[[i+1]]
        i <- i+1
      }
      renglones[l] <- glue::glue_collapse(vecPalabras[linestart[[l]]:i]," ")
      i <- i+1
      l <- l+1
    }
    if(i == numPalabras){
      renglones[l] <- vecPalabras[i]
    }
    ans <- glue::glue_collapse(renglones, "\n")
    ans
  } else {
    ans <- x
    ans
  }
  
}

labsLQL <- function(title = NULL,
                    subtitle = NULL,
                    x = NULL,
                    y = NULL,
                    color = NULL,
                    size = NULL,
                    fill = NULL,
                    shape = NULL) {
  ggplot2::labs(title = if(!is.null(title)) toupper(title) else waiver(),
                subtitle = if(!is.null(subtitle)) toupper(subtitle) else waiver(),
                x = if(!is.null(x)) stringEnRenglones(x, 15) else waiver(),
                y = if(!is.null(y)) stringEnRenglones(y, 15) else waiver(),
                color = if(!is.null(color)) stringEnRenglones(color, 15) else waiver(),
                size = if(!is.null(size)) stringEnRenglones(size, 15) else waiver(),
                fill = if(!is.null(fill)) stringEnRenglones(fill, 15) else waiver(),
                shape = if(!is.null(shape)) stringEnRenglones(shape, 15) else waiver())
}

themeLQL <- function(x.angle = 0){
  base_size = 11
  base_family = ""
  base_line_size = base_size / 33
  base_rect_size = base_size / 22
  
  t <- ggplot2::theme(
    legend.box = 'vertical',
    legend.box.margin = margin(5,0,0,0),
    legend.box.just = 'left',
    legend.direction = 'horizontal',
    legend.justification = 'left',
    legend.key = element_rect(fill = NA),
    legend.position = 'bottom',
    legend.text = element_text(color = '#262626', margin = margin(0, 12, 0, -3)),
    legend.title = element_text(color = '#262626', size = rel(0.8), hjust = 1, margin = margin(0,10,0,0)),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey', size = base_line_size),
    panel.spacing = unit(35, 'pt'),
    strip.text = element_text(size = rel(1.1), color = '#262626'),
    axis.line = element_line(color = 'black', arrow = arrow(length = unit(0.25, "cm"))),
    axis.text = element_text(size = rel(0.9)),
    axis.text.x = element_text(angle = x.angle, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(margin=margin(0,5,0,00)),
    axis.ticks = element_blank(),
    axis.title.x = element_text(margin=margin(10,0,0,0), color = '#262626'),
    axis.title.y = element_text(angle = 0, hjust = 1, margin = margin(0,10,0,0), color = '#262626'),
    title = element_text(size = 14),
    plot.title = element_text(margin=margin(0,0,10,0), color = '#262626'),
    plot.subtitle = element_text(margin=margin(0,0,40,00),  color = '#666665'),
    
  )
}

scale_y_LQL <- function(formato = 'comma', precision = 1) {
  ggplot2::scale_y_continuous(expand = c(0.01,0.01),
                              labels = eval(rlang::parse_expr(paste0('scales::', formato, '_format(', precision, ')')))
  )
}

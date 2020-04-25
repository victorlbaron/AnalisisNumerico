#Se limpian los elementos creados con anterioridad
rm(list=ls())
#Se limpia la consola para una mejor visualizaciÃ³n 
cat("\014")
library(grid)
library(gridBezier)

#Se multiplica todos los vectores por 3.5 para agrandar la letra, puesto que con los valores originales
#la misma queda muy pequeña

x0 <- c(0.110, 0.150, 0.190, 0.198)*3.5
y0 <- c(0.260, 0.165, 0.010, 0.075)*3.5


#x1 <- c(0.108, 0.170, 0.060, 0.090)*3.5
#y1 <- c(0.040, 0.030, 0.060, 0.057)*3.5


x2 <- c(0.173, 0.205, 0.190, 0.250)*3.5
y2 <- c(0.090, 0.015, 0.080, 0.260)*3.5

grid.bezier(x0, y0, gp=gpar(lwd = 2, fill="black"))
#grid.bezier(x1, y1, gp=gpar(lwd = 2, fill="black"))
grid.bezier(x2, y2, gp=gpar(lwd = 2, fill="black"))
open3d()#Esta funcion nos abre una nueva ventana para graficos en 3D
bg3d("white")#Hace parte de la funcion RGL, puede configurar el entorno del fondo de la escena
material3d(col = "black")#Es un aspecto que indica si se debe ajustar la relacion de aspecto
volumenes <- c()   # tabla para ver valores finales
areas <-c()
figuras <-c("semi_esfera", "paraboloide", "mortero" )
options(digits=16)
#Aqui comenzamos a configurar la base del mortero---------------------------
x <- seq(-1, 1, length = 30)#Graficamos lineas por cuadrante para poder mostrar las zonas
y <- x

f <- function(x, y) { 
  r <- 2*x^2 + 2*y^2; r^2 #Manejamos los valores de los 3 ejes  
}

z <- outer(x, y, f)#OUTER aplica una funcion a dos matrices, pasamos x,y y la funcion f

#Con la funcion persp3d generamos dibujos trazados en superficies de 3 espacios,
#le pasamos los puntos a trazar X,Y,Z
#Aspect, es una indicacion logica de si se debe ajustar la relacion de aspecto
#Col, asigna un color al trazo dibujado
#Xlab, Ylab, Zlab, son los titulos para nuestros 3 ejes
#Xlim, Ylim, Zlim, son los limites de la region utilizada
#Add, Nos sirve para agregar puntos a una trama existente

persp3d(x, y, z-1.5, aspect = c(2, 2, 1), col = "blue",
        xlab = "X", ylab = "Y", zlab = "z", 
        xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1.5, 2),
        polygon_offset = 1)

persp3d(x, y, z-1.5, front = "lines", back = "lines",
        xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1.5, 2),
        lit = FALSE, add = TRUE)

#Aqui comenzamos a dibujar las boquillas/orejas del mortero
#Aplicamos Curvas de Bezier-------------------------------------------
#Especificamos valores param?tricos entre [0 y 1] para muestrear una curva de bezier 
t <- seq(0, 1, length=50)

#Puntos que controlan las curvas de bezier
p1 <- matrix(c(0.94,0.25,1.95, 1.2,0.2,1.95, 1.3,0,1.95, 1.2,-0.2,1.95, 0.94,-0.25,1.95), nrow=5, ncol=3, byrow=TRUE)
#Ploteamos la curva de bezier
plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#1B106B", size = 6, add = TRUE)
#Agregamos puntos que controlan la curva de bezier
p2 <- matrix(c(0.94,-0.25,1.95, 1.2,-0.2,1.95, 1.3,0,1.95, 1.2,0.2,1.95, 0.94,0.25,1.95), nrow=5, ncol=3, byrow=TRUE)
#Ploteamos la curva de bezier
plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#1B106B", size = 6, add = TRUE)
#Agreamos puntos que controlan la curva de bezier
p3 <- matrix(c(0.9,0,1, 1.25,0,1.35, 1.3,0,1.95, 1.25,0,1.35, 0.9,0,1.3), nrow=5, ncol=3, byrow=TRUE)
#Ploteamos la curva de bezier
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#1B106B", size = 6, add = TRUE)

#Aqui dibujamos la segunda boquilla/oreja del mortero
## BEZIER CONTROL POINTS
p1 <- matrix(c(-0.94,0.25,1.95, -1.2,0.2,1.95, -1.3,0,1.95, -1.2,-0.2,1.95, 0.94,-0.25,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#1B106B", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p2 <- matrix(c(-0.94,-0.25,1.95, -1.2,-0.2,1.95, -1.3,0,1.95, -1.2,-0.2,1.95, -0.94,-0.25,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#1B106B", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p3 <- matrix(c(-0.9,0,1, -1.25,0,1.35, -1.3,0,1.95, -1.25,0,1.35, -0.9,0,1.3), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#1B106B", size = 6, add = TRUE)

# Dibuja la tercera oreja del mortero valenciano
## BEZIER CONTROL POINTS
p1 <- matrix(c(0.25,0.94,1.95, 0.2,1.2,1.95, 0,1.3,1.95, 0.2,-1.2,1.95, 0.25,-0.94,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#1B106B", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p2 <- matrix(c(-0.25,0.94,1.95, -0.2,1.2,1.95, 0,1.3,1.95, 0.2,1.2,1.95, 0.25,0.94,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#1B106B", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p3 <- matrix(c(0,0.9,1, 0,1.25,1.35, 0,1.3,1.95, 0,1.25,1.35, 0,0.9,1.3), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#1B106B", size = 6, add = TRUE)

# Dibuja la cuarta oreja del mortero valenciano
## BEZIER CONTROL POINTS
p1 <- matrix(c(0.25,-0.94,1.95, 0.2,-1.2,1.95, 0,-1.3,1.95, 0.2,1.2,1.95, 0.25,0.94,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p1[1:3, ]), col = "#1B106B", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p2 <- matrix(c(-0.25,-0.94,1.95, -0.2,-1.2,1.95, 0,-1.3,1.95, 0.2,-1.2,1.95, 0.25,-0.94,1.95), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p2[1:3, ]), col = "#1B106B", size = 6, add = TRUE)
## BEZIER CONTROL POINTS
p3 <- matrix(c(0,-0.9,1, 0,-1.25,1.35, 0,-1.3,1.95, 0,-1.25,1.35, 0,-0.9,1.3), nrow=5, ncol=3, byrow=TRUE)
## PLOT A BEZIER CURVE
plot3d(bezier_points <- bezier(t=t, p=p3[1:3, ]), col = "#1B106B", size = 6, add = TRUE)

#Con estas funciones lo que hacemos es calcular algunos datos basicos del mortero

#Aqui calculamos el area de media esfera
area_semiesfera <- function (radio) {
  areas[1] <- (area = (4 * pi * (radio^2) )/2)
}

#Aqui calculamos el area de una boquilla(paraboloide)
area_paraboloide <- function (lado) {
  areas[2] <- (area = (lado * lado /2 ) * 3)
}

# Con los resultados de las dos funciones anteriores, calculamos el area de nuestro mortero
area_mortero <- function(p,s)  {
  area = p+s
}

areas[1] <- area_semiesfera (1)
areas[2] <- 4 * area_paraboloide(0.3)
areas[3] <- (area_mortero(areas[1],areas[2]))

# Aqui calculamos el volumen de media esfera
volumen_semiesfera <- function (radio_externo,radio_interno) {
  volumen_externo = (4/3 * pi * radio_externo^3) /2
  volumen_interno = (4/3 * pi * radio_interno^3) /2
  volumenes[1] <- (volumen_total = volumen_externo - volumen_interno)
}

# Aqui calculamos el volumen de una boquilla(paraboloide)
volumen_paraboloide <- function (lado_externo,lado_interno)  {
  h1 = sqrt( lado_externo^2 + (lado_externo/2) ^ 2)   #
  b1 = lado_externo * lado_externo / 2
  volumen_externo = 1/3 * b1 * h1
  h2 = sqrt( lado_interno^2 + (lado_interno/2) ^ 2)
  b2 = lado_interno * lado_interno / 2
  volumen_interno = 1/3 * b2 * h2
 volumen_total = volumen_externo - volumen_interno
  
}

# Con los resultados de las funciones anteriores calculamos el volumen del mortero
volumen_mortero <- function(p,s) {
    volumen = p + s
}

volumenes[1] <- volumen_semiesfera(1,0.99)
volumenes[2] <- volumen_paraboloide(0.3,0.29) * 4
# Imprimimos el volumen del mortero calculado
volumenes[3] <- (volumen_mortero(volumenes[1],volumenes[2]))


data.frame(figuras,areas,volumenes)



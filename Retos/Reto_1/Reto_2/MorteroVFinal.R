open3d()#Esta funcion nos abre una nueva ventana para gráficos en 3D
bg3d("white")#Hace parte de la funcion RGL, puede configurar el entorno del fondo de la escena
material3d(col = "black")#Es un aspecto que indica si se debe ajustar la relacion de aspecto
#install.packages("plot3D")
#Aqui comenzamos a configurar la base del mortero---------------------------
x <- seq(-1, 1, length = 30)#Graficamos lineas por cuadrante para poder mostrar las zonas
y <- x
f <- function(x, y) { 
  r <- 2*x^2 + 2*y^2; r^2 #Manejamos los valores de los 3 ejes  
}
z <- outer(x, y, f)#OUTER aplica una funcion a dos matrices, pasamos x,y y la funcion f
z[is.na(z)] <- 1 #La funcion IS.NA crea la mascara para cada bando

#Con la funcion persp3d generamos dibujos trazados en superficies de 3 espacios,
#le pasamos los puntos a trazar X,Y,Z
#Aspect, es una indicacion lógica de si se debe ajustar la relacion de aspecto
#Col, asigna un color al trazo dibujado
#Xlab, Ylab, Zlab, son los titulos para nuestros 3 ejes
#Xlim, Ylim, Zlim, son los limites de la region utilizada
#Add, Nos sirve para agregar puntos a una trama existente
persp3d(x, y, z-1.5, aspect = c(1, 1, 0.5), col = "blue",
        xlab = "X", ylab = "Y", zlab = "z", 
        xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1.5, 2),
        polygon_offset = 1)
persp3d(x, y, z-1.5, front = "lines", back = "lines",
        xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1.5, 2),
        lit = FALSE, add = TRUE)

#Aqui comenzamos a dibujar las boquillas/orejas del mortero
#Aplicamos Curvas de Bezier-------------------------------------------
#Especificamos valores paramétricos entre [0 y 1] para muestrear una curva de bezier 
t <- seq(0, 1, length=100)
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

#Con estas funciones lo que hacemos es calcular algunos datos básicos del mortero
#Aqui calculamos el area de media esfera
halfsphere.area = function(r)
{
  h = r
  
  area = 2 * pi * r * h
  
  area
}

#Aqui calculamos el area de una piramide
pyramid.area = function(l)
{
  area = (l*l/2)*3
  
  area
}

#Con los resultados de las dos funciones anteriores, calculamos el area de nuestro mortero
mortero.area = function() 
{
  area = halfsphere.area(1) + 4 * pyramid.area(0.5)
  
  area
}

mortero.area()

#Aqui calculamos el volumen de media esfera
halfsphere.volume = function(r)
{
  volume = (4/3 * pi * r^3)/2
  
  volume
}

#Aqui calculamos el volumen de una piramide 
pyramid.volume = function(l) 
{
  h = sqrt(l^2+(l/2)^2)
  b = l*l/2
  volume = 1/3 * b * h
  
  volume
}

#Con los resultados de las funciones anteriores calculamos el volumen del mortero
mortero.volume = function()
{
  volume = pyramid.volume(0.5) * 4 + halfsphere.volume(1)
  
  volume
}

#Mostramos el volumen del mortero calculado
mortero.volume()
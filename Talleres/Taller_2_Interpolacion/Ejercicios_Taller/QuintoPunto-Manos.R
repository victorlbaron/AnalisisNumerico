# Datos


#Coordenadas de victor
x=c(10, 8.2, 6.0, 6.8,  8.7,   9.8, 8.9, 8.5, 9.2, 10.0, 11.0, 11.5, 12.0,  12.8, 12.9, 13.0, 14.3,  15.6, 15.9, 14.5, 15.4, 16.8, 17.6,  15.7, 14.71)
y=c(16,  13, 9.7,   8.8, 10.5, 10.7, 7.2,   5,  4.6,  6.0,  8.2,  5.7,  4.2,   4.3,  5.8,  8.2,  5.8,   4.6,  5.0,  8.8,  8.3,  6.8,  7.1, 10.22, 14.33)


#Coordenadas de Estefania

#x=c(10,9.0, 8.2, 6.0, 6.7,  8.7,   9.8, 8.9, 8.5, 9.2, 10.0, 11.0, 11.5, 12.0,  12.8, 12.9, 13.0, 14.3,  15.2, 16,15.8, 14.5, 15.9, 17, 17.9,  15.7, 15.4,15.5)
#y=c(17, 15.2,  14, 10.7,   9.8, 11.5, 11.7, 8.2,6,  5.6,  7.0,  9.2,  6.7,  4.2,   4.3,  6.8,  9.2,  6.8, 5.3,  5.5,6.8, 9.8,  8.3,  6.7 ,  6.9, 11.22, 13.8, 14.2)

#Coordenadas de Oscar

#x=c(9.3, 8, 5.8, 6.6,  8.5,   9.6, 8.7, 8.4, 9.4, 9.8, 10.8, 11.3, 11.8,  12.6, 12.7, 12.8, 14.1,  15.4, 15.7, 14.7, 15.7, 16.6, 17.4,  15.8,15.1,14.8, 14.6)
#y=c(15,  13, 9.7, 9, 10.7,  10.7, 6.6, 4.8, 4.5,  6.0,  8.5,  5.0,  3.6,   3.7,  5.0,  8.4,  5.8,   4.6,  5.0,  8.8,  8.3,  7.2,  7.4, 10.2 ,13,14 , 14.3)

#Calculo del error de interpolaci?n
error<-function(x,f,s,n)
{
  err<-0
  sol<-0
  ans<-0
  for(i in 1:n)
  {
    h<-max(x[i+1]-x[i])
    err=h^(3/2)
    ans = f(x[i])-s[i]
    if(!is.null((ans< err))||!is.null((ans==err)))
    {
      sol[i]=ans
    }
  }
  return(sol)
}

#Imprime los resultados pertinentes
tabla<-function(x,y,err)
{
  datos<-data.frame(cbind(x,y,err))
  colnames(datos)<-c('X','Spline','Error')
  print(datos)
}

#Calculo del tama?o de paso "h"
#De la forma que h(i) = x(i+i) - x(i)
calHi<-function(x)
{
  n = length(x)
  h <- (c(x,0) - c(0,x))[2:n]
  return (h)
}

#Calculo de la matriz A que hace parte del sistema de ecuaciones As=B
#Es de la forma que cuando esta en la iteraci?n i
#Cuando (i,j) i = j = 2(hi+hi+1)
#Cuando (i,j-1) = hi
#Cuando (i,j+1) = hi+1
matrizA<-function(h,a,x)
{
  n = length(x)
  alph <- (3/c(1,h,1,1)*(c(a,1,1) - c(1,a,1)) - 3/c(1,1,h,1)*(c(1,a,1)-c(1,1,a)))[3:n]
  A <- c(1,rep(0,times=n-1))
  for (i in 1:(n-2)) {
    A <- rbind(A,c( rep(0,times=i-1) , c(h[i],2*(h[i]+h[i+1]),h[i+1]) , rep(0,times=n-i-2) ) )
  }
  A <- rbind(A,c(rep(0,times=n-1),1))
  
  return(A)
}

#Calculo de la matriz B que hace parte del sistema de ecuaciones As=B
#Es de la forma que cuando esta en la iteraci?n i
#Cuando (i,1) = 6((yi+2-yi+1)/hi+1-(yi+1-yi)/hi)
matrizB<-function(h,a)
{
  n = length(x)
  alph <- (3/c(1,h,1,1)*(c(a,1,1) - c(1,a,1)) - 3/c(1,1,h,1)*(c(1,a,1)-c(1,1,a)))[3:n]
  b <- c(0,alph,0)
  return (b)
}

#Despeje de la ecuaci?n de la forma AS=B en el cual obtenemos los valores de Spline
#que nos ayudar? a calcular los polinomios de interpolaci?n
calS<-function(A,b)
{
  c <- solve(A, b)
  return(c)
}

#Calculo de los coeficientes del polinomio de interpolaci?n Spline
#Recibe los spline del despeje de la ecuaci?n As=B
#los valores en X, Y y el tama?o de paso (h)
sNormal<-function(a,c,h)
{
  n = length(x)
  b <- ((c(a,0) - c(0,a))/c(1,h,1) - c(1,h,1)/3*(c(c,0) + 2*c(0,c)))[2:n]
  d <- ((c(c,0) - c(0,c))/(3*c(1,h,1)))[2:n]
  
  ans = rbind(a[1:n-1],b,c[1:n-1],d)
}

#Splie cubico, llama a las respectivas funciones para que la interpolacion funcione
sCubico<-function(x,y)
{
  h<-calHi(x)
  A<-matrizA(h,y,x)
  B<-matrizB(h,y)
  S<-calS(A,B)
  sNormal(y,S,h)
}

#Funcion que llama a las funciones de interpolar
#Graficar y calcular el error
splineCubico<-function(x,y)
{
  plot(x,y, pch=19, cex=1, col = "red", asp = 1, xlab="X", ylab = "Y", main = "Mano Spline Cubico")
  f = splinefun(x,y,method = "natural", ties = mean)
  t = 1:length(x)
  sx = sCubico(t,x)
  sy = sCubico(t,y)
  for (i in 1:(length(t)-1)) {
    dat<- data.frame(t=seq(t[i],t[i+1], by=0.1) )
    fx <- function(x) (sx[1,i] + sx[2,i]*(x-t[i]) + sx[3,i]*(x-t[i])^2 + sx[4,i]*(x-t[i]))
    fy <- function(x) (sy[1,i] + sy[2,i]*(x-t[i]) + sy[3,i]*(x-t[i])^2 + sy[4,i]*(x-t[i]))
    dat$y=fy(dat$t)
    dat$x=fx(dat$t)
    points(dat$x,dat$y,type='l', col='black')
    err=error(dat$x,f,dat$y,length(dat$x))
    tabla(dat$x,dat$y,err)
  }
}
splineCubico(x,y)

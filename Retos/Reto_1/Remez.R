


# Valores Modificab
grado = 3
#intervalo
ini = -pi/2
fin = pi/2
#Particion
TipoParticion = 1

# Codigo
funcion = function(x) {sin(x)}
x = c()
y = c()

numero_ecuaciones =  grado + 2
intervalo = ( 2*(fin)) / (numero_ecuaciones - 1 )
#Hallar particiones regulares
i = 1
while (length(x) < numero_ecuaciones)
{
  if (length(x) == 0 )
  {
    x[i]= ini;
    i = i +1
  }
  x[i] = (x[i-1] + (intervalo))
  i = i+1
}
chebyshev = function(intervaloA,intervaloB,numeros){#Funcion para hallar particiones
  puntos = c()
  
  for (i in numeros){
    puntos = c(puntos, (1/2*(intervaloA + intervaloB)) + (1/2*(intervaloB-intervaloA)) * cos((2*i-1) / (2 * length(numeros))* pi) )
  }
  return (puntos)
}
# Se definen los puntos a tomar y que tipo de partición
if(TipoParticion != 0 ){
  x = chebyshev(-pi/2,pi/2,seq(1,numero_ecuaciones))
}

#Se genera la matriz con Remez
aux = c(0)
j = 1
i = 1
r = 0
for(i in i:numero_ecuaciones^2){
  if(r > grado){
    r = 0
    aux[i] = (-1)^(j-1)
    j=j+1
  }
  else{
    aux[i] = (x[j])^r
    r = r+1
  }
  i=i+1
}

i=0
for(i in i:numero_ecuaciones){
  y[i] = funcion(x[i])
}
a = matrix(aux, nrow = numero_ecuaciones)
a = t(a) #Se hace la transpuesta de la matriz para que quede comodo de operar


p = solve(a,y) # se resuelve la ecuación para encontrar los coheficientes


#impresion de la base
plot(funcion,xlim = c(-4,4), ylim=c(-1.5,1.5), col = "orange", ylab="sin(x)")
abline(0,0, col = "gray")
abline(v=0, col = "gray")
par(new = TRUE)
plot(x,y, xlim = c(-4,4), ylim=c(-1.5,1.5), ylab="", col = "red")

#Creacion del polinomio como tal
if(grado == 1){
  r = function(x){p[1]+p[2]*x}
}
if(grado == 2){
  r = function(x){p[1]+p[2]*x+p[3]*x^2}
}
if(grado == 3){
  r = function(x){p[1]+p[2]*x+p[3]*x^2+p[4]*x^3}
}
if(grado == 4){
  r = function(x){p[1]+p[2]*x+p[3]*x^2+p[4]*x^3+p[5]*x^4}
}
if(grado == 5){
  r = function(x){p[1]+p[2]*x+p[3]*x^2+p[4]*x^3+p[5]*x^4+p[6]*x^5}
}
if(grado == 6){
  r = function(x){p[1]+p[2]*x+p[3]*x^2+p[4]*x^3+p[5]*x^4+p[6]*x^5+p[7]*x^6}
}
if(grado == 7){
  r = function(x){p[1]+p[2]*x+p[3]*x^2+p[4]*x^3+p[5]*x^4+p[6]*x^5+p[7]*x^6+p[8]*x^7}
}
if(grado == 8){
  r = function(x){p[1]+p[2]*x+p[3]*x^2+p[4]*x^3+p[5]*x^4+p[6]*x^5+p[7]*x^6+p[8]*x^7+p[9]*x^8}
}
if(grado == 9){
  r = function(x){p[1]+p[2]*x+p[3]*x^2+p[4]*x^3+p[5]*x^4+p[6]*x^5+p[7]*x^6+p[8]*x^7+p[9]*x^8+p[10]*x^9}
}


par(new = TRUE)
plot(r, xlim = c(-4,4), ylim=c(-1.5,1.5), col = "cyan4", ylab="")

arr = c()

# Se hallan los puntos entre las particiones
for (i in 1:numero_ecuaciones -1) {
    arr[i] = (x[i] + x[i+1]) / 2
}
sumError = 0
# Se calcula el error total
for (i in 2:numero_ecuaciones-1) {
error = abs(r(arr[i]) - funcion(arr[i]))
sumError = sumError + error
}
print(p)
print(sumError)




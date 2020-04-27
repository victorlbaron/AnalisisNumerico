
# Valores modificables
xo = 0.5
grado = 5
# el grado acepta valores desde 1-8


#Funcion y sus Derivadas
f = function(x){exp(x)}
d1= function(x){exp(x)}
d2= function(x){exp(x)}
d3= function(x){exp(x)}

# como las derivadas hacen un ciclo no hace falta crearlas todas
arr=c(f(xo),d1(xo),d2(xo),d3(xo))

grado = grado + 1

poli=c(0)

i=1
j=1
k=0
# se encuentra una parte de la expresion de Taylor
for(i in i:grado){
  if(j>4){
    j=1
  }
  poli[i] = ((arr[j])/factorial(k))#*(x-xo)^k
  k = k + 1
  j = j + 1
}

#impresion base
plot(f,xlim = c(-1,2), ylim=c(0,4), ylab="sin(x)", col = "orange")
abline(0,0, col = "gray")
abline(v=0, col = "gray")

#Dependiendo del grado complete y cree el polinomio
if((grado-1) == 1){
  final = function(x){poli[1]+poli[2]*(x-xo)}
}
if((grado-1) == 2){
  final = function(x){poli[1]+poli[2]*(x-xo)+poli[3]*(x-xo)^2}
}
if((grado-1) == 3){
  final = function(x){poli[1]+poli[2]*(x-xo)+poli[3]*(x-xo)^2+poli[4]*(x-xo)^3}
}
if((grado-1) == 4){
  final = function(x){poli[1]+poli[2]*(x-xo)+poli[3]*(x-xo)^2+poli[4]*(x-xo)^3+poli[5]*(x-xo)^4}
}
if((grado-1) == 5){
  final = function(x){poli[1]+poli[2]*(x-xo)+poli[3]*(x-xo)^2+poli[4]*(x-xo)^3+poli[5]*(x-xo)^4+poli[6]*(x-xo)^5}
}
if((grado-1) == 6){
  final = function(x){poli[1]+poli[2]*(x-xo)+poli[3]*(x-xo)^2+poli[4]*(x-xo)^3+poli[5]*(x-xo)^4+poli[6]*(x-xo)^5+poli[7]*(x-xo)^6}
}
if((grado-1) == 7){
  final = function(x){poli[1]+poli[2]*(x-xo)+poli[3]*(x-xo)^2+poli[4]*(x-xo)^3+poli[5]*(x-xo)^4+poli[6]*(x-xo)^5+poli[7]*(x-xo)^6+poli[8]*(x-xo)^7}
}
if((grado-1) == 8){
  final = function(x){poli[1]+poli[2]*(x-xo)+poli[3]*(x-xo)^2+poli[4]*(x-xo)^3+poli[5]*(x-xo)^4+poli[6]*(x-xo)^5+poli[7]*(x-xo)^6+poli[8]*(x-xo)^7+poli[9]*(x-xo)^8}
}
#impresión de la funcion generada y comparacion con la original
par(new = TRUE)
plot(final, xlim = c(-1,2), ylim = c(0,4), col = "cyan4", ylab= "")
par(new = TRUE)
plot(xo,f(xo), xlim = c(-1,2), ylim = c(0,4), col = "red", ylab= "")

#mestra del error en dos puntos
error = integrate(f,0,1)
error = error - integrate(final,0,1)
#error = abs(f(0)-final(0))
print(error*4)





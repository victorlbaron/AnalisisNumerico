

n = 4 # grado del polinomio

arr = c(2,0,-3,3,-4)
xo = -2
bn = c(0)
der = c(0)

bn[1] = arr[1] #se igualaal primer coeficiente del polinomio

# Se hace Horner para hallar Q(x) 
for(i in 2:(n+1)){
  bn[i] = arr[i]+bn[i-1]*xo
}

# Se hace horner pero con el Q(x_0) que es Equivalente a P'(x_0)
der[1] = bn[1]
for(i in 2:n){
  der[i] = bn[i]+der[i-1]*xo
}

derivada = der[length(der)]
print(derivada) # Derivada en el punto dado

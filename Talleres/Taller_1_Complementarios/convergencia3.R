#3 punto 2da parte

f = function(x){log(x+2)}

g = function(x){sin(x)}

Tolerancia = 0.00000001

q = function(x){log(x+2)-sin(x)}

Teorico = uniroot(q, c(-1.8,-1), tol = 1e-9) 

plot(f, xlim = c(-4,3), ylim=c(-3,3), ylab = "f(x) Y g(x)", col = "blue")
par(new = TRUE)
plot(g, xlim = c(-4,3), ylim=c(-3,3), ylab = "", col = "salmon")
plot(q, xlim = c(-3,5), ylim=c(-4,4), ylab = "f(x) - g(x)", col = "purple")
par(new = TRUE)
abline(0,0, col = "red")

a = -1.8
b = -1
arr = c(0)

arr[1] = a
arr[2] = b
i=3

Error1 = c() 
ErrorM1 = c()
 #valor -6

while(abs(Teorico$root-arr[i-2]) > Tolerancia){
  Error1[i-2]=abs(Teorico$root-arr[i-2])
  arr[i]= arr[i-1]-((q(arr[i-1])*(arr[i-1]-arr[i-2]))/(q(arr[i-1])-q(arr[i-2])))
  i = i+1
}

i=1
while(i<length(Error1)+1){
  ErrorM1[i]=Error1[i+1]
  i = i+1
}
plot(Error1,ErrorM1, type="l")

numero_iteraciones =length(arr)

Metodo2 = c(0)
Error2 = c(0)
Error2M1 = c(0)
Metodo2[1] = a
Metodo2[2] = b
i=3

while(abs(Teorico$root-Metodo2[i-2]) > Tolerancia){
  Error2[i-2]=abs(Teorico$root-arr[i-2])
  Metodo2[i]= Metodo2[i-1]-((q(Metodo2[i-1]))*(Metodo2[i-1]-Metodo2[i-2])/(q(Metodo2[i-1])-q(Metodo2[i-2])))
  i = i+1
}

i=1
while(i<length(Error2)+1){
  Error2M1[i]=Error2[i+1]
  i = i+1
}

plot(Error2,Error2M1, type ="l")


tab=data.frame(Error2,Error2M1)




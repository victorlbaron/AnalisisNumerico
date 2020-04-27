

x<-c(1,2) 
y<-c(2,6) 
plot(x,y,type="p", main = "Figura 2. Polinomio Interpolante ejercicio 3") 
p<-function(x) x^3-3*x^2+6*x-2 
plot(p,xlim = c(1,2),ylim = c(0,6),type="l",main = "Polinomio Interpolante",add=TRUE,col = "red")
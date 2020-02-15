#n representa el número de polinomios de taylor 
#x representa el exponente de la expresión
Aproximacion=function (n,x){
  suma=1
  i=n
  while(i>0){
    suma=1+x*suma/i
    cat("Polinomio: ",suma,"\n")
    i=i-1
  }
  cat(signif(suma,digits=5)) 
}
Aproximacion(5,0.5)
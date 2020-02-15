#PUNTO 2 - RAICES DE UNA ECUACION
puntob = function(x){
  resul = (x*(x+1)*(2*(x)+1))/6
  plot(resul)
  return (resul)
}
x = puntob(6)
y = puntob(7)
z = puntob(10)

graf=c(x,y,z)
plot(graf,type="l")
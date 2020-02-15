
resultadoIterativo = 0;
i = 0;
x = 1.0001;
#Inicio del Ciclo
repeat{
  
  resultadoIterativo=resultadoIterativo+x^i;
  i=i+1;
  if(i>50) break;
}

q=function(b) ((b^51)-1)/(b-1)
resultadoDirecto=q(x);
errorA=abs(resultadoDirecto-resultadoIterativo);
errorR=((resultadoIterativo/(resultadoIterativo+resultadoDirecto))*(errorA/resultadoIterativo))+((resultadoDirecto/(resultadoIterativo+resultadoDirecto))*(errorA/resultadoDirecto));
Datos = data.frame(i,errorA,errorR,resultadoIterativo,resultadoDirecto)
print(Datos);
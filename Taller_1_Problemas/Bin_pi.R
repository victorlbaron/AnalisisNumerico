#Pi en numero binario
print ("Pi en numero binario es: ")

parte_entera_pi = trunc (pi)
parte_decimal_pi = pi-3

residuo = 0
numero_residuos = 0
binario <- c()


# Parte entera pi
while (parte_entera_pi > 0){
  
    residuo = trunc (parte_entera_pi %% 2)
    parte_entera_pi = trunc (parte_entera_pi / 2)
    numero_residuos = numero_residuos + 1
    
    binario[numero_residuos] = residuo
}
    
  numero_binario <- c()
  j <- 0
  for(i in 1 : numero_residuos){
    numero_binario[i] = binario [numero_residuos-j]
    j = j+1
  }
  print (numero_binario)

  
  
  
binario_decimal <- c()
# Parte decimal de pi
for (i in 1:13) {
  
  parte_decimal_pi = parte_decimal_pi * 2;
  
  if(parte_decimal_pi >= 1) {
    
    parte_decimal_pi = parte_decimal_pi - 1;
    binario_decimal[i] = 1
  }
  else {
    binario_decimal[i] = 0
    
  }
}
  cat ("Pi en numero binario de 15 bits es: " , numero_binario, binario_decimal)
  
   

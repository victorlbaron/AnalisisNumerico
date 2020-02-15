rm(list = ls())

# Parte binaria del numero decimal convertido a binario: 

decimal_a_binario <- function (parte_decimal) {
  
  bits_permitidos <- 10
  round (parte_decimal , 1)
  binario <- c()
  
  for (i in 1 : bits_permitidos) {
    
    parte_decimal = parte_decimal * 2;
    
    
    if(parte_decimal >= 1) {
      
      parte_decimal = parte_decimal - 1
      binario[i] = 1
      
    }
    
    else
    {
       binario[i] = 0
    }
  }
  return(binario)
}


# Parte entera del numero decimal convertido a binario:

entero_a_binario <- function (parte_entera) {
  
  residuo = 0
  numero_residuos = 0
  binario <- c()
  
  while (parte_entera > 0) {
    
    residuo = (parte_entera %% 2)
    parte_entera = trunc (parte_entera / 2)
    numero_residuos = numero_residuos + 1
    
    binario[numero_residuos] = residuo
    
    
  }
  numero_binario <- c()
  j <- 0
  for(i in 1 : numero_residuos){
    numero_binario[i] = binario [numero_residuos-j]
    j = j+1
  }
  return (numero_binario)
}

cat("Primero numero de base 10 a binario: ", entero_a_binario(11) , "." ,decimal_a_binario (0.25), "\n")
cat("Segundo Numero a Base 10 a binario: ", "0" , "." , decimal_a_binario (2/3), "\n")
cat("Tercer Numero a Base 10 a binario: ", entero_a_binario(30), "." , decimal_a_binario(0.6), "\n")
cat("Cuarto Numero a Base 10 a binario: ", entero_a_binario(99), "." , decimal_a_binario(0.9), "\n")

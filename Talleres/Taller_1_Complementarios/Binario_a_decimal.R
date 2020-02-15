# Parte entera del binario convertido a base 10
binario_a_entero <- function (parte_entera) {
  
  numero_potencia = 0
  binario = 0
  total = 0
  
  while(parte_entera > 0) {
    
    binario = parte_entera %% 10
    total = total + (binario*(2 ^ numero_potencia))
    numero_potencia = numero_potencia + 1
    parte_entera = parte_entera %/% 10
  }
  
  return (total)
  
}

# Parte decimal del binario convertida a base 10

binario_a_decimal <- function (parte_decimal) {
  
  vector_binario <- c()
  i = 1
  
  while(parte_decimal > 0){
    
    vector_binario[i] = parte_decimal %% 10
    parte_decimal = parte_decimal %/% 10
    i = i + 1 
    
  }
  
  j <- 0
  numero_decimal <- c()
  for(i in 1 : length(vector_binario)) {
    numero_decimal[i] = vector_binario[length(vector_binario)-j]
    j = j+1
  }
  
  
  
  binario = 0
  total = 0

  numero_potencia=1
  print(numero_decimal)
 
  for(i in 1 : length(numero_decimal)) {
    
    binario = numero_decimal[i]
    if(binario == 1){
    total = total + 1/(2 ^ numero_potencia)
    }
    numero_potencia = numero_potencia +1
  }
   
  
  return(total)
  
  
}

cat("Primer Numero: ", binario_a_entero (101010101),"\n")

cat("Segundo Numero: ",binario_a_entero(1011) + binario_a_decimal(101),"\n")

cat("Tercer Numero: ",binario_a_entero(10111) + binario_a_decimal(010101))

cat("Cuarto Numero: ",binario_a_entero(111) + binario_a_decimal(1111))

#n representa el numero cuyo raiz quiero encontrar
#Error represental la tolerancia o el error del resultado
squareRoot = function(n, Error)
{
  x = 100;
  y = 0.5*( x + n / x);
  repeat
  {
    if( abs(x - y) < Error)
    {
      cat("Raíz aproximada de ",n, " es: ", y , " Con error de " ,abs(x-y))
      break;
    }
    x = y
    y = 0.5 * (x+n/x)
  }
  
}
squareRoot(7,1e-9)
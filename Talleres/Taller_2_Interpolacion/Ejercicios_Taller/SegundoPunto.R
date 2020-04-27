


require(pracma) ## Loading required package: pracma 
library(pracma) 

x <- c(0,1,2) 
y <- c(10,15,5) 

xi <- seq(0,2,by=1) 

f <- cubicspline(x, y, xi = NULL, endp2nd = FALSE, der = c(1,1))
ffun <- function(xi) ppval(f,xi)
print(f) 
## $breaks 
## [1] 0 1 2
## 
## $coefs 
##       [,1]   [,2]  [,3] [,4] 
## [1,] -4.25   1.00  8.25   10 
## [2,]  4.25 -11.75 -2.50   15 
##  
## $pieces
## [1] 2 
##  
## $order 
## [1] 4 
##  
## $dim 
## [1] 1 
##  
## attr(,"class") 
## [1] "pp" 
plot(x,y, main="Interpolación Spline Cúbico") 
plot(ffun, xlim=c(-1,3), add=TRUE, col = rainbow(1), main="Figura 1.
     Interpolación Spline Cúbico") 
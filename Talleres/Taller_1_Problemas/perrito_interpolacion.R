x=c( 00.50 , 01.01 , 05.85 , 07.46 , 11.28 , 15.20 , 18.46 , 21.25 
     , 24.15 , 25.80 , 28.00, 30.80 , 30.81 , 29.40 , 27.40 , 26.21 ,
     24.97 , 20.32 , 19.54 , 18.80 , 14.04 , 12.54  , 11.68 , 09.55 , 
     08.30 , 09.10 , 08.85 , 07.80 , 00.50) 
y=c( 02.40 , 02.95 , 03.86 , 05.41 , 07.45 , 06.30 , 04.49 , 07.15 , 
     07.05 , 05.80 , 05.85, 04.50 , 02.40 , 01.20 , 00.80 , 00.44 ,
     00.54 , 01.01 , 00.80 , 01.08 , 00.98 , 01.08, 01.33 , 01.00 ,
     01.64 , 02.65 , 02.70 , 02.24 , 02.40) 

segx1=x[1:7] 
segx2 = x[7:12] 
segx3 = x[12:14] 
segx4 = x[14:15] 
segx5 = x[15:18] 
segx6 = x[18:20] 
segx7 = x[20:25] 
segx8 = x[25:26] 
segx9 = x[26:28] 
segx10 = x[28:29] 
segy1 = y[1:7]     
segy2 = y[7:12]    
segy3 = y[12:14]   
segy4 = y[14:15]   
segy5 = y[15:18]   
segy6 = y[18:20]   
segy7 = y[20:25]   
segy8 = y[25:26] 
segy9 = y[26:28] 
segy10 = y[28:29] 
seg = spline(segy3,segx3)
i = seg$x 
seg$x = seg$y 
seg$y = i 
plot(x, y,sub="Interpolación Perro",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx1, segy1), col = "green",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx2, segy2), col = "green",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx4, segy4), col = "green",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx5, segy5), col = "green",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx6, segy6), col = "green",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx7, segy7), col = "green",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx8, segy8), col = "green",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx9, segy9), col = "green",xlim=c(0,31),ylim=c(0,9)) 
lines(spline(segx10, segy10), col = "green",xlim=c(0,31),ylim=c(0,9))
lines(seg, col = "green") 
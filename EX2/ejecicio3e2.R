#ejercicio 3
rm(list = ls(all.names = TRUE))
gc()

empresakent<-data.frame(ventas=c(27, 33, 22, 26, 28,12, 10, 15, 19, 11,23, 20, 18, 17,11, 17, 16, 14, 15),diseño=c(rep(1,5),rep(2,5),rep(3,4),rep(4,5)))

empresakent$diseño<-as.factor(empresakent$diseño)
summary(empresakent)
empresa
#i)
library(GGally)
ggpairs(empresakent[,c(1,2)])
#nos damos cuenta que estamos trabajando con una variable categórica de 4 niveles
#que representan los distintos diseños. Los niveles 1 2 y 4 tienen la misma proporción y el empaque 3 es el de menor propoerción
#el mínimo de la variable ventas es 10 y el máximo 33
levels(empresakent$diseño)
empresakent$diseño <- relevel(empresakent$diseño, ref = "4")


#ii)
#nuestro modelo seria Y= B0++B1diseño1+B2diseño2+B3diseño3
#
fit1=lm(ventas ~ diseño, data = empresakent)
summary(fit1)
#número de ventas promedio

#para el diseño 1
#E[Y|Y|x_1=diseño1]=B0+B1(0)+B2(0)+B3(0) = B0

#para el diseño 2
#E[Y|Y|x_1=diseño2]=B0+B1(1)+B2(0)+B3(0) = B0+B1

#para el diseño 3
#E[Y|Y|x_1=diseño3]=B0+B1(0)+B2(1)+B3(0) = B0+B2

#para el diseño 4
#E[Y|Y|x_1=diseño4]=B0+B1(0)+B2(0)+B3(1) = B0+B3

#estimaciones puntuales

#para el diseño 4
#E[Y|Y|x_1=diseño1]=14.600-1.200(0)+4.900(0) 
14.600

#para el diseño 2
#E[Y|Y|x_1=diseño2]=14.600-1.200(1)+4.900(0) +12.600(0)=14.600-1.200=13.400
a=14.600 -1.200 
a

#para el diseño 3
#E[Y|Y|x_1=diseño3]=14.600-1.200(0)+4.900(1) +12.600(0)=14.600+4.900=19.500
b=14.600+ 4.900 
b

#para el diseño 1
#E[Y|Y|x_1=diseño4]=14.600-1.200(0)+4.900(0) +12.600(1)=14.600+12.600=27.200
c=14.60+12.600
c

#iii)

# Los intervalos de confianza

 
K=matrix(c(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1), ncol=4, nrow=4, byrow=TRUE)
m=c(0,0,0,0)
confint(glht(fit1, linfct=K, rhs=m), level=.95)
plot(confint(glht(fit1, linfct=K, rhs=m), level=.95))


#iv) Prueba ANOVA(Usando la prueba lineal general)

#Aqui se contrasta las pruebas Ho: Bi=0 vs Ha: Bi=!0 para alguna i=1,2,3 
library(multcomp)
K=matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1  ), ncol=4, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit1, linfct=K, rhs=m),test=Ftest())
#posible interpretacion
# La prueba asociada a la tabla anova nos indica que se rechaza Ho, es decir,
# las ventas están asociadas al menos a uno de los empaques.

#v) ¿Se puede considerar que el diseño del empaque afecta las ventas promedio? Use alpha=0.5. Argumente.
#por inciso 2 sabemos que:
#E[Y|x_1=diseño4]=beta_0
#E[Y|x_1=diseño1]=beta_0+beta_1
#E[Y|x_1=diseño2]=beta_0+beta_2
#E[Y|x_1=diseño3]=beta_0+beta_3
#Si el diseño del empaque no afectará las ventas promedio, por ende  tendríamos: 
#E[Y|x_1=diseño4]=E[Y|x_1=diseño1]=E[Y|x_1=diseño2]=
#E[Y|x_1=diseño3]
#Lo cual ocurre si y sólo si beta_1=beta_2=beta_3=0
#Esto es justo lo que probamos en el inciso anterior con la tabla Anova.
#por otra parte lo podemos confirmar como se hizo en clase
drop1(fit1, test="F")
#Obtenemos el mismo resultado, rechazamos H_0  por tanto concluimos que el diseño
#de empaque sí afecta las ventas promedio.

#vi Realice la prueba de hipótesis simultánea asociada a la igualdad de las ventas promedio entre todos los posibles pares de diferentes empaques. Use alpha=.05 Interprete los resultados
#psobles combinaciones
#E[Y|x_1=diseño4]=beta_0= E[Y|x_1=diseño1]=beta_0+beta_1, E[Y|x_1=diseño4]=beta_0=E[Y|x_1=diseño2]=beta_0+beta_2, E[Y|x_1=diseño4]=beta_0= E[Y|x_1=diseño3]=beta_0+beta_3, E[Y|x_1=diseño1]=beta_0+beta_1=E[Y|x_1=diseño2]=beta_0+beta_2, E[Y|x_1=diseño1]=beta_0+beta_1=E[Y|x_1=diseño3]=beta_0+beta_3, 
#E[Y|x_1=diseño2]=beta_0+beta_2 = E[Y|x_1=diseño3]=beta_0+beta_3
K=matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1, 0,1,-1,0, 0,1,0,-1, 0,0,1,-1 ), ncol=4, nrow=6, byrow=TRUE)
m=c(0,0, 0,0,0,0)

summary(glht(fit1, linfct = mcp(diseño = "Tukey")))
#con la salida de la prueba realizada nos damos cuenta que hay evidencia estadística que indica que  si hay una diferencia significativa entre los empaques 1 y 4, 2 y 1, y entre los empaques 3 y 1 
#pues el p-value para esas pruebas es menor a alpha = .05, en las otras comparaciones no podemos hacer ninguna afirmación pues no se rechaza la prueba 

#Vi) Suponga que los ejecutivos de la empresa tienen la sospecha de que el diseño de empaque 1 es el que aumenta las ventas en comparación con el resto de empaques. Realice una prueba de hipótesis para argumentar en favor o en contra de esta hipótesis de acuerdo con los datos observados. Use $\alpha=.05$
#queremos comprobar lo sigueinte 
#E[Y|x_1=diseño1]=beta_0+beta_1 > E[Y|x_1=diseño4]=beta_0 y E[Y|x_1=diseño1]=beta_0+beta_1>E[Y|x_1=diseño2]=beta_0+beta_2  y E[Y|x_1=diseño1]=beta_0+beta_1 E[Y|x_1=diseño3]=beta_0+beta_3
#Nuestra hipótesis alternativa está dada por:
#H_1: beta_1>0  y  beta_1-beta_2>0 y beta_1-beta_3>0
K=matrix(c(0,1,0,0, 0,1,-1,0, 0,1,0,-1 ), ncol=4, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit1, linfct=K, rhs=m, alternative="greater"))
#con la salida anterior nos damos cuenta que se rechazaron las 3 pruebas para un p- value de .05,  por ende hay evidencia estadística que indica que efectivamente el empaque 1 aumenta más las ventas en comparación a los demás empaques. 

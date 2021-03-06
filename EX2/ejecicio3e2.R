#ejercicio 3
rm(list = ls(all.names = TRUE))
gc()

empresakent<-data.frame(ventas=c(27, 33, 22, 26, 28,12, 10, 15, 19, 11,23, 20, 18, 17,11, 17, 16, 14, 15),dise�o=c(rep(1,5),rep(2,5),rep(3,4),rep(4,5)))

empresakent$dise�o<-as.factor(empresakent$dise�o)
summary(empresakent)
empresa
#i)
library(GGally)
ggpairs(empresakent[,c(1,2)])
#nos damos cuenta que estamos trabajando con una variable categ�rica de 4 niveles
#que representan los distintos dise�os. Los niveles 1 2 y 4 tienen la misma proporci�n y el empaque 3 es el de menor propoerci�n
#el m�nimo de la variable ventas es 10 y el m�ximo 33
levels(empresakent$dise�o)
empresakent$dise�o <- relevel(empresakent$dise�o, ref = "4")


#ii)
#nuestro modelo seria Y= B0++B1dise�o1+B2dise�o2+B3dise�o3
#
fit1=lm(ventas ~ dise�o, data = empresakent)
summary(fit1)
#n�mero de ventas promedio

#para el dise�o 1
#E[Y|Y|x_1=dise�o1]=B0+B1(0)+B2(0)+B3(0) = B0

#para el dise�o 2
#E[Y|Y|x_1=dise�o2]=B0+B1(1)+B2(0)+B3(0) = B0+B1

#para el dise�o 3
#E[Y|Y|x_1=dise�o3]=B0+B1(0)+B2(1)+B3(0) = B0+B2

#para el dise�o 4
#E[Y|Y|x_1=dise�o4]=B0+B1(0)+B2(0)+B3(1) = B0+B3

#estimaciones puntuales

#para el dise�o 4
#E[Y|Y|x_1=dise�o1]=14.600-1.200(0)+4.900(0) 
14.600

#para el dise�o 2
#E[Y|Y|x_1=dise�o2]=14.600-1.200(1)+4.900(0) +12.600(0)=14.600-1.200=13.400
a=14.600 -1.200 
a

#para el dise�o 3
#E[Y|Y|x_1=dise�o3]=14.600-1.200(0)+4.900(1) +12.600(0)=14.600+4.900=19.500
b=14.600+ 4.900 
b

#para el dise�o 1
#E[Y|Y|x_1=dise�o4]=14.600-1.200(0)+4.900(0) +12.600(1)=14.600+12.600=27.200
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
# las ventas est�n asociadas al menos a uno de los empaques.

#v) �Se puede considerar que el dise�o del empaque afecta las ventas promedio? Use alpha=0.5. Argumente.
#por inciso 2 sabemos que:
#E[Y|x_1=dise�o4]=beta_0
#E[Y|x_1=dise�o1]=beta_0+beta_1
#E[Y|x_1=dise�o2]=beta_0+beta_2
#E[Y|x_1=dise�o3]=beta_0+beta_3
#Si el dise�o del empaque no afectar� las ventas promedio, por ende  tendr�amos: 
#E[Y|x_1=dise�o4]=E[Y|x_1=dise�o1]=E[Y|x_1=dise�o2]=
#E[Y|x_1=dise�o3]
#Lo cual ocurre si y s�lo si beta_1=beta_2=beta_3=0
#Esto es justo lo que probamos en el inciso anterior con la tabla Anova.
#por otra parte lo podemos confirmar como se hizo en clase
drop1(fit1, test="F")
#Obtenemos el mismo resultado, rechazamos H_0  por tanto concluimos que el dise�o
#de empaque s� afecta las ventas promedio.

#vi Realice la prueba de hip�tesis simult�nea asociada a la igualdad de las ventas promedio entre todos los posibles pares de diferentes empaques. Use alpha=.05 Interprete los resultados
#psobles combinaciones
#E[Y|x_1=dise�o4]=beta_0= E[Y|x_1=dise�o1]=beta_0+beta_1, E[Y|x_1=dise�o4]=beta_0=E[Y|x_1=dise�o2]=beta_0+beta_2, E[Y|x_1=dise�o4]=beta_0= E[Y|x_1=dise�o3]=beta_0+beta_3, E[Y|x_1=dise�o1]=beta_0+beta_1=E[Y|x_1=dise�o2]=beta_0+beta_2, E[Y|x_1=dise�o1]=beta_0+beta_1=E[Y|x_1=dise�o3]=beta_0+beta_3, 
#E[Y|x_1=dise�o2]=beta_0+beta_2 = E[Y|x_1=dise�o3]=beta_0+beta_3
K=matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1, 0,1,-1,0, 0,1,0,-1, 0,0,1,-1 ), ncol=4, nrow=6, byrow=TRUE)
m=c(0,0, 0,0,0,0)

summary(glht(fit1, linfct = mcp(dise�o = "Tukey")))
#con la salida de la prueba realizada nos damos cuenta que hay evidencia estad�stica que indica que  si hay una diferencia significativa entre los empaques 1 y 4, 2 y 1, y entre los empaques 3 y 1 
#pues el p-value para esas pruebas es menor a alpha = .05, en las otras comparaciones no podemos hacer ninguna afirmaci�n pues no se rechaza la prueba 

#Vi) Suponga que los ejecutivos de la empresa tienen la sospecha de que el dise�o de empaque 1 es el que aumenta las ventas en comparaci�n con el resto de empaques. Realice una prueba de hip�tesis para argumentar en favor o en contra de esta hip�tesis de acuerdo con los datos observados. Use $\alpha=.05$
#queremos comprobar lo sigueinte 
#E[Y|x_1=dise�o1]=beta_0+beta_1 > E[Y|x_1=dise�o4]=beta_0 y E[Y|x_1=dise�o1]=beta_0+beta_1>E[Y|x_1=dise�o2]=beta_0+beta_2  y E[Y|x_1=dise�o1]=beta_0+beta_1 E[Y|x_1=dise�o3]=beta_0+beta_3
#Nuestra hip�tesis alternativa est� dada por:
#H_1: beta_1>0  y  beta_1-beta_2>0 y beta_1-beta_3>0
K=matrix(c(0,1,0,0, 0,1,-1,0, 0,1,0,-1 ), ncol=4, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit1, linfct=K, rhs=m, alternative="greater"))
#con la salida anterior nos damos cuenta que se rechazaron las 3 pruebas para un p- value de .05,  por ende hay evidencia estad�stica que indica que efectivamente el empaque 1 aumenta m�s las ventas en comparaci�n a los dem�s empaques. 

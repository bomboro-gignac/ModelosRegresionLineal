#ejercico 5
rm(list = ls(all.names = TRUE))
gc()

setwd("C:/Users/eliot/Desktop/VerificacionSupuestosA")
Datos=read.csv("Ex5.csv", header=TRUE )
str(Datos)
summary(Datos)


#i)
#la variable tratamiento es dicotmica con dos niveles, control y Med
#la proporción entre los niveles no es la misma pues el nivel control es el doble del nivel Med (200 y 100) respectivamente
#la variable Edad es de tipo numérica, donde la edad mínima es 16 y la máxima 60
#en la mayoría de las edades notamos que la variable med está por encima de la variable control
with(Datos, plot(Edad, Ant, col=c("red", "blue")[Datos[,2]] ))
legend("topleft",levels(Datos[,2]), col=c("red", "blue"), pch = c(0,0) )


levels(Datos$Trat)



#ii)

# Se empieza con el modelo que incluye interacciones
# y= b0 + b1Med+ b2Edad + b3(Med*Edad) 
#Datos$Trat <- relevel(Datos$Trat, ref = "Med")
fit=lm(Ant~Trat+Edad+Trat:Edad, data=Datos)
summary(fit)


##Supuestos
#1. Linealidad: E(Y|Med+Edad +Med*Ead)= y= b0 + b1Med+ b2Edad + b3(Med*Edad) 
#2. Homocedasticidad: varianza constante, V(e_i)=sigma2
#3. Normalidad de los errores.
#4. Covarianza entre los errores es cero. 
#Independencia de los errores cuando se asume normalidad.



#Linealidad en E(Y|Med+Edad +Med*Ead)
plot(fit, 1)
library(car)
residualPlots(fit)
residualPlots(fit, terms= ~ Trat,fitted=FALSE)
residualPlots(fit, terms= ~ Edad,fitted=FALSE)
#notamos que tanto globalmente como marginalmente se cumple el supuesto de linealidad


#Varianza constante 

plot(fit, 3)

library(lmtest)
lmtest::bptest(fit)

#No se rechaza Ho por ende  no hay evidencia estadística que indique que hay relación entre
#la varianza con las variables que considera el modelo ajustado

#Normalidad 

plot(fit, 2)
#graficamente, a excepción de unos pocos datos, se cumple el supuesto de normalidad pues los datos se acoplan a la recta
library(broom)
Datosfit=augment(fit)
shapiro.test(Datosfit$.std.resid)

#no se rechaza Ho, por ende no hay evidencia estadística que indique que los datos no provengan de una distribución normal


#Pruebas de aleatoriedad

#Prueba de rachas
library(lawstat)
lawstat::runs.test(Datosfit$.std.resid, plot.it = TRUE)
#no se rechaza Ho por ende no hay evidencia estadística que indique que 
#no hay aleatoriedad en los datos

#inciso iv. ¿Se puede decir que la edad afecta de la misma forma la efectividad del medicamento en el grupo control
#y en el grupo que recibe el medicamento? Realice una prueba de hipótesis apropiada e interprete.
#se ajustó el modelo 
# y= b0 + b1Med+ b2Edad + b3(Med*Edad) 


#E(Y|Trat=control, X2)= b0 + b2Edad
#E(Y|Trat=med, X2)= b0 + b1 + b2Edad + b3Edad = (b0 + b1) + (b2 + b3) X2

#Observemos que el efecto de la edad en la efectividad media es la misma para los 2 tratamientos cuando
#los coeficientes asociados a la Edad son iguales para ambos niveles (med y control). Es decir, cuando:
#b2= (b2+b3)

#Por lo tanto, probar que el efecto de la edad en la efectividad media (numero de anticuerpos) es la misma para el grupo de control y el que recibió el medicamento
#equivalente a realizar la prueba de hipótesis:

#H0: b3=0  vs Ha: b3!=0 
#usando multcomp
library(multcomp)
K=matrix(c(0,0,0,1), ncol=4, nrow=1, byrow=TRUE)
m<-0
summary(glht(fit, linfct=K, rhs=m), test=Ftest())
#se rechaza Ho, por ende hay evidencia estadística que indica que B3 =! 0, y por ende podemos afirmar que
#la edad NO afecta de la misma forma la efectividad del medicamento en el grupo control
#y en el grupo que recibe el medicamento

#v. Comente sobre el ajuste del modelo incluyendo la interpretación de cada uno de los coeficientes. 
#el modelo es uno del tipo ancova, se explica el nivel de anticuerpos a través del tratamiento que se aplica, la edad y también se incluye la variable que denota la interacción 
#entre la variable edad y el grupo que recibió medicamento. A través de el inciso anterior nos damos cuenta que  hay una dependencia de la edad para 
#en el aumento de producción de anticuerpos una vez suministrado el medicamento, lo que se etraduce como efectividad del medicamento
#entonces la variable asociada a la interacción de la aplicaión del medicamento y la edad es significativa

#interpretación de los coeficientes

fit$coefficients


#interpretación
#B_0 = 15.28716698 se puede interpretar como el nivel medio de anticuerpos de una persona de edad 0 sin recibir tratamiento

#B_1 = 2.45674 , condicionando en valores fijos de la variable edad , se puede interpretar como el incremento en la efectividad media de 2.45674 unidades para los pacientes
#recibieron el medicamento con respecto a los pacientes de la amisma edad fija  que no recibieron medicamento (nivel de referencia).
#Es decir, condiconando en una edad fija, la efectividad media de los pacientes que recibieron el medicamento aumenta

#B_2 = -0.07609043, condicionando en algún nivel de la variable tratamiento, se puede interpretar como el decremento de 0.07609043 unidades en la efectividad media por año adicional
#de las personas que recibieron culauiera de los dos tratamientos.
#Es decir, condiconando en una un nivel de la variable tratamiento, la efectividad media de los pacientes que recibieron cualquiera de los dos tratamientos disminuye por
#cada año adicional del paciente.

#B_3 = 0.04046,  condicionando en valores fijos de la variable edad, se puede interpretar como el aumento de 0.04046 unidades en la efectividad media
#por año adicional de los pacientes que recibieron el medicamento con respecto a los pacientes del grupo control (nivelde referencia). 
#Es decir, condiconando en una edad fija, la efectividad media de los pacientes que recibieron el medicamento aumenta por
#cada año adicional del paciente.


#vi) Argumente en contra o a favor de la afirmación: "El medicamente funciona aumentando el número de anticuerpos para todos los pacientes entre 16 y 60 años". Se puede apoyar de pruebas de hipótesis o intervalos de confianza simultáneos. 
#para ello utlizaremos intervalos de confianza simultáneos

summary(Datos)


edad <- seq(from = 16, to = 60, by = .5)
length(edad)

# confianza a 95%

#Para una banda para la recta del grupo control
#E(Y|TRT=A, X2)= b0 + b2 edad
KA <- cbind(1, 0, edad, 0)

#Para una banda para la recta del grupo medicado
#E(Y|TRT=B, X2)=  b0 + b1 age +b2 + b4 age = (b0 + b1) + (b2 + 3) edad
KB <- cbind(1, 1, edad,edad)


K=rbind(KA, KB)

fitE <- glht(fit, linfct = K)
fitci <- confint(fitE, level = 0.95)

lines(edad, coef(fitE)[1:89], col="red")
lines(edad, fitci$confint[1:89,"upr"], col="red")
lines(edad, fitci$confint[1:89,"lwr"], col="red")

lines(edad, coef(fitE)[90:178], col="green")
lines(edad, fitci$confint[90:178,"upr"], col="green")
lines(edad, fitci$confint[90:178,"lwr"], col="green")



#notamos que las bandas no se traslapan en ningún punto de la variable edad, es decir el límite inferior de la banda del grupo medicado es mayor al límite superior de la banda 
#del grupo que no recibió medicamento 

#eso significa que hay evidencia estadística con un 95% de confianza de que 
#para una persona con cualquier edad entre los 16 y 60 años,  el nivel de anticuerpos es mayor para las personas que recibieron el medicamento en comparación a las personas que no lo recibieron

#por ende podemos concluir que para las personas de todas las edades entre 16 y 60 años, efectivamente el medicamento sí aumenta el número de anticuerpos, lo que se traduce como la efectividad del medicamento.






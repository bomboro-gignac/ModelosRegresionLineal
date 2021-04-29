rm(list = ls(all.names = TRUE))
gc()

library(readr)
setwd("C:/Users/eliot/Desktop/VerificacionSupuestosA")
Datos=read.csv("Ex7.csv", header=TRUE )
str(Datos)
View(Datos)
str(Datos)
summary(Datos)

Datos$X1=factor(Datos$X1)
Datos$X2=factor(Datos$X2)
str(Datos)
summary(Datos)

library(GGally)
#ggpairs(Datos)

#Notemos que tenemos 2 variables categoricas con 4 niveles cada una y 5 continuas, nuestros datos son 400
#hay numeros negativos en 3 de las 5 variables continuas. Por otro lado las variables X4 y X6 tienen una relación significativa
#negativa y positivamente, respec.


#i) verificando supuestos

fit1 <- lm(Y~., data = Datos)
summary(fit1)

plot(fit, 1)
library(car)
residualPlots(fit1)

#podemos notar que la variable x4 es la que se esta poniendo loca, no hay aleatoriedad
# y la curva no se parece en nada a la recta que pasa por el cerapio 0.
residualPlots(fit1, terms= ~ X2,fitted=FALSE)
#Analisando las variables categoricas, notamos que hay un problema, pues las medianas estan lejos del cero.

library(lmtest)
lmtest::bptest(fit1)
# Sabemos que H0: la varianza es constante vs Ha: hay relación, y buscamos no rechazar H0
# EN este caso rechazamos la hipotesis nula, entonces parece que hay una relación 

#library(car)
car::ncvTest(fit1)
car::ncvTest(fit1,~X1) # H0 no se rechaza, entonces la la varianza no tiene una relación lineal con X1
car::ncvTest(fit1,~X3) # se rechaza H0
car::ncvTest(fit1,~X4) # H0 no se rechaza, entonces la la varianza no tiene una relación lineal con X4
car::ncvTest(fit1,~X5) # Se rechaza
car::ncvTest(fit1,~X6) # H0  no se rechaza

#Normalidad 
#Se basa en los residuales estandarizados o estudentizados
# H0: los datos provienen de una distribución normal vs Ha:No provienen de una distribución normal
#Buscamos que no se rechace H0

plot(fit, 2)
library(broom)
Datosfit1=augment(fit1)
shapiro.test(Datosfit1$.std.resid)
library(nortest)
nortest::lillie.test(Datosfit1$.std.resid)
library(normtest)
normtest::jb.norm.test(Datosfit1$.std.resid)
# vemos que el p-value es pequeño en todas las pruebas, entonces se rechaza H0

summary(Datos)
#vemos que en en x4 y y, se pueden hacer tranformaciones, debido a que sus datos son
#no negativos


summary(powerTransform(fit1)) #Es necesario una transformación y se rechaza la logaritmica
#lambada=0.4 si queremos transformar a la variable y
yp<-((Datos$Y^(0.4))-1)/0.4
fit2<-lm(yp~., data = Datos)
plot(fit2, 1) # vemos que aún hay un patron
plot(fit2, 2) #aún vemos que no se cumple normalidad
plot(fit2, 3)
plot(fit2, 5)

residualPlots(fit2)# las categoricas ya se van controlando, mientas que la variable X4 aún se pone rejega
# y un poco x6

# verificando supuestos, segunda vuelta

#Homocedasticidad
car::ncvTest(fit2)
car::ncvTest(fit2,~X1) # H0 no se rechaza, entonces la la varianza no tiene una relación lineal con X1
car::ncvTest(fit2,~X3) # se rechaza H0, por poco
car::ncvTest(fit2,~X4) # H0 no se rechaza, entonces la la varianza no tiene una relación lineal con X4
car::ncvTest(fit2,~X5) # H0 no se rechaza
car::ncvTest(fit2,~X6) # H0 no se rechaza

# Normalidad
Datosfit2=augment(fit2)
shapiro.test(Datosfit2$.std.resid)
nortest::lillie.test(Datosfit2$.std.resid)
normtest::jb.norm.test(Datosfit2$.std.resid)
 
# Se rechaza normalidad ya que nuestro p values son pequeños 


# como en X4 hay datos no negativos, entonces procederemos a hacer un prueba para transformar
boxTidwell(yp~X4,~X1+X2+X3+X5+X6,data = Datos)
# Debemos hacer una transformación
l=1.7
X4p<-Datos$X4^l


fit2<-lm(yp~X1+X2+X3+X4+X4p+X5+X6, data = Datos)
plot(fit2, 1) # vemos que aún hay un patron
plot(fit2, 2) # Aún no se cumple normalidad, unos datos siguen sin domarse
plot(fit2, 3) 
plot(fit2, 5)

#linealidad
residualPlots(fit2) #Vems que casi todo esta bien, pero hay algo que afecta a nuestro modelo
# Esto huele a que tenemos datos muy raros

#Homocedasticidad
car::ncvTest(fit2) 
car::ncvTest(fit2,~X1)
car::ncvTest(fit2,~X2)
car::ncvTest(fit2,~X3)
car::ncvTest(fit2,~X4p) # es la unica que rechaza 
car::ncvTest(fit2,~X5)
car::ncvTest(fit2,~X6)

#normalidad
BDfit2=augment(fit2)
shapiro.test(BDfit2$.std.resid)
nortest::lillie.test(BDfit2$.std.resid)
normtest::jb.norm.test(BDfit2$.std.resid)
# Se rechaza, p-values pequeños

# aleatoriedad
acf(BDfit2$.std.resid) #no cumple aleatoriedad

require(car)
influencePlot(fit2) # notemos que el dato 287 es el que influye más en nuestros datos
outlierTest(fit2)

#Quitando el outlier
BD<-Datos[-287,]
#veamos si necesita otra transformación la variable x o y
fit2<-lm(Y~X1+X2+X3+X4+X5+X6, data = BD) 
summary(powerTransform(fit1)) # La variable Y necesita la transformación
yp<-((BD$Y^(0.5))-1)/0.5
boxTidwell(yp~X4,~X1+X2+X3+X5+X6,data = BD)# la varible x4 necesita transformacion
l=2
X4p<-BD$X4^l

#Con el nuevo modelo tenemos.
fit2<-lm(yp~X1+X2+X3+X4+X4p+X5+X6, data = BD)
plot(fit2, 1) 
plot(fit2, 2) 
plot(fit2, 3)
plot(fit2, 5)

#linealidad
residualPlots(fit2) #se cumple

#Homocedasticidad
car::ncvTest(fit2) #se
car::ncvTest(fit2,~X1)
car::ncvTest(fit2,~X2)
car::ncvTest(fit2,~X3)
car::ncvTest(fit2,~X4p)
car::ncvTest(fit2,~X5)
car::ncvTest(fit2,~X6)
#Todas rechazan H0, entonces decimos que hay relación lineal


#normalidad
BDfit2=augment(fit2)
shapiro.test(BDfit2$.std.resid)
nortest::lillie.test(BDfit2$.std.resid)
normtest::jb.norm.test(BDfit2$.std.resid)

# Los p-values son grandes, por lo que decimos que hay normalidad, es decir 

# aleatoriedad
acf(BDfit2$.std.resid) #cumple aleatoriedad

#######

#Verificando que se cumplen los supuestos, procederemos a realizar una selección de variables
# MEdiante el metodo del mejor subconjunto
# El criterio que tomaremos será por BIC, R^2adj y Cp
# Pero antes agamos un descriptivo
ggpairs(BD)
summary(BD)
str(BD)
#Podemos notar que son significativas las variables x6 y x4 respecto a la variable Y
#vemos que nuestros datos son numericos y ya estan como factores nuestras variables categoricas

library(leaps)
subconts<-regsubsets(yp~X1+X2+X3+X4+X4p+X5+X6, data = BD, nbest=2, nvmax=10)
summary(subconts)

subconts2=summary(subconts)
combine <- cbind(subconts2$which,subconts2$rsq,subconts2$adjr2,subconts2$cp,subconts2$bic)
ndim=dim(subconts2$which)
dimnames(combine)[[2]][(ndim[2]+1):(ndim[2]+4)]=c("R^2", "R^2_adj", "Cp", "BIC")
round(combine, digits=4)
#Notamos que el R^2adj el modelo que tiene 8 variables es bueno, además que el BIC es el más pequeño
#Mientras con el cp la diferencia es un poco grande pero aceptable.

coef(subconts, 15) # notamos que las variables son altamente significativa, por lo que nuestra selección es buena

#ajustando los modelos
modfit<-lm(yp~X1+X2+X4p+X6, data=BD)
summary(modfit)
anova(modf)
#vemos que no se puede reducir más el modelo debido a que los p-values son muy pequeños, además nuestro modelo esta
#muy bien explocado, con un 99.84%

#Sacamos los datos importantes de este modelo
broom::glance(modfit)

#nuestro modelo consta de 8 variables, y un R^2adj de 99.8%, mientra que nuestro BIC es de 1751, es diferente
# al que vimos en la seleccion del mejor subconjunto pues falta agregar la penalización correspondiente al parametro sigma

# iii)

#esperanzas de y dado cada nivel de las variables categóricas
# MODELO que se ajustó ... y = bo + b1X1A2+b2X1A3+b3X1A4+b4X2B2+b5X2B3+b6X2B4+b7X4p+b8X6 

#E(Y|X1 = A1, X2=b1, X4p,X6) =bo + b7X4p+b8X6 
#E(Y|X1 = A2, X2=b1, X4p,X6) =bo + b1 b7x4p + b8x6
#E(Y|X1 = A3, X2=b1, X4p,X6) = bo + b2+ b7x4p + b8x6
#E(Y|X1 = A4, X2=b1, X4p,X6) = bo + b3+ b7x4p + b8x6


#E(Y|X1 = A1, X2=b2, X4p,X6) =bo + b4 + b7X4p+b8X6 
#E(Y|X1 = A2, X2=b2, X4p,X6) =bo + b1 +b4+ b7x4p + b8x6
#E(Y|X1 = A3, X2=b2, X4p,X6) = bo + b2 +b4+ b7x4p + b8x6
#E(Y|X1 = A4, X2=b2, X4p,X6) = bo + b3 +b4+ b7x4p + b8x6



#E(Y|X1 = A1, X2=b3, X4p,X6) =bo + b5 + b7X4p+b8X6 
#E(Y|X1 = A2, X2=b3, X4p,X6) =bo + b1 +b5+ b7x4p + b8x6
#E(Y|X1 = A3, X2=b3, X4p,X6) = bo + b2 +b5+ b7x4p + b8x6
#E(Y|X1 = A4, X2=b3, X4p,X6) = bo + b3 +b5+ b7x4p + b8x6


#E(Y|X1 = A1, X2=b4, X4p,X6) =bo + b6+ b7X4p+b8X6 
#E(Y|X1 = A2, X2=b4, X4p,X6) =bo + b1 + b6+ b7x4p + b8x6
#E(Y|X1 = A3, X2=b4, X4p,X6) = bo + b2+ b6+ b7x4p + b8x6
#E(Y|X1 = A4, X2=b4, X4p,X6) = bo + b3+ b6+ b7x4p + b8x6


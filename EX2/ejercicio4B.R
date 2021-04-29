rm(list = ls(all.names = TRUE))
gc()
library(readr)

ruta<-file.choose()
datos<-read.csv(ruta)

View(datos)

# i) Descriptivo
summary(datos)
(boxplot(Puntaje ~ Sexo + Trat, data = datos))
str(datos)

datos$Sexo=factor(datos$Sexo)
datos$Trat=factor(datos$Trat)
str(datos)

levels(datos$Sexo)
levels(datos$Trat)
library(GGally)
ggpairs(datos)
# notemos que tenemos una variable continua (Puntaje) y dos variables categoricas
# las cuales estan bien proporcionadas, la variables Trat cuenta con 3 niveles, mientras que
# la variable categorica Sexo cuenta con dos niveles
# a simple vista parece ser que el tratamiento 1 es mejor para el hombre y mujer
# ya que se va pareciendo a una normal, mientras que control y el tratamiento nuevo decepcionan

#ii)
# Se empieza con el modelo que incluye interacciones
# y= b0 + b1x1 + b2x2 + b3x3 + b4(x3*x1) + b5(x3*x2)

mod1<-lm(Puntaje ~ Sexo*Trat, data = datos)
summary(mod1)

#Sea   Y = Puntaje
#     x1= variable dicotomica asociado al sexo mujer
#     x2= variable dicotomica asociada al tratamiento 1
#     x3= variable dicotomica asociada al tratamiento 2
# Recordemos que nuestras variableas dicotomicas asociadas son hombre y control
# por esto tenemos que E[Y| hombre, control]= b0

#E[Puntaje|control, hombre]= b0
#E[Puntaje|trat1, hombre]  = b0 + b2x2 = b0 + b2
#E[Puntaje|trat2, hombre]  = b0 + b3x3 = b0 + b3
#E[Puntaje|control, mujer] = b0 + b1x1 = b0 + b1
#E[Puntaje|trat1, mujer]   = b0 + b1x1 + b2x2 + b4(x3*x1) = b0 + b1 + b2 + b4 
#E[Puntaje|trat2, mujer]   = b0 + b1x1 + b3x3 + b5(x2*x3) = b0 + b1 + b3 + b5

#Las estimaciones promedio son
b0= as.numeric(mod1$coefficients[1])
b1= as.numeric(mod1$coefficients[2])
b2= as.numeric(mod1$coefficients[3])
b3= as.numeric(mod1$coefficients[4])
b4= as.numeric(mod1$coefficients[5])
b5= as.numeric(mod1$coefficients[6])


#Dado que nuestras variables son categoricas, nuestros valores van a ser 1 o 0 si está xi con i=1...5
#E[Puntaje|control, hombre]= b0 de donde 
b0
#E[Puntaje|trat1, hombre]  = b0 + b2x2 = b0 + b2
b0 + b2*1
#E[Puntaje|trat2, hombre]  = b0 + b3x3 = b0 + b3
b0 + b3*1
#E[Puntaje|control, mujer] = b0 + b1x1 = b0 + b1
b0 + b1*1
#E[Puntaje|trat1, mujer]   = b0 + b1x1 + b2x2 + b4(x3*x1) = b0 + b1 + b2 + b4 
b0 + b1*1 + b2*1 + b4*1*1
#E[Puntaje|trat2, mujer]   = b0 + b1x1 + b3x3 + b5(x2*x3) = b0 + b1 + b3 + b5
b0 + b1*1 + b3*1 + b5*1*1

# iii)Calculo de intervalos de confianza al 95%
library(multcomp)
X=matrix(c(1,0,0,0,0,0,
           1,0,1,0,0,0,
           1,0,0,1,0,0,
           1,1,0,0,0,0,
           1,1,1,0,1,0,
           1,1,0,1,0,1), ncol=6, nrow=6, byrow=TRUE)
m=c(0,0,0,0,0,0)
confint(glht(mod1, linfct=X, rhs = m), level = 0.95)
plot(confint(glht(mod1, linfct=X, rhs = m), level = 0.95))

# iv) Hipotesis que contrastan con la tabla ANOVA
#Aqui se contrasta las pruebas Ho: mi=mj vs Ha: mi=mj para alguna i=0,1,2,3,4,5
#E[Puntaje|control, hombre]= m0
#E[Puntaje|control, mujer] = m1
#E[Puntaje|trat1, hombre]  = m2
#E[Puntaje|trat1, mujer]   = m3
#E[Puntaje|trat2, hombre]  = m4
#E[Puntaje|trat2, mujer]   = m5

set <- aov(Puntaje~Sexo*Trat, data = datos)
summary(set)

#Con un 95% de confianza notamos que ambas variables categoricas son significativas
#mientras que su interacción tambien lo es

# v) 

##E[Puntaje|control, hombre]= b0 - #E[Puntaje|control, mujer] = b0 + b1 = 0(abuso de notación, sorry)
#b0-(b0+b1)=0 => b1=0

#E[Puntaje|trat1, hombre]  = b0 + b2 - #E[Puntaje|trat1, mujer] = b0 + b1 + b2 + b4
#b0+b2-(b0+b1+b2+b4)=0 => b1+b4=0

#E[Puntaje|trat2, hombre]  = b0 + b3 - #E[Puntaje|trat2, mujer] = b0 + b1 + b3 + b5
#b0 + b3 - (b0+b1+b3+b5)=  => b1+b5=0

X=matrix(c(0,1,0,0,0,0,
           0,1,0,0,1,0,
           0,1,0,0,0,1), ncol=6, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(mod1, linfct=X, rhs=m), test=Ftest())

# se rechaza H0,Hay evidencia estadistica para decir que el sexo tiene un efecto en el puntaje
# procederemos a hacer una prueba simultanea para ver

# ¿En cuál tratamiento podemos identificar que el sexo tiene algun efecto?

summary(glht(mod1, linfct=X, rhs=m))

# vemos que a los que no se le aplico ningun tratamiento (control) y los que se les aplico el tratamiento nuevo (trat2) el sexo no tiene efecto, con el 95%
# de confianza, mientras que el tratamiento actual (trat1) es el relevante, entonces reducimos el modelo a:
# y = b0 + b1x2 + b2x3 + b4(x1*x2)
# Donde
#Sea  Y = Puntaje
#     x1= variable dicotomica asociada al sexo mujer
#     x2= variable dicotomica asociada al tratamiento 1
#     x3= variable dicotomica asociada al tratamiento 2

fit <- lm(Puntaje ~ Trat + I((Trat == "Trat1")*I(Sexo == "Mujer")), data = datos)
summary(fit)

#Las estimaciones promedio son
b0= as.numeric(fit$coefficients[1])
b1= as.numeric(fit$coefficients[2])
b2= as.numeric(fit$coefficients[3])
b3= as.numeric(fit$coefficients[4])
b4= as.numeric(fit$coefficients[5])
b5= as.numeric(fit$coefficients[6])


#Estimaciones puntuales
# E[Puntaje|control] = b0
b0
# E[Puntaje|trat1, hombre]= b0 + b1x1
b0+b1*1
# E[Puntaje|trat1, mujer]= b0 + b1x1 + b3x3
b0+b1*1+b3*1*1
# E[Puntaje|trat2]= b0 + b2x2
b0 + b2*1

# vii) Realizando una prueba de hipotesis para argumentar si estamos a favor o en contra de
# ¿El nuevo tratamiento tiene mejor desempeño?
# E[Puntaje|control] = b0 < E[Puntaje|trat2]=b0 + b2
# b0< b0 + b2 ==> H0:b2>0 vs Ha: b2<=0

# E[Puntaje|trat1, mujer]= b0 + b1 + b3 < E[Puntaje|trat2, mujer]= b0 + b2
# b0 + b1 + b3 < b0 + b2 ==> H0: -b1 + b2 -b3 >0 va Ha:  -b1 + b2 -b3 <=0

# E[Puntaje|trat1, hombre]= b0 + b1 < E[Puntaje|trat2]=b0 + b2
# b0 + b1 < b0 + b2 ==> H0: -b1 + b2 > 0 vs Ha: -b1 + b2 >= 0

X=matrix(c(0,0,1,0,
           0,-1,1,-1,
           0,-1,1,0), ncol = 4, nrow = 3, byrow = T)
m=c(0,0,0)
summary(glht(fit, linfct=X, rhs=m, alternative="greater"))

# Con un 95% de confianza podemos decir que es bueno el tratamiento nuevo, pero no mejor que el actual

# viii) ¿El tratamiento actual es el "más" mejor? 

# E[Puntaje|trat1, mujer]=b0 + b1 + b3> E[Puntaje|control] = b0 
# b0 + b1 + b3 > b0 ==> H0:b1 + b3 > 0 vs Ha: b1 + b3 <=0

# E[Puntaje|trat1, hombre]= b0 + b1 > E[Puntaje|control] = b0
# b0 + b1 > b0 ==> H0:b1>0 vs Ha: b1<=0

# E[Puntaje|trat1, mujer]= b0 + b1 + b3 > E[Puntaje|trat2, mujer]= b0 + b2
# b0 + b1 + b3 > b0 + b2 ==> H0: b1 - b2 + b3 >0 va Ha:  b1 + b2 + b3 <=0

# E[Puntaje|trat1, hombre]= b0 + b1 > E[Puntaje|trat2]=b0 + b2
# b0 + b1 < b0 + b2 ==> H0: b1 - b2 > 0 vs Ha: b1 - b2 >= 0
X=matrix(c(0,1,0,1,
           0,1,0,0,
           0,1,-1,1,
           0,1,-1,0), ncol = 4, nrow = 4, byrow = T)
m=c(0,0,0,0)
summary(glht(fit, linfct=X, rhs=m, alternative="greater"))

#Podemos decir que el tratamiento actual reduce lo niveles de ansiedad
# es el "más" mejor


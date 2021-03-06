rm(list = ls(all.names = TRUE))
library(readr)
library(broom)
library(lmtest)
library(car)
library(nortest)
library(normtest)
library(latex2exp)
library(randtests)
library(ALSM)
#Problema 8
Est <- c(1,2,3,4,5,6,7,8)
G <- c(3.6,2.2,3.1,3.5,2.9,2.6,3.9,2.6)
H <- c(3,15,8,9,12,12,4,16)

Datos8 <- data.frame(cbind(Est, G, H))
Datos8


#Consideremos G = Y (variable dependiente) y H = X
# G = Porcentaje de GPA del estudiante en el semestre anterior
# H = Promedio de numero de horas a la semana que el estudiante paso en el bar(alias cafe 97)
#procederemos a calcular los siguientes valores para que todo sea mas r�pido y sencillo.
options(digits=5)
(xbar=mean(Datos8$H))
(SSx=sum((Datos8$H-xbar)^2))
(ybar = mean(Datos8$G))
(SSy=sum((Datos8$G - ybar)^2))
(SSxy = sum((Datos8$G - ybar)*(Datos8$H - xbar)))
(beta1= SSxy/SSx)
(beta0=ybar-beta1*xbar)
Datos8$"X^2"<- (Datos8$H)^2 
Datos8$"X_i*Y_i"<- Datos8$H*Datos8$G
Datos8$yhat <- beta0+beta1*Datos8$H
Datos8$"(y-yhat)^2" <- (Datos8$G -Datos8$yhat)^2
Datos8$"(y-ybar)^2" <- (Datos8$G - ybar)^2
Datos8$"(yhat-ybar)^2"<- (Datos8$yhat - ybar)^2
ggplot(data = Datos8, aes(x=H, y=G)) +
  geom_point() +
  geom_smooth(method = "lm")

#i). �Dir�as que un modelo lineal simple servir�a para describir la relaci�n entre G (puntaje del GPA) y H (horas a la semana en el bar)?
#Podemos ver que mientras va creciendo el numero de horas, los estudiantes van teniendo
#un GPA menor, en cambio, si pasa menos horas en el bar su GPA es mayor.
#Por lo tanto tiene sentido.

#ii)Ajusta un modelo lineal simple. Da la ecuaci�n de la recta.
#Con los resultados que anteriormente calculamos, definimos
betahat <- beta1
alphahat <- beta0
#Asi tenemos que el modelo lineal simple esta definido como 
# yhat = beta0+beta1*X_i 
yhat <- beta0+beta1*Datos8$H #Donde Datos8$H = x_i

#Podemos interpretar lo siguiente:
#alphahat: Si el estudiante no pasa horas en el bar, en promedio su GPA es de 4.14704
#betahat: En promedio por cada hora que pase el estudiante en el bar, en promedio 
#su puntuaje de GPA disminuye 0.11109

#ii)Encuentra deltahat^2, las desviaciones est�ndar estimadas para alphahat y betahat, sus intervalos de confianza y el R^2.
#�Qu� puedes decir al respecto?
#Sea sigmahat^2 = sigmahat2, Donde sigmahat^2 = sum(y-yhat)^2/n-2
n=length(Datos8$Est)
sigmahat2 <- (sum(Datos8$`(y-yhat)^2`))/(n-2) ##cambiar a sigma
#Por lo tanto
sigmahat = sqrt(sigmahat2) # Es la dispersi�n de los datos con respecto a la funci�n de regresi�n
#Procederemos a calcular las desviacion est�ndar estimada para alphahat y betahat
#De las clases sabemos que:
#VAr(alphahat)=sigmahat^2*(sum(x_i^2)/(n*SSx) y 
#Var(betahat)=sigmahat^2/SSx
#Como ya calculamos los datos que necesitamos, entonces solo aplicamos formulazos.
Var_alpht <- sigmahat2*((sum(Datos8$`X^2`))/(n*SSx))
Var_betht <- sigmahat2/SSx
#por lo tanto la desviacion estandar de cada uno es:
desv_alpht<-sqrt(Var_alpht) #Nos indica la dispersi�nde los datos, en promedio 
desv_betht<-sqrt(Var_betht) #Nos indica la dispersi�nde los datos, en promedio

#Pasemos a calcular los intervalos de confianza al 95%
#Para beta -> betahat +- t*raiz(var(var_betht))  t de student
#A mano
alpha = 0.05
tcuantil1<-qt(1-alpha/2, n-2, lower.tail = TRUE)
print(c(betahat-tcuantil1*desv_betht, betahat+tcuantil1*desv_betht))

#Pero con R lo obtenemos mas f�cil
fit8<-lm(G ~ H, data = Datos8)
confint(fit8, level = 0.95)

#interpretaci�n
#Afirmamos con el 95% de confianza, que por cada hora (en promedio) que el estudiante pase en bar
#la disminuci�n de GPA es entre 0.1624 y 0.059781
#Y Con la misma confianza, podemos decir que si el estudiante no va al bar, en promedio
#su puntuaje de GPA esta entre 3.5911 y 4.70296

#Ahora calculemos R^2

#Sabemos que R^2 = (sum(yhat-ybar)^2/sum(y_i - ybar)^2)
#Como ya sacamos los resultados simplemente aplicaremos la formula
R2 <- (sum(Datos8$`(yhat-ybar)^2`)/sum(Datos8$`(y-ybar)^2`))
#Es decir que el 82.38% de la variaci�n en el GPA es explicada por el n�mero promedio
#de horas que el estudiante pasa en el bar. 

#iv)Realiza la prueba t para contrastar H0:beta=0 vs Ha:beta!=0
#H0: nos dice que no hay relacion lineal entre las horas que se pasa en el bar
#Y el puntuaje de GPA que se obtuvo anteriormente vs Ha: Si hay relaci�n lineal entre H y G

#Calculemos nuestra estadistica de prueba.
#Dado el ejercicio 6 tenermos que 
#t = betahat/raiz(sigmmahat^2/SSx)
t. <- betahat/sqrt(sigmahat2/SSx)
abst <- abs(t.) #|t*|
#mientras que
t <- qt(0.975,6)

#Como |t*|>t entonces rechazamos H0, con un nivel de significancia de 0.05, asi las cosas
#concluimos que si hay relacion lineal entre horas que se pasa en el bar
#Y el puntuaje de GPA que se obtuvo anteriormente

# v)Obt�n la tabla ANOVA y realiza la prueba asociada con una significancia de .05

anova(fit8)
#Denotamos a F_1,6,0.95 = F_16.95 a la prueba F
F_16.95 <- (qt(0.975,6))^2
# De la tabla anova y de F_16.95 vemos que Fanoava > F_1,6,0.95 i.e
#28.065>5.987
#Rechaza,ps H0 y concluimos que hay relaci�n linesal entre en promedio horas que visitas el
# bar y el puntuaje GPA obtenido anteriormente

#vi) Suponga ahora que un nuevo estudiante de econom�a pasa 15 horas a la semana en el bar
#durante las dos primeras semanas de clase. 
#Calcule un intervalo para su porcentaje de GPA en su primer semestre 
#si contin�a pasando 15 horas a la semana en el bar

#Como es un nuevo estudiante, podemos decir que en este caso tratamos con un intervalo 
#de prediccion

#Buscamos el intervalo de prediccion con un nivel de significancia de 0.05
#dada por:
# alphahat + betahat*(15=X_i)+-t*raiz(gammmahat^2+ 1/n + ((15-xbar)^2)/SSX)
a<-alphahat+betahat*15-t*sqrt(gammahat2*(1 + 1/n + ((15-xbar)^2)/SSx))
b<-alphahat+betahat*15+t*sqrt(gammahat2*(1 + 1/n + ((15-xbar)^2)/SSx))

confianza95 <- c(a,b)
confianza95 #El puntuaje del nuevo estudiante si pasara 15 horas(promedio) a la semana
#en el bar entonces, su puntuaje GPA estar�a entre 1.7460 y 3.2153

# vii) �Cu�l ser�a la variaci�n promedio del porcentaje de GPA al aumentar en una hora 
#a la semana la estancia en el bar? De un intervalo de confianza al 90%
#Recordando nuestro modelo de regresi�n y sacando los intervalos de confianza al 90%
#con ayuda del superpoderoso R tenemos:
fit8<-lm(G ~ H, data = Datos8)

confint(fit8, level = 0.90)

# Decimos que con el 90% de confianza que al aumentar una hora a la semana, estar en el bar
#tenemos que el promedio de su puntuaje GPA disminuye entre 0.15184 y 0.070344

# viii)Suponga que un grupo de 5 estudiantes 
#acudir� al bar en promedio 10 horas a la semana durante el siguiente semestre
#�Cu�l ser� el porcentaje de GPA promedio de los cinco estudiantes para el pr�ximo semestre?. 
#Calcule un intervalo.

#Obtendremos un nuevo intervalo de predicci�n para los 5 estudiantes observados
# Sea Y_h1,Y_h2,....,Y_h5 las observaciones, y sea x_h=10
#Queremos estimar el promedio de las observaciones i.e
#Suma(Y_j)/5 con j = 1,..,5
#Calculando un intervalo de confianza al 95% tenemos y con ayuda de la tabla ANOVA que obtuvimos 
#sacamos la suma de las medias cuadradas

a1<-alphahat+betahat*10-t*sqrt(0.07*((1/6) + (1/n) + ((10-xbar)^2)/SSx))
b1<-alphahat+betahat*10+t*sqrt(0.07*((1/6) + (1/n)  + ((10-xbar)^2)/SSx))

confianza95 <- c(a1,b1)
confianza95

#El porcentaje de GPA en promedio que los 5 alumnos van a tener el siguiente semestre si acuden
# al bar en promedio 10 horas, ser� de 2.6864 a 3.3858


#ix) Describa en general cu�l es el porcentaje de GPA promedio de los estudiantes que asisten 8 horas a la semana al bar.
# Tenemos que esperanzahat:
# E(x|x_h=8)=alphahat + betahat*x_h i.e
Ehatx_h <- alphahat + betahat*(8)
Ehatx_h #Cuando los estudiantes van al bar 8 horas a la semana, en genral su puntuaje de GPA es 3.258301

#Veamos en que intervalo de confianza al 95%

a.1<-alphahat+betahat*8-t*sqrt(0.07*((1/6) + (1/n) + ((8-xbar)^2)/SSx))
b.1<-alphahat+betahat*8+t*sqrt(0.07*((1/6) + (1/n)  + ((8-xbar)^2)/SSx))

confianza95b <- c(a.1,b.1)
confianza95b #Decimos que en general, un estudiante que en promedio vaya al bar 8 horas a la semana
#Su porcentaje de GPA esta entre 2.8956 y 3.6209

# x) Este es el problema 10, pero es mucho para escribir :c
# Tenemos lo sigueinte:
alphahat ; betahat
# Teniendo en cuenta que x_h= 8 horas procederemos calcular el intervalo de confianza
#al 95%.Con base al modelo siguiente: y_i= alpha + (beta/2)*x_h + error
#As� las cosas.
a.2<-alphahat+(betahat/2)*8-t*sqrt(0.07*((1/6) + (1/n) + ((8-xbar)^2)/SSx))
b.2<-alphahat+(betahat/2)*8+t*sqrt(0.07*((1/6) + (1/n)  + ((8-xbar)^2)/SSx))

confianza95c <- c(a.2,b.2)
confianza95c

#Notemos que con un 95% de confianza los estudiantes que asistes a la cafeteriria su puntuaje
#GPA esta entre 3.34 y 4.0653, el cual es mejor que entrar en le bar.
#concluimos que si queremos tener un buen puntuaje de GPA pero no nos queremos volver locos
#por estar siempre en la escuela, entonces ir a la cafeteria es la mejor opci�n para socializar y tener un buen puntuaje de GPA









rm(list = ls(all.names = TRUE))
library(readr)
library(broom)
library(lmtest)
library(car)
library(nortest)
library(normtest)
library(latex2exp)
library(randtests)
library(ggplot2)
library(multcomp)

# Ejercicio 10
#ruta <- file.choose()
#Stereo <- read.csv(ruta)
View(Stereo)

#Ajuste del modelo
fit <- lm(SOUND ~ COST, data = Stereo )

#Verifiquemos los supuestos.

##HOMOCESASTICIDAD
# veamos el supuesto, por medio de una grafica el modelo de y vs x
plot(Stereo$COST, Stereo$SOUND)

#Vemos que conforme va aumentando la variable y (SOUND) la variable x se va estabilizando

#analicemos los residuos estandarizados
Datosfit <- augment(fit)
par(mfrow=c(1,2))
plot(Stereo$COST, sqrt(abs(Datosfit$.std.resid)),
     xlab = TeX("$x$"), ylab=TeX("$e_{s}$") )
plot(fit, 3)

#Aqui tambien vemos que cualquier corte que dieramos en el eje x, la variable no es constante
#en los residuos estandarizados, pues no hay un patrón que veamos.

#verifiquemos el cumplimiento o no del supuesto.
#H0: es que la varianza es constante vs Ha:  la varianza es una función lineal de la variable x.
#que sea plausible asumir que la varianza no depende de forma lineal de x.
#Se busca no rechazar H0.
# i.e. p-value mayor a significancia.
#Usa residuales studentilizados

lmtest::bptest(fit)
car::ncvTest(fit)
#En ambas pruebas tenemos el p-value > 0.05, entonces no rechazamos la hipotesis nula
#pero con algunas dudas, pues no el p-value esta muy cercano para ser rechazado.

#Verifiquemos que las yi > 0
Stereo$SOUND>0

#otra forma de ver si se debe de transformar la variable, es mediante BOX-COX

summary(powerTransform(fit))

#Se rechaza la hipótesis de la log transformación y también se rechaza la hipótesos de que no hay que
#transformar los datos. Por lo cual, hay evidencia estadística que indica necesaria la transformación de la
#variable y (SOUND).

#2-Linealidad
#Nuevamente grafiquemos y vs x
plot(Stereo$COST, Stereo$SOUND)
#Obviamente no es una recta

#Analizando los residuales
par(mfrow=c(1,2))
plot(Datosfit$.fitted, Datosfit$.resid,
     xlab = TeX("$\\widehat{y}$"), ylab=TeX("$e$") )
plot(fit,1)

#Observamos un patrón claro en los residuales, como un x^2, por lo que los residuales no
#se encuentran distribuidos aleatoriamente al rededor del cerapio
#se procede a realizar transformaciones en la variable x, una de
#ellas es la transformación de Box-Tidwell que busca una transformación para obtener un modelo.
#Primero se verifica que xi > 0:
Stereo$COST>0

#Dado que es verdadera esta condición, aplicamos Box-Tidwell
boxTidwell(SOUND ~ COST, data = Stereo)
#Se rechaza H0, por lo que hay evidencia estadística para decir que se puede realizar una transformación en la
#variable x y por tanto no se cumple la linealidad.

#3- Normalidad
#Como n>30 (es grande), basta con realizar la grafica de los cuantiles de los errores estandarizados vs los cuantiles
#de una normal estándar teórica.
par(mfrow=c(1,2))
plot(fit, 2)
qqPlot(Datosfit$.std.resid, dist="norm")

#El supuesto de normalidad pareciése satisfacerse ya que las observaciones están cercanas
#a la recta.
#Donde H0: es que los datos (errores) son una muestra aleatoria de la distribución normal.
#Se busca no rechazar H0
#Las pruebas vistas en clase son:
shapiro.test(Datosfit$.std.resid)
nortest::lillie.test(Datosfit$.std.resid)
normtest::jb.norm.test(Datosfit$.std.resid)
#En todos los casos no se rechaza H0.

#4- Independencia
#Se busca verificar que la covarianza de los errores es cero para errores diferentes
#o en el modelo normal que los errores son independientes.

#Graficar e_s vs el índice de los datos
plot(1:length(Datosfit$.std.resid), Datosfit$.std.resid,
     xlab = TeX("$i$"), ylab=TeX("$e_s$") )
#Se busca que no haya un patrón, pero en este caso vemos un patron parecido a x^2

#También se pueden graficar las autocorrelaciones
acf(Datosfit$.std.resid)
#Se busca que sólo la primera linea quede fuera de la banda de confianza.
#Se observa que varias líneas rebasan la banda de confianza.

#Las pruebas de hipótesis vistas en clase son:
#La hipótesis nula para la prueba de aleatoriedad es que las observaciones se pueden considerar como aleatorias
#y la hipótesis nula para la prueba Durbin-Watson es que los errores no tienen una autocorrelación de orden 1.

randtests::runs.test(Datosfit$.std.resid)
lmtest::dwtest(fit)
#Para ambas se rechaza H0, por lo que habría evidencia estadística para decir 
#que las observaciones no son independientes como era de esperar.

############ Parte 2
#Como no se satisfacen los supuestos, se busca hacer transformaciones.
#Probando la transformación de Box-Cox para la variable y:
summary(powerTransform(fit))
#Considerando la potencia redondeada se procede a transformar los datos
Stereo$lambda2=bcPower(Stereo$SOUND, 2)
#Graficando la variable transformada vs x
plot(Stereo$COST, Stereo$lambda2)
#Aún se observa que no se ha corregido la linealidad, pero hay esperanza.
#procederemos a corregir x (cost)
#Probando transformaciones de Box-Tidwell
boxTidwell(lambda2 ~ COST, data=Stereo)
#Se rechaza H0, por lo que hay evidencia estadística para considerar una transformación de tipo potencia
#en la variable x
Stereo$x1 <- Stereo$COST^-1
plot(Stereo$x1, Stereo$lambda2)
#pero vemos que resulto peor.
#Probemos con exponencial y logaritmo, ya que boxTidwell no considera estos casos

#Probando la transformación exponencial:
Stereo$eCOST = exp(Stereo$COST)
plot(Stereo$eCOST, Stereo$lambda2)
plot(Stereo$COST, Stereo$lambda2)
#No se ve nada mejor
#Probemos con logaritmo

Stereo$logCOST = log(Stereo$COST)
par(mfrow=c(1,2))
plot(Stereo$logCOST, Stereo$lambda2)
plot(Stereo$COST, Stereo$lambda2)

# Hay una ligera mejoría con esta tranformación.
#Hagamos la grafica de e_i vs y^i para visualizar mejor el supuesto.
par(mfrow=c(1,2))
fit1 <- lm(lambda2 ~ logCOST, data = Stereo)###el ganón!!!
plot(fit1,1)
fit2 <- lm(lambda2 ~ COST, data = Stereo)
plot(fit2, 1)

#Aquí se observa claramente que transformar la variable x es lo más factible para la modelación.
#Por lo que el modelo de regresión simple queda 
#yhat y Xhat transformadas por boxcox y boxtidwell
#Verificación de supuestos con las variables ya transformadas.

#1- HOMOSEDASTICIDAD
plot(fit1, 3)

lmtest::bptest(fit1)
car::ncvTest(fit1)
#En ambos casos tenemos que P-value >0.05 por lo que no rechazamos H0
summary(powerTransform(fit1))
#En este caso no se tiene que transformar la variable y
#por lo cual no hay evidencia estadística que indique que la varianza no es constante.

#2-Linealidad
plot(fit1, 1)
#Parece que se satisface la linealidad,
boxTidwell(lambda2 ~ logCOST, data = Stereo)
#En este caso no se rechaza, por lo cuál no habría evidencia que nos llevara a hacer alguna transformación,

#3-NORMALIDAD
plot(fit1,2)
#observamos una mejoría en la normalidad :D hay esperanza abuelita

#Realizando la prueba de hipotesis
Datosfit=augment(fit2)
shapiro.test(Datosfit$.std.resid)
nortest::lillie.test(Datosfit$.std.resid)
normtest::jb.norm.test(Datosfit$.std.resid)
#De las pruebas se tiene que no se rechaza H0 por lo que no hay evidencia estadística 
#para decir que las observaciones no provienen de una distribución normal.

#4-INDEPENDENCIA
#Graficar e_s vs el índice de los datos
plot(1:length(Datosfit$.std.resid),
     Datosfit$.std.resid, xlab = TeX("$i$"), ylab=TeX("$e_s$"))
acf(Datosfit$.std.resid)
randtests::runs.test(Datosfit$.std.resid)
lmtest::dwtest(fit1)
#Podemos notar que aqui no se cumple independencia, pero no sabemos de donde provienen
# los datos, entonces la unica manera de que lo cumpliera, sería que estuvieran ordenados los datos.}

# Paso 3
#Transformemos a y* a la escala original
## y* = betha0 + betha1*x*
## Donde y* = (y^(1/2)-1)/2 la cual fue obtenida en la prueba de Box-Cox
### Transformando y* a la escala original 
## yhat=(2*(betha_0hat+betha_1hat*x*+1))^(1/2)
##Asi que tenemos lo siguiente
#yhat = raiz(2(betha_0hat+betha_1hat*log(x)+1)))
plot(Stereo$COST, Stereo$SOUND)
abline(fit, col = "blue")
f1 <- function(x) {sqrt(2*(fit1$coefficients[1] + fit1$coefficients[2]*log(x)) + 1)}
curve(f1, from = min(Stereo$COST), to = max(Stereo$COST),
      col = "red", add = T)
#La recta azul es la definida por el modelo 1 y la curva roja es la definida por el modelo 2
# un mejor ajuste en el modelo 2
### PAso 4
#Recordando que el modelo ajustado es:
# Y*hat= betha_0 + betha_1*X*
#se tiene que la relación lineal que estamos asumiendo es entre la variable y*(Sound) y la
#variable x (cost), por lo cual las interpretaciones irán de acuerdo a esto.

summary(fit1)$r.squared

#con el 86% de la variabilidad de la variable SOUND transformada (y*) es explicado por el modelo que incluye a
#la variable COST (x).

##PRUEBA ANOVA
anova(fit1)

#De la prueba se tiene que se rechaza H0, por lo cual hay evidencia estadística para decir que el modelo tiene
#sentido, es decir, la variable tansformada (x*) nos está ayudando a explicar la variable volumen transformada (y*)
                                                                                                           
###Parte %
#Queremos observar la calidad del sonido para un equipo de costo 400
#PRimero veamos cual es la calidad de sonido cuando el costo es 400
Y <- sqrt(2*(fit1$coefficients[1] + fit1$coefficients[2] *log(400)) + 1)
#La calidad del sonido es  69.62513 
#con ayuda de tenemos la hipotesis: la calidad del sonido es 69.62513 para un equipo de costo 400
#Y la hipótesis que está siendo probada es:
   #H0 : ??0 + 400 ??? ??1 >= c vs Ha : ??0 + 400 ??? ??1 < c
## Con c =((69.62513)^(1/2)-1)/2
MatZ0Z1 <- matrix(c(1,400), ncol=2, nrow=1)
c <- ((69.62513^2)-1)/2
prueba <- glht(fit1, linfct=MatZ0Z1, rhs=c, alternative="less")
summary(prueba)


















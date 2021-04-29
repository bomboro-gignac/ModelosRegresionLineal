
rm(list = ls(all.names = TRUE))
library(datasets)
Datos = iris
summary(iris)
library(tidyverse)

"################## Inciso 1
filtramos los datos"
Datosfil=Datos %>% filter (Species %in% c( "setosa","versicolor"))
summary(Datosfil)
is.factor(Datosfil$Species)
levels(Datosfil$Species)
levels(Datosfil$Species) <- list(SLV="versicolor", SLS = "setosa")
levels(Datosfil$Species)
"SLV = Sepal length Versicolor
SLS = Sepal length Setosa"
summary(Datosfil)

boxplot(Sepal.Length ~ Species, data = Datosfil)
"con el boxplot notamos que la media de ambos grupos si divide los datos a la mitad y que la media del grupo SLV pareciera mayor a la media del grupo SLS "
#ajustamos el modelo de regresión lineal
fit <- lm(Sepal.Length ~ Species, data=Datosfil)
fit$coefficients

"vamos a verificar los supuestos"

"1.Homocedasticidad. H_o la varianza es cte vs H_a la varianza depende de los valores de x"
plot(fit,3)
#graficamente vemos como difieren las varianzas de ambos grupos
library(lmtest)
lmtest::bptest(fit)
library(car)
car::ncvTest(fit)
"en ambas pruebas se rechza Ho por ende hay evidencia estadística para decir que que la
varianza depende de x y no es constante."

"2.Normalidad. H_o los datos (errores) son de una muestra normal vs H_a los datos No son de una muestra normal"
library(broom)
Datosfit=augment(fit)
head(Datosfit)

plot(fit, 2)
"graficamente vemos que el supuesto de normalidad pareciése satisfacerse ya que las observaciones están cercanas
a la recta."
shapiro.test(Datosfit$.std.resid)
library(nortest)
nortest::lillie.test(Datosfit$.std.resid)
library(normtest)
normtest::jb.norm.test(Datosfit$.std.resid)

"en las pruebas no se rechaza H0, por lo cual no hay evidencia estadística que indique que los datos (errores)
 no son de una muesrtra de la distribución normal"

"3.aleatoriedad. H_o las observaciones se pueden considerar aleatorias vs H_a las observaciones NO se pueden considerar aleatorias
como estamos trabajando con una variable categorica recordamos que 
podemos asumir este supuesto desde el principio pero aún así hagamos las pruebas
 Vamos a graficar las autocorrelaciones y notamos que aún  hay un dato que se sale de la banda de confianza pero los demás si están dentro de la banda"

acf(Datosfit$.std.resid)

#hacemos las pruebas de hipótesis que vimos en clase
library(randtests)
randtests::runs.test(Datosfit$.std.resid)

"no se rechaza H0, por lo cual no hay evidencia estadística que indique que no hay aleatoriedad en los datos"


"pruebas para grupos"

"Pruebas de homocedasticidad para grupos
H_o: las varianzas de los grupos es la misma vs H_a: al menos un grupo tiene una varianza diferente"
bartlett.test(Sepal.Length ~ Species, data=Datosfil)

#prueba más robusta 
library(car)
leveneTest(Sepal.Length ~ Species, data=Datosfil)
"como lo vimos ateriormente no se pasa el supuesto de heterocedasticidad
 hay evidencia estadística para decir que la varianza no es cte y depende de las x´s en los respectivos grupos "

#2.Normalidad
shapiro.test( Datosfil$Sepal.Length[Datosfil$Species=="SLV"])
nortest::lillie.test(Datosfil$Sepal.Length[Datosfil$Species=="SLV"])
normtest::jb.norm.test(Datosfil$Sepal.Length[Datosfil$Species=="SLV"])
shapiro.test( Datosfil$Sepal.Length[Datosfil$Species=="SLS"])
nortest::lillie.test(Datosfil$Sepal.Length[Datosfil$Species=="SLS"])
normtest::jb.norm.test(Datosfil$Sepal.Length[Datosfil$Species=="SLS"])
"no se rechaza H0 en ninguna de las pruebas, por lo cual no hay evidencia estadística que indique que ,en los respectivos grupos, los datos (errores) no
son muestras de una distribución normal
se pasaron las pruebas de normalidad en ambos grupos "


"####No se cumplió el supuesto de homocedasticidad por ende procedemos a transformar la variable y####"

summary(powerTransform(fit))
Datosfil$Ytransf=bcPower(Datosfil$Sepal.Length, lambda=0)
"vemos en la salida de la funicón que es necesaria una transformación"
"transformaremos la variable y aplicándole la función logaritmo
##El modelo que se va a ajustar es Y* = B_0 +B1X, donde Y* = ln(y)##"

fitln <- lm(Ytransf  ~ Species, data=Datosfil)
"con el bloxplot notamos que con la tranformación se presenta cierta mejoría pero veamos las pruebas"

boxplot(Ytransf ~ Species, data = Datosfil)

"volvemos a ver si con la transformación ya se cumplen los supuestos de homocedasticidad y de normalidad"
"1.Homocedasticidad
vemos graficamente que las varianzas parecen coincidar un poco más"
plot(fitln,3)
#prueba para verificar homocedasticidad con la variable y transformada
lmtest::bptest(fitln)
car::ncvTest(fitln)
"no se rechaza HO ,por lo tanto no hay evidencia estadística que indique que la varianza no es constante.
se pasa el supuesto de homocedasticidad"

"2.Normalidad
#prueba para verificar normalidad "
library(broom)
Datosfitln=augment(fitln)
head(Datosfitln)
plot(fitln, 2)
"graficamente vemos que el supuesto de normalidad pareciése satisfacerse ya que las observaciones están cercanas
a la recta"
shapiro.test(Datosfitln$.std.resid)
nortest::lillie.test(Datosfitln$.std.resid)
normtest::jb.norm.test(Datosfitln$.std.resid)

"en todas las pruebas no se rechaza H0, por lo cual no hay evidencia estadística que indique que los datos (errores)
no son de una muesrtra de la distribución normal"


"3.aleatoriedad, 
recordamos que como estamos trabajando con una varaible categorica, recordamos que 
podemos asumir este supuesto desde el principio pero aún así hagamos las pruebas
Vamos a graficar las autocorrelaciones y notamos que aún  hay un dato  (posible outlier) que se sale de la banda de confianza pero los demás si están dentro de la banda"

acf(Datosfitln$.std.resid)

#hacemos las pruebas de hipótesis que vimos en clase
library(randtests)
randtests::runs.test(Datosfitln$.std.resid)
acf(Datosfitln$.std.resid)
"no se rechaza H0, por lo cual no hay evidencia estadística que indique que no hay aleatoriedad en los datos"


"pruebas para grupos con la variable Y transformada"

# H_o: las varianzas de los grupos es la misma vs H_a: al menos un grupo tiene una varianza diferente
bartlett.test(Ytransf  ~ Species, data = Datosfil)

# prueba más robusta 
leveneTest(Ytransf  ~ Species, data = Datosfil)
"no se rechaza H0, por lo cual no hay evidencia estadística que indique
que la varianza no es constante en los respectivos grupos" 

#Para normalidad
shapiro.test( Datosfil$Ytransf[Datosfil$Species=="SLV"])
nortest::lillie.test(Datosfil$Ytransf[Datosfil$Species=="SLV"])
normtest::jb.norm.test(Datosfil$Ytransf[Datosfil$Species=="SLV"])
shapiro.test( Datosfil$Ytransf[Datosfil$Species=="SLS"])
nortest::lillie.test(Datosfil$Ytransf[Datosfil$Species=="SLS"])
normtest::jb.norm.test(Datosfil$Ytransf[Datosfil$Species=="SLS"])

"no se rechaza H0 en ninguna de las pruebas, por lo cual no hay evidencia estadística que indique que ,en los respectivos grupos, los datos (errores) no
son muestras de una distribución normal
se cumple normalidad"



" Vamos a responder si En promedio el largo de los sépalos de la especie Iris versicolor es mayor al correspondiente a la  especie Iris setosa
recordemos que la prueba t Permite comparación H_0: mu_1=mu_2 vs H_a: mu_1!=mu_2
queremos rechazar Ho para poder afirmar la Ha es decir 
 H_0: mu_1<=mu_2 vs H_a: mu_1>mu_2"
levels(Datosfil$Species)
" Primer nivel está asociado con mu_1=mu_SLV
Segundo nivel está asociado con mu_2=mu_SLS"

# H_0: mu_1<=mu_2 vs H_a: mu_1>mu_2
t.test(Ytransf  ~ Species, data = Datosfil, alternative ="greater", var.equal = TRUE)
"se rechaza Ho, por ende hay evidencia estadística para asumir que la media, en escala logarítmica,del largo de los sépalos del grupo Versicolor es mayor a la del grupo Setosa"

"interpretaciones: el modelo que se ajustó fue
Y* = B_0 +B1X, donde Y* = ln(y)"

#ANOVA

anova(fitln)
"interpretación: De la prueba se tiene que se rechaza H0, por lo cual hay evidencia estadística
para decir que el modelo tiene sentido, es decir, la variable especies nos está ayudando a explicar la variable longitud transformada (Y*)"



"#################INCISO ii  #######################"

boxplot(Sepal.Width ~ Species, data = Datosfil)
"con el bloxpot podemos intuir que la media del grupo SLV es menor a la del grupo SLS
SLV = es la identificación de la especie Versicolor
SLS  = es la identificación de la especie Setosa"


#se ajusta el modelo de regresión lineal simple 
fit2 <- lm(Sepal.Width ~ Species, data=Datosfil)
fit2$coefficients


"el modelo que se ajusta es Y = B_0 +B_1X
verificación de supuestos"

"1.Homocedasticidad
H_o la varianza es cte vs H_a la varianza depende de los valores de x"
plot(fit2, 3)
#graficamnete vemos que la varinza en la anchura de ambas especies no difiere en gran medida
library(lmtest)
lmtest::bptest(fit2)
library(car)
car::ncvTest(fit2)
"no se rechaza HO ,por lo tanto no hay evidencia estadística que indique que la varianza no es constante."

"2.Normalidad.H_o los datos (errores) son de una muestra normal vs H_a los datos No son de una muestra normal"
library(broom)
Datosfit2=augment(fit2)
head(Datosfit2)

plot(fit2, 2)
"graficamente vemos que el supuesto de normalidad pareciése satisfacerse ya que las observaciones están cercanas
a la recta"

# pruebas para comprobar normalidad
shapiro.test(Datosfit2$.std.resid)
library(nortest)
nortest::lillie.test(Datosfit2$.std.resid)
library(normtest)
normtest::jb.norm.test(Datosfit2$.std.resid)
"en todas las pruebas no se rechaza H0, por lo cual no hay evidencia estadística que indique que los datos (errores)
no son de una muesrtra de la distribución normal
entonces pasa el supuesto de normalidad"

"3.aleatoriedad, como estamos trabajando con una varaible categorica, recordamos que 
podemos asumir este supuesto desde el principio pero aún así hagamos las pruebas 
Vamos a graficar las autocorrelaciones y notamos que aún  hay un dato que se sale de la banda de confianza pero los demás si están dentro de la banda
es decir hay un posible outlier o dato atipico"

acf(Datosfit2$.std.resid)

#hacemos la prueba de hipótesis que vimos en clase
library(randtests)
randtests::runs.test(Datosfit$.std.resid)
"no se rechaza H0, por lo cual no hay evidencia estadística que indique que no hay aleatoriedad en los datos "

"Vamos a responder si  En promedio el ancho de los sépalos de la especie Iris versicolor es mayor al correspondiente a la  especie Iris setosa
recordemos que la prueba t Permite comparación H_0: mu_1=mu_2 vs H_a: mu_1!=mu_2
queremos rechazar Ho para poder afirmar la Ha es decir 
H_0: mu_1<=mu_2 vs H_a: mu_1>mu_2"
levels(Datosfil$Species)
t.test( Sepal.Width ~ Species, data=Datosfil, alternative ="greater", var.equal = TRUE)
"no se rechaza HO , por ende NO hay evidencia estadística para indicar que En promedio, el ancho de los sépalos de la especie Iris versicolor es mayor al correspondiente a la  especie Iris setosa"


"Interpretaciónes: el modelo que se ajustó fue
Y = B_0 +B_1X"

#ANOVA

anova(fit2)
"interpretación: De la prueba se tiene que se rechaza H0, por lo cual hay evidencia estadística
para decir que el modelo tiene sentido, es decir, la variable especies nos está ayudando a explicar la variable anchura "


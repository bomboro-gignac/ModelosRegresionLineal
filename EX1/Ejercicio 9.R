
rm(list = ls(all.names = TRUE))
#setwd("C:/Users/eliot/Downloads")
#setwd("C:/Users/eliot/Desktop/VerificacionSupuestosA")
Datos <- read.table("infectionrisk.txt", header = T)

"El primer paso que se siguió fue leer el txt y seguido de ello se filtró la información como se pidió en el ejercicio "

library(tidyverse)
Datosfil=Datos %>% filter (Region %in% c( "1","2"))
summary(Datosfil)
levels(Datosfil$Region)
levels(Datosfil$Region) <- list(Region1="1", Region2 = "2")
levels(Datosfil$Region)
#para ver como quedaron los datos filtrados usamos el summary en el data frame "Datosfil"
summary(Datosfil)

library(latex2exp)
"graficamos la variable independiente(Stay) y la variable dependiente (InfctRisk) 
esto para darnos una idea gráfica de como estaban relacionados los datos"
plot(Datosfil$Stay, Datosfil$InfctRsk, xlab = TeX("$Stay$"), ylab=TeX("$InfctRsk$") )


########### INCISO i
# se ajusta el modelo de regresión lineal simple
fit <- lm(InfctRsk ~ Stay, data = Datosfil)
fit$coefficients
abline(fit, col = "green")
"como se ve en la gráfica, parece que los datos no se ajustan a la recta de la regresión hecha"

"Procedamos a la verificación de los supuestos"

"1.Homocedasticidad. H_o la varianza es cte vs H_a la varianza depende de los valores de x"

"graficando y vs x notamos que si bien pareciese que no hay un comportamiento lineal, los valores de las y´s parecen quedarse en una banda  a excepción de un par de datos"
plot(Datosfil$Stay, Datosfil$InfctRsk, xlab = TeX("$Stay$"), ylab=TeX("$InfctRsk$") )

"ahora utilicemos las pruebas de hipótesis para comprobar la homocedasticidad
se realizan las pruebas breush pagan y ncv test"
library(lmtest)
lmtest::bptest(fit)
library(car)
car::ncvTest(fit)
 "no se rechaza HO ,por lo tanto no hay evidencia estadística que indique que la varianza no es constante.
 por lo tanto si pasa el test de varianza cte "

"2.linealidad
#anterioremnete ya graficamos y vs x notando que los datos parecían no tener un comportamiento lineal"
#se cumple que Xi>0 para todas las X´s 
Datosfil$Stay>0

#aplicamos la función BoxTidwell
boxTidwell(InfctRsk ~ Stay, data = Datosfil)
"Notamos que se rechaza H0, por lo que hay evidencia estadística para decir que se puede realizar una transformación en la
variable x y por tanto no se cumple el supuesto de linealidad. Se debe de ir pensando en ajustar otro modelo.
No se pasa el supuesto de linealidad"

"3.Normalidad. H_o los datos (errores) son de una muestra normal vs H_a los datos No son de una muestra normal"

library(broom)
Datosfit=augment(fit)
head(Datosfit)
plot(fit, 2)

"graficamente vemos que el supuesto de normalidad pareciése satisfacerse ya que las observaciones están cercanas a la recta
a excepcion de unos cuantos de datos "
shapiro.test(Datosfit$.std.resid)
normtest::jb.norm.test(Datosfit$.std.resid)
"en todas las pruebas no se rechaza H0, por lo cual no hay evidencia estadística que indique que los datos (errores)
 no son de una muesrtra de la distribución normal
pasa el supuesto de normalidad"



"4.Independencia. H_o las observaciones se pueden considerar aleatorias vs H_a las observaciones NO se pueden considerar aleatorias"
"podemos graficar las autocorrelaciones y notamos que hay un dato que se sale de la banda notando que éste es un posible outlaier o valor atípico y se nos dio a entender desde el principio que era posible que nos encontráramos con alguno de ellos"

acf(Datosfit$.std.resid)

#hacemos la prueba randtest
library(randtests)
randtests::runs.test(Datosfit$.std.resid)

"no se rechaza H0, por lo cual no hay evidencia estadística que indique que no hay aleatoriedad en los datos 
se cumple e supuesto de aleatoriedad"



"####################INCISO ii
#Como vimos anteriormente era necesario transformar la variable X y procedemos a hacerlo"

### Se le aplica la transformación logaritmo a las X´s
x1<- log(Datosfil$Stay)
#se ajusta el modelo y = B_0 +B_1*z donde z=ln(x)
fitp <- lm(InfctRsk ~ x1, data = Datosfil)
"Procedamos a la verificación de los supuestos después de haber transformado las X´s"

"1.Homocedasticidad. H_o la varianza es cte vs H_a la varianza depende de los valores de x"

#graficando y vs x notamos que las y´s parecen quedarse en un "banda" a excpeción de un par de datos
plot(x1, Datosfil$InfctRsk, xlab = TeX("$Stay$"), ylab=TeX("$InfctRsk$") )

"ahora utilicemos las pruebas de hipótesis para comprobar la homocedasticidad
se hacen las pruebas breuch pagan test y ncv test"
library(lmtest)
lmtest::bptest(fit)
library(car)
car::ncvTest(fit)
"en ambas pruebas no se rechaza HO ,por lo tanto no hay evidencia estadística que indique que la varianza no es constante."



"#2.linealidad"
#se cumple que X1_i>0 para todas las X1´s 
x1>0
#aplicamos la afunción BT
boxTidwell(InfctRsk ~ x1, data = Datosfil)
"Notamos que ya NO se rechaza H0, por lo que no hay evidencia estadística que indique que se deba realizar una transformación en la
variable x y por tanto se cumple el supuesto de linealidad."
"entonces Ya se cumple el supuesto de linealidad, se arregló linealidad"


"3.Normalidad. H_o los datos (errores) son de una muestra normal vs H_a los datos No son de una muestra normal"
library(broom)
Datosfitp=augment(fitp)
head(Datosfitp)
plot(fitp, 2)

"graficamente vemos que el supuesto de normalidad pareciése satisfacerse ya que las observaciones están cercanas a la recta
nuevamente, a excepcion de unos cuantos datos"
shapiro.test(Datosfitp$.std.resid)
normtest::jb.norm.test(Datosfitp$.std.resid)
"En las pruebas Shapito.test y jarque bera NO rechaza H0, por lo cual no hay evidencia estadística que indique que los datos (errores)
no son de una muesrtra de la distribución normal si se cumple e supuesto de normalidad"


"4.Independencia.H_o las observaciones se pueden considerar aleatorias vs H_a las observaciones NO se pueden considerar aleatorias"
"volvemos a graficar las autocorrelaciones y notamos que aún  hay un dato que se sale de la banda
recrodemos que éste es un posible outlayer y se nos advirtió desde el principio que era posible que nos encontráramos con alguno de ellos."

acf(Datosfitp$.std.resid)

#hacemos las pruebas de hipótesis que vimos en clase
library(randtests)
randtests::runs.test(Datosfitp$.std.resid)
acf(Datosfitp$.std.resid)

"no se rechaza H0, por lo cual no hay evidencia estadística que indique que no hay aleatoriedad en los datos 
se cumple e supuesto de aleatoriedad"

"############# Resumen
el modelo que se va a ajustar es Y = B_0 + B_1(Z) donde Z=ln(X)
Para hacer la transformación se creó el vector x1 el cual se definió como
 x1 <- ln(Datosfit$Stay)

Pruebas que se hicieron
1.Homocedasticidad
Para verificar homocedasticidad se graficó y vs x y las pruebas que se hicieorn fueron
breush-pagan test y NCV-test donde las hipótesis eran
H_o la varianza es cte vs H_a la varianza depende de los valores de x

2.Linealidad
Para verificar linealidad se aplicó la función Box-tidwell al modelo y se observó que había necesidad de una tranformación
se eligió la tranformación logaritmo pues era una de las recomendaciones del ejercicio

3.Normalidad 
#para verificar este supuesto graficamente se observó la grafica 2 del modelo ajustado, es decir el plot(fitp,2)
#Se utilizaron las pruebas Shapiro-test y Norm-test donde las hipótesis eran
#H_o los datos (errores) son de una muestra normal vs H_a los datos No son de una muestra normal


4.Independencia. Para la comprobación del cumplimiento de este supuesto se graficaron las autocorrelaciones
 y se utilizó la prueba randtest donde las hipotesis eran
H_o las observaciones se pueden considerar aleatorias vs H_a las observaciones NO se pueden considerar aleatorias"



"#############     INCISO iii"

#puntos de la escala original
plot(Datosfil$Stay, Datosfil$InfctRsk, xlab = TeX("$Stay$"), ylab=TeX("$InfctRsk$"))
#recta de la regresión del modelo 1
abline(fit, col = "green")
#curva del modelo en ii
f1 <- function(x) {fitp$coefficients[1] + fitp$coefficients[2] * log(x)}
curve(f1, col = "red", add = T)

"la recta verde es la definida por el modeo i (fit)
la curva roja es la definida por el modelo en ii (fitp)"



"#########INCISO iV"
#R^2
summary(fitp)$r.squared
"interpretación: El 37% de la variabilidad de la variable riesgo de infección (y)
es explicado por el modelo que incluye a la variable Estancia transformada (Z)"

"ANOVA"

anova(fitp)
"interpretación: De la prueba se tiene que se rechaza H0, por lo cual hay evidencia estadística
para decir que el modelo tiene sentido, es decir, la variable Estancia transformada (Z) nos está ayudando a explicar la variable riesgo de infección (y)"



"############# INCISO V
Debemos de argumentar si el riesgo de infección de los pacientes cuando tienen una estancia de 10 es en general menor a 3."

library(multcomp)
MatZ0Z1 <- matrix(c(1,log(10)), ncol=2, nrow=1)
c <- 3
prueba <- glht(fitp, linfct=MatZ0Z1, rhs=c, alternative="less")
summary(prueba)
"no se rechaza Ho por lo tanto no hay evidencia estadística que indique que el riesgo de infección sea menor a 3 cuando la estancia es ln(10)"

"a excepción de los datos atípicos, vemos que podría decirse que mientras más tiempo de estancia el riesgo de infección aumenta 
pero los datos atípicos muestran un valor grande en la variable estancia con un riesgo de infección no tan alto, pues hay puntos con una estancia menor y con un riesgo de infección mayor, entonces la intuición diría
que tal vez el riesgo de infección está relacionado a más factores, no sólo at tiempo de  estancia"


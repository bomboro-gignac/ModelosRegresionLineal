rm(list = ls(all.names = TRUE))
library(latex2exp)

x=c(79, 93, 100, 105, 101, 96, 96, 109, 70, 71, 87)
y=c(133, 148, 164, 171, 165, 159, 162, 170, 127, 133, 148 )

Datos=data.frame(cbind(x,y))

"INciso i"
#se ajusta el modelo de regresi�n lineal
fit<- lm(y ~x , data = Datos)
fit$coefficients
plot(x, y, xlab = TeX("$X$"), ylab=TeX("$Y$") )

abline(fit, col = "green")
"el modelo que se ajusta es Y = 45.67 + (1.6939) X
con la recta ajustada por el modelo podemos decir que el modelo parece correcto, pues la recta parace acoplarse a los valores, pero verifiquemos los supuestos"


"1.Homocedasticidad. H_o la varianza es cte vs H_a la varianza depende de los valores de x"

"utlizando la grafica anterior de  y vs x notamos que parece ser que la variabilidad de y es constante"

#ahora utilicemos las pruebas de hip�tesis para comprobar la homocedasticidad
library(lmtest)
lmtest::bptest(fit)
library(car)
car::ncvTest(fit)
"No se rechaza H0, por lo que no hay evidencia estad�stica para decir que no se cumple el supuesto de homocedasticidad
por lo tanto si pasa el test de varinza cte "

"2.linealidad"
"ya vimos la gr�fica de  y vs x anteriormente y parece que los datos si siguen un comportamiento lineal"
#se cumple que Xi>0 para todas las X�s 
Datos$x>0

#aplicamos la funci�n Box Tidwell
boxTidwell(y ~ x, data = Datos)
"no se rechaza Ho, por ende no hay evidencia estad�stica que indique que es necesaria una transformaci�n
se cumple el supuesto de linealidad"

"3.Normalidad. H_o los datos (errores) son de una muestra normal vs H_a los datos No son de una muestra normal"

library(broom)
Datosfit=augment(fit)
head(Datosfit)
plot(fit, 2)
"graficamente vemos que el supuesto de normalidad pareci�se satisfacerse ya que las observaciones est�n cercanas
a la recta"

shapiro.test(Datosfit$.std.resid)
normtest::jb.norm.test(Datosfit$.std.resid)
"En las pruebas Shapito.test y jarque bera NO rechaza H0  por lo cual no hay evidencia estad�stica que indique que los datos (errores) no son de una muesrtra de la distribuci�n normal
si se cumple e supuesto de normalidad"

"4.Independencia, H_o las observaciones se pueden considerar aleatorias vs H_a las observaciones NO se pueden considerar aleatorias"
"podemos graficar las autocorrelaciones y notamos que hay un dato que se sale de la banda notando que �ste es un posible outlaier o valor extremo "
acf(Datosfit$.std.resid)

#hacemos las pruebas de hip�tesis que vimos en clase
library(randtests)
randtests::runs.test(Datosfit$.std.resid)
"no se rechaza Ho por ende no hay evidencia estad�stica que indique que no hay aleatoriedad en los datos"

"Interpretaci�nes: el moelo que se ajust� fue
# Y = 45.67 + (1.6939) X"
#R^2
summary(fit)$r.squared
"interpretaci�n: El 95% de la variabilidad de la variable huevo peso mayor (y)
#es explicado por el modelo que incluye a la variable huevo peso menor (x)"

"ANOVA"

anova(fit)
"interpretaci�n: De la prueba se tiene que se rechaza H0, por lo cual hay evidencia estad�stica
para decir que el modelo tiene sentido, es decir, la variable X (huevo peso menor) nos est� ayudando a explicar la variable Y (huevo peso mayor)"

"####### Inciso 2: Pruebe si la pendiente de la regresi�n difiere significativamente (estad�sticamente) de la unidad. Interprete
queremos hacer la prueba   H_O :B1 = 1 vs H_a B1=! 1"

library(multcomp)

#H_0: B1=1 vs H_a: B1=!1

MatZ0Z1=matrix(c(0,1), ncol=2, nrow=1)
c=1
prueba1=glht(fit, linfct=MatZ0Z1, rhs=c)
summary(prueba1)

"No se rechaza H_o por lo tanto no hay evidencia estad�stica para concluir que B1 difiere de 1"

"observemos que si el peso del huevo menor realmente no fuera diferente del peso del huevo mayor entonces la pendiente (B1) deber�a ser igual a 1  
entonces se podr�a discutir sobre si realmente hay diferencia entre el peso de los huevos, es decir, si hay un huevo con peso mayor y un huevo de peso menor
pues no hay evidencia que indique que B1 difiere de 1."



"####Inciso 3: Posteriormente se observa el peso de los huevos de una nueva nidada, observ�ndose un peso de 75 y  115 gramos.
#usaremos un intervalo de predicci�n con la nueva observaci�n de Xh= 75
#Ahora se busca estimar yh donde yh representa el nuevo valor respuesta."
newdata <- data.frame(x = c(75) )
HG <- predict(fit, newdata, interval = "prediction", level = 0.95)
head(HG)

"el intervalo de predicci�n para yh dado xh =75 es (124.0271,142.731) con una confianza del 95%"

"con el intervalo de predicci�n anterior, se puede concluir que con el 95 % de confianza
la nidada NO proviene de pinguinos de Macaroni ya que el yh=115 observado no pertenece al intervalo 
de predicci�n presentado lineas arriba.
por ende las sospechas de que la nidada no proviene de los pinguinos de Macaroni parece ser ciertas"


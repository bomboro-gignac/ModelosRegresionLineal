
rm(list = ls(all.names = TRUE))
gc()
#6-La tabla de abajo contiene información de la edad y el peso de embriones de pollo  

informacion <- c(6.00, 0.03, 7.00, 0.05, 8.00, 0.08, 9.00, 0.12, 10.00, 0.18, 11.00, 0.26, 12.00, 0.42, 13.00, 0.74, 14.0, 1.1, 15.0, 1.9, 16.0, 2.8) 
Base<- as.data.frame(matrix(informacion, nrow = 11, ncol = 2, byrow = T))
colnames(Base)<- c("tiempo","peso")
Base

# i) analisis descriptivo de los datos
summary(Base)
str(Base)
plot(Base)
#Vemos que el peso minimo es de 6 y el maximo es de 16, mientras que la edad es de 0.03 para el más pequeño
#y de 2.8 para el maximo, ambas variables con las que jugaremos son numericas, mientras que el plot vemos que
#que es un modelo polinomico

# ii) Compare los modelos de regresion polinomial de grado 1 a 5 y seleccione un modelo, justifique la elección

Base$x2=Base$tiempo^2; Base$x3=Base$tiempo^3; Base$x4=Base$tiempo^4; Base$x5=Base$tiempo^5
Base
mod1 <- lm(peso ~ tiempo+x2+x3+x4+x5 , data = Base)
summary(mod1)

#X<- model.matrix(fit)
#X

library(multcomp)
Matrix1 <- matrix(c(0, 0, 0, 0, 0, 1), ncol=6, nrow=1)
c <- 0
prueba <- glht(mod1, linfct=Matrix1, rhs=c, alternative="two.sided")
summary(prueba)
# vemos que el coeficiente  a la variable asociada a x5 es significativo
# supongamos que quitamos la variable x5 y veamos si ajusta mejor la variable x4
# por razones del problema.


mod2 <- lm(peso ~ tiempo+x2+x3+x4 , data = Base)
summary(mod2)

Matrix1 <- matrix(c(0, 0, 0, 0, 1), ncol=5, nrow=1)
c <- 0
prueba <- glht(mod2, linfct=Matrix1, rhs=c, alternative="two.sided")
summary(prueba)

# vemos que el coeficiente  a la variable asociada a x4 es significativo, pero no tanto como el x5
# supongamos que quitamos la variable x4 y veamos si ajusta mejor la variable x3
# por razones del problema.

mod3 <- lm(peso ~ tiempo+x2+x3 , data = Base)
summary(mod3)

Matrix1 <- matrix(c(0, 0, 0, 1), ncol=4, nrow=1)
c <- 0
prueba <- glht(mod3, linfct=Matrix1, rhs=c, alternative="two.sided")
summary(prueba)

# vemos que el coeficiente  a la variable asociada a x3 es significativo, pero no tanto como el x5
# supongamos que quitamos la variable x2 y veamos si ajusta mejor la variable x2
# por razones del problema.

mod4 <- lm(peso ~ tiempo+x2 , data = Base)
summary(mod4)

Matrix1 <- matrix(c(0, 0, 1), ncol=3, nrow=1)
c <- 0
prueba <- glht(mod4, linfct=Matrix1, rhs=c, alternative="two.sided")
summary(prueba)

#vemos que aqui es muy significativo y tenemos pocas variables, por ello decidimos que el mejor modelo
# es el 2

# consideremos el modelo de regresion polinomial 1
mod5 <- lm(peso ~ tiempo, data = Base)
summary(mod5)

Matrix1 <- matrix(c(0, 1), ncol=2, nrow=1)
c <- 0
prueba <- glht(mod5, linfct=Matrix1, rhs=c, alternative="two.sided")
summary(prueba)

#Veamos ya que todos los modelos son significativos, veamos que modelo 
# es el mejor dado el problema que nos pide
tabla1 <-broom::glance(mod1)
tabla2 <-broom::glance(mod2)
tabla3 <-broom::glance(mod3)
tabla4 <-broom::glance(mod4)
tabla5 <-broom::glance(mod5)
tablat <-rbind(tabla1,tabla2,tabla3,tabla4,tabla5)
head(tablat)

#Basados en R^2_adj y el BIC, notamos que el modelo ideal para nosotros es el
#modelo 2 es decir el modelo polinomial de grado 4, ya que su ajuste es bueno, el BIC es el más chico y
# el R^2 adj es uno de los mejores, además graficamente veremos que el ajuste a la curva
# es muy bueno.

# iii) Graficos

library(ggplot2)
ggplot(Base, aes(tiempo, peso)) +
  geom_point()+
  stat_smooth(color = "red", method='lm', formula = y~poly(x,2), se = F) +
  stat_smooth(color = "blue", method='lm',formula = y~poly(x,3), se = F) +
  stat_smooth(color = "black", method='lm', formula = y~poly(x,4), se = F) +
  stat_smooth(color = "green", method='lm', formula = y~poly(x,5), se = F)



plot(Base$tiempo, Base$peso)
f1 <- function(x) {mod2$coefficients[1] + mod2$coefficients[2] * x + mod2$coefficients[3] * x^2+ mod2$coefficients[4]*x^3+ mod2$coefficients[5]*x^4}
curve(f1, from = min(Base$tiempo), to = max(Base$tiempo), col = "red", add = T)

# iv) peso de un nuevo embrion que tiene 10 días, usando el modelo seleccionado
# tenemos que E[Y|x=10]= b0 + b1*10 +b2*10^2 +b3*10^3+b4*10^4 y damos el intervalo
newdata <- data.frame(tiempo = c(10), x2 = c(10), x3 = c(10), x4=c(10))
predict(mod2, newdata, interval = "confidence", level = 0.95)

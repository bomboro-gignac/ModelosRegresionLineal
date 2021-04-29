
rm(list = ls(all.names = TRUE))


#Problema 4.
#ruta <- file.choose()
#Ejercicio4 <- read.csv(ruta)
Ejercicio4$Y <- as.numeric(Ejercicio4$Y)
Ejercicio4$Edad<- as.factor(Ejercicio4$Edad)
Ejercicio4$Med<- as.factor(Ejercicio4$Med)

View(Ejercicio4)
tail(Ejercicio4)
# Y = Número total de anticuerpos
# Med= se aplico medicamento o no se aplico
#i)Realice un analisis descriptivo
# Veamos a primera instancia si se cumplen los supuestos para la prueba t
# Observemos si se cumple normalidad:
boxplot(Y ~ Med, data = Ejercicio4)
#Como no se cumple normalidad, dado que no se ve simetrico el boxplot, recurrimos a lo siguiente:
#Ademas vemos que hay más datos en la respuesta de los que si recibieron medicamentos y los que no.


#ii)
#Homocedasticidad
bartlett.test(Y ~ Med, data = Ejercicio4)
#Como el p-value < 0.05, entonces no rechazamos, entonces hay evidencia de que las varianzas son diferentes
# no se cumple homocedastisidad

#normalidad
shapiro.test(Ejercicio4$Y[Ejercicio4$Med=="Si"]) #H0: Los datos se distribuyen de forma normal
#Aqui obsermaos que el p-value es mayor que 0.05 por lo que no se rechaza la hipotesis nula
nortest::lillie.test(Ejercicio4$Y[Ejercicio4$Med=="Si"])
#Tampoco rechazamos la hiportes nula, debido a que el p-value es mayor de 0.05
normtest::jb.norm.test(Ejercicio4$Y[Ejercicio4$Med=="Si"])
#Tampoco rechazamos la hiportes nula, debido a que el p-value es mayor de 0.05
#Hay evidencia estadistitica que podemos asumir normalidad en las personas que si se administro medicamento
shapiro.test(Ejercicio4$Y[Ejercicio4$Med=="No"])
nortest::lillie.test(Ejercicio4$Y[Ejercicio4$Med=="No"])
normtest::jb.norm.test(Ejercicio4$Y[Ejercicio4$Med=="No"])
# En estos sucede lo mismo entonces
##Hay evidencia estadistitica que podemos asumir normalidad en las personas que no se administro medicamento


#iii)
##Usemos t.test  ya que solamente tenemos dos niveles SI y No
#permite la comparación H0: mu_1 = mu_2 vs Ha: mu_1!=mu_2
levels(Ejercicio4$Med)
# Primer nivel asociado con mu_"No"=mu_1
# Primer nivel asociado con mu_"Si"=mu_2

#H0: mu_1 >= mu_2 vs Ha: mu_1<mu_2
t.test( Y~Med, data = Ejercicio4, alternative = "less", var.equal = FALSE)
# Entonces rechazamos H0, y tenemos que mu_1<mu_2, i.e la media que se aplico el medicamento es mayor que a las que no se les aplico.
# Entonces decimos que las personas tienen mayores anticuerpos si se les aplica el medicamento
# Decimos que la compañia farmaceutica tiene razón.

# iv) Haciendo lo mismo pero con la variable categorica tenemos edad

boxplot(Y ~ Edad, data = Ejercicio4)
#observamos un poco de simetria en el boxplot, pero pra estar seguros veamos las pruebas
#de homocedasticidad

#Homocedasticidad
bartlett.test(Y ~ Edad, data = Ejercicio4)
#Como el p-value > 0.05, rechazamos, entonces hay evidencia de que las varianzas son iguales

#normalidad
shapiro.test(Ejercicio4$Y[Ejercicio4$Edad=="<60"]) #H0: Los datos se distribuyen de forma normal
#Aqui obsermaos que el p-value es mayor que 0.05 por lo que no se rechaza la hipotesis nula
nortest::lillie.test(Ejercicio4$Y[Ejercicio4$Edad=="<60"])
#Tampoco rechazamos la hiportes nula, debido a que el p-value es mayor de 0.05
normtest::jb.norm.test(Ejercicio4$Y[Ejercicio4$Edad=="<60"])
#Tampoco rechazamos la hiportes nula, debido a que el p-value es mayor de 0.05
#Hay evidencia estadistitica que podemos asumir normalidad en las personas <60 años
shapiro.test(Ejercicio4$Y[Ejercicio4$Edad==">60"])
nortest::lillie.test(Ejercicio4$Y[Ejercicio4$Edad==">60"])
normtest::jb.norm.test(Ejercicio4$Y[Ejercicio4$Edad==">60"])
# En estos sucede lo mismo entonces
##Hay evidencia estadistitica que podemos asumir normalidad en las personas que >60 años
levels(Ejercicio4$Edad)
# Primer nivel asociado con mu_<60=mu_1
# Primer nivel asociado con mu_>60=mu_2

#H0: mu_1 >= mu_2 vs Ha: mu_1<mu_2
t.test( Y~Edad, data = Ejercicio4, alternative = "less", var.equal = FALSE)

# Entonces rechazamos H0, y tenemos que mu_1<mu_2, i.e la media de menores a 60 años es mayor a la media de mayores de 60 años.
#podemos decir no se pueden atribuir las conclusiones antes analizadas, pues el medicamento se le aplico
#a personas mayores
































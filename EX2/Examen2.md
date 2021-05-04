
El examen se deberá subir al classroom antes de las 10:00 PM del 17 de
enero de 2021. Las preguntas 1, 2 y 6 tienen un valor de 1.5, mientras
que las preguntas 3, 4 y 5 valen 2 puntos cada una. La pregunta 7 es
opcional y tiene un valor de 1 punto adicional si se entrega de forma
correcta.

Favor de argumentar con detalle las respuestas.

NOTA. En caso de que se identifiquen respuestas iguales en otros
examenes, se procederá a la anulación de los examenes involucrados.

NOTA. Incluir el(los) nombre(s) completo(s) de la(s) persona(s) que
está(n) resolviendo los ejercicios.

Usar una confianza de 95% o una significancia de .05 en los casos en
donde no se requiera otro nivel de forma explícita. En el caso de
realizar alguna transformación de las variables, se tiene que hacer
explícita la variable que se usa y la interpretación en las pruebas de
hipótesis o intervalos de confianza.

### 1.

Considere el modelo de regresión siguiente:
*y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *β*<sub>2</sub>(3|*x*<sub>*i*</sub>| − 2) + *ϵ*<sub>*i*</sub>,  *i* = 1, 2, 3,

donde *x*<sub>1</sub> = 0, *x*<sub>2</sub> = 1, *x*<sub>3</sub> =  − 1.

1.  Defina la matrix diseño $\\bf{X}$ asociada a este modelo. Calcule
    $\\bf{X}^t\\bf{X}$ y su inversa.
2.  Dé las expresiones de los estimadores por mínimos cuadrados
    ordinarios de *β*<sub>0</sub>, *β*<sub>1</sub> y *β*<sub>2</sub>:
    *β̂*<sub>0</sub>, *β̂*<sub>1</sub> y *β̂*<sub>2</sub>. Deberán ser una
    expresión en términos de
    *y*<sub>1</sub>, *y*<sub>2</sub>, *y*<sub>3</sub> donde los
    *x*<sub>*i*</sub> ya deben ser sustituidos por los valores antes
    dados.
3.  Muestre que los estimadores por mínimos cuadrados ordinarios del
    modelo reducido cuando se supone *β*<sub>2</sub> = 0 no se alteran,
    es decir, que *β̂*<sub>0</sub><sup>\*</sup> = *β̂*<sub>0</sub> y
    *β̂*<sub>1</sub><sup>\*</sup> = *β̂*<sub>1</sub>, donde
    *β̂*<sub>0</sub><sup>\*</sup> y *β̂*<sub>1</sub><sup>\*</sup> son los
    estimadores por mínimos cuadrados del modelo
    *y*<sub>*i*</sub> = *β*<sub>0</sub><sup>\*</sup> + *β*<sub>1</sub><sup>\*</sup>*x*<sub>*i*</sub> + *ϵ*<sub>*i*</sub><sup>\*</sup>,  *i* = 1, 2, 3.

### 2.

Considere el modelo de regresión

*y* = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>1</sub> + ... + *β*<sub>*p*</sub>*x*<sub>*p*</sub> + *ϵ*
Considere los estimadores obtenidos por mínimos cuadrados escritos en
forma matricial
$$ \\widehat{\\bf{\\beta}} = {\\bf (X^{t}X)^{-1} X^{t}} \\bf{y}.$$

-   Encuentre $V(\\widehat{\\bf{y}})$, donde
    $\\widehat{\\bf{y}}=\\bf{X}\\widehat{\\bf{\\beta}}$. Deberá quedar
    en función de la matriz proyección $\\bf H$.
-   Encuentre $\\sum\_{i=1}^n V(\\widehat{y}\_i)$. Notar que esto es
    equivalente a la traza de la matriz $V(\\widehat{\\bf{y}})$.

### 3.

La Compañía Kenton Food desea comparar 4 diferentes diseños de empaque
de un nuevo cereal. Veinte tiendas, con aproximadamente igual volumen de
ventas y perfil de clientes, fueron seleccionadas como unidades
experimentales. A cada una de las tiendas se le asignó uno de los
empaques de forma aleatoria, de manera que cada empaque fuera asignado a
5 tiendas distintas. Las ventas, en número de casos, fueron observadas
durante un período de estudio de 2 semanas:

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;">
ventas
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
empaque
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
</tr>
</tbody>
</table>

Un incendio ocurrió en una de las tiendas durante el período de estudio
y dado que esto cambia las condiciones de venta con respecto a las otras
tiendas se decidió eliminar la medición de esa tienda. El número de
ventas de esa tienda se excluye de la tabla anterior.

Asuma que se cumplen todos los supuestos de un problema tipo ANOVA.

1.  Presente un gráfico para describir los datos.
2.  Ajuste el modelo de regresión lineal múltiple adecuado. Indique de
    acuerdo a los parámetros del modelo la expresión del número de
    ventas promedio por cada tipo de empaque y dé estimaciones
    puntuales.
3.  Calcule intervalos de confianza que de forma simultánea tengan una
    confianza de 95% para el número de ventas promedio por cada tipo de
    empaque.
4.  Escriba las hipótesis que se contrastan con la tabla ANOVA, calcule
    ésta e interprete. Use *α* = .05.
5.  ¿Se puede considerar que el diseño del empaque afecta las ventas
    promedio? Use *α* = .05. Argumente.
6.  Realicé la prueba de hipótesis simultánea asociada a la igualdad de
    las ventas promedio entre todos los posibles pares de diferentes
    empaques. Use *α* = .05. Interprete los resultados.
7.  Suponga que los ejecutivos de la empresa tienen la sospecha de que
    el diseño de empaque 1 es el que aumenta las ventas en comparación
    con el resto de empaques. Realice una prueba de hipótesis para
    argumentar en favor o en contra de esta hipótesis de acuerdo con los
    datos observados. Use *α* = .05

### 4.

Una institución de investigación realiza un estudio para analizar los
efectos de un nuevo tratamiento para controlar los niveles altos de
ansiedad. Para eso consideran un puntaje (a mayor valor mayores niveles
de ansiedad) y definen un conjunto experimental con 120 individuos que
en ese puntaje presentan valores similares al inicio del estudio, 60 son
hombres y 60 mujeres. En el mercado se sabe que hay otro tratamiento que
se usa comúnmente para este fin, de manera que de forma aleatoria han
divido a los 120 en tres grupos: 40 a los que no se aplicó ningún
tratamiento (control), 40 a los que se aplicó el tratamiento actual
(Trat1) y 40 a los que se aplicó el nuevo tratamiento (Trat2); 20
hombres y 20 mujeres en cada grupo. Los datos se presentan en el archivo
*Ex4B.csv*.

Los investigadores sospechan que para el nuevo tratamiento podría
existir un efecto diferenciado de acuerdo con el sexo, por lo que
consideran conveniente incluir esta variable en el análisis.

(para este ejercicio no se requiere verificar supuestos del modelo,
asuma que se cumplen)

1.  Realice un análisis descriptivo de los datos. Observe que hay dos
    variables categóricas, incluya entonces un boxplot para cada posible
    combinación de niveles que se pueden observar en esas dos variables
    categóricas (*boxplot(Puntaje~Trat+Sexo, …)*). Comente lo que
    observe.
2.  Considere un modelo de regresión donde en las covariables se
    incluyan las dos variables categóricas de forma individual y también
    su interacción. De acuerdo con los parámetros de ese modelo, ajuste
    la regresión y dé la expresión del puntaje promedio para cada valor
    en las variables categóricas:
    *E*(*p**u**n**t**a**j**e*|*T**r**a**t* = *k*, *S**e**x**o* = *l*),
    con *k* ∈ {*C**o**n**t**r**o**l*, *T**r**a**t*1, *T**r**a**t*2} y
    *l* ∈ {*H**o**m**b**r**e*, *M**u**j**e**r*}; así como estimaciones
    puntuales.
3.  Calcule intervalos de confianza que de forma simultánea tengan una
    confianza de 95% para el puntaje promedio para cada valor en las
    variables categóricas.
4.  Escriba las hipótesis que se contrastan con la tabla ANOVA, calcule
    ésta e interprete. Use *α* = .05.
5.  ¿Se puede considerar que el sexo tiene un efecto en el puntaje, es
    decir, al menos para un tratamiento existe un efecto diferenciado en
    el puntaje derivado del sexo de los individos? Use una prueba F con
    *α* = .05. Argumente. Aquí:
    *H*<sub>0</sub> : *E*(*p**u**n**t**a**j**e*|*T**r**a**t* = *k*, *S**e**x**o* = *H**o**m**b**r**e*) = *E*(*p**u**n**t**a**j**e*|*T**r**a**t* = *k*, *S**e**x**o* = *M**u**j**e**r*)∀*k* ∈ {*C**o**n**t**r**o**l*, *T**r**a**t*1, *T**r**a**t*2}.
    En caso de no rechazar *H*<sub>0</sub> considere el modelo reducido
    eliminando la variable Sexo; pero si se rechaza *H*<sub>0</sub>,
    considere una prueba simultánea que ayude a identificar para qué
    tratamiento se puede considerar que el sexo tiene un efecto, con los
    resultados de esa prueba reduza el modelo si es posible.
6.  En caso de que en el inciso anterior se haya reducido el modelo,
    ajuste de nuevo la regresión y dé la expresión del puntaje promedio
    para cada valor en las variables categóricas:
    *E*(*p**u**n**t**a**j**e*|*T**r**a**t* = *k*, *S**e**x**o* = *l*),
    con *k* ∈ {*C**o**n**t**r**o**l*, *T**r**a**t*1, *T**r**a**t*2} y
    *l* ∈ {*H**o**m**b**r**e*, *M**u**j**e**r*}; así como estimaciones
    puntuales.
7.  Realice una prueba de hipótesis para argumentar en favor o en contra
    de la hipótesis: *el nuevo tratamiento tiene el mejor desempeño*.
    Use *α* = .05
8.  Realice una prueba de hipótesis para argumentar en favor o en contra
    de la hipótesis: *el tratamiento actual tiene el mejor desempeño*.
    Use *α* = .05

### 5.

Suponga que una empresa farmacéutica está ofreciendo al gobierno un
nuevo medicamento para tratar a pacientes con la enfermedad Covid-19. El
costo del medicamento es considerable y para tomar una buena decisión se
han acercado a usted para analizar los datos que ha compartido la
empresa farmacéutica. El archivo Ex5.csv contiene la información:
*A**n**t* es el número total de anticuerpos, *T**r**a**t* es una
variable con dos niveles dependiendo si se aplicó o no el nuevo
medicamento. Se sabe que tener mayores anticuerpos evita que se
desarrolle una versión grave de la enfermedad y la empresa afirma que
eso se logra al aplicar el medicamento, pues los pacientes que
recibieron el medicamento tienen más anticuerpos que los que sólo
recibieron placebo. También se sabe que la generación de anticuerpos es
diferente dependiendo la edad de los individuos y se sospecha que eso
también podría afectar la efectividad del medicamento, así que al
diseñar el experimento se seleccionaron al azar 100 personas de 300 que
presentaban síntomas leves al iniciar el cuadro de la enfermedad a los
que se les administró el medicamento, al resto se les dió sólo
seguimiento. En todos los pacientes se capturó la edad y se procuró
tener pacientes en el rango entre 16 y 60 años. No se sospecha de otro
aspecto que pudiera modificar la evaluación del medicamento.

1.  Realice un análisis descriptivo de los datos
2.  Ajuste un modelo adecuado para evaluar la efectividad del
    medicamento ajustando por la edad de los pacientes.
3.  Verifique el cumplimiento de los supuestos de este modelo
    (linealidad, normalidad, homocedasticidad).
4.  ¿Se puede decir que la edad afecta de la misma forma la efectividad
    del medicamento en el grupo control y en el grupo que recibe el
    medicamento? Realice una prueba de hipótesis apropiada e interprete.
5.  Comente sobre el ajuste del modelo incluyendo la interpretación de
    cada uno de los coeficientes.
6.  Argumente en contra o a favor de la afirmación: “El medicamente
    funciona aumentando el número de anticuerpos para todos los
    pacientes entre 16 y 60 años”. Se puede apoyar de pruebas de
    hipótesis o intervalos de confianza simultáneos.

### 6.

La tabla de abajo contiene la información de la edad y el peso de
embriones de pollo. Se tiene la sospecha de que un modelo de regresión
polinomial del peso *Y* en la edad *X* se ajusta a los datos, pero se
desea ajustar un modelo con **el menor grado** posible.

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;">
tiempo
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
14.0
</td>
<td style="text-align:right;">
15.0
</td>
<td style="text-align:right;">
16.0
</td>
</tr>
<tr>
<td style="text-align:left;">
peso
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.42
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
1.1
</td>
<td style="text-align:right;">
1.9
</td>
<td style="text-align:right;">
2.8
</td>
</tr>
</tbody>
</table>

1.  Realice una análisis descriptivo de los datos.
2.  Compare los modelos de regresión polinomial de grado 1 a 5 y
    seleccione un modelo, justifique la selección.
3.  Presente la gráfica de los datos y la curva o recta ajustada.
4.  Con el modelo seleccionado dé un intervalo para el peso de un nuevo
    embrión de pollo que tiene 10 días.

### 7.

Considere los datos del archivo *Ex7.csv*.

1.  Considere un modelo de regresión lineal múltiple con las covariables
    *X*<sub>1</sub> a *X*<sub>6</sub> sin ninguna interacción. La
    variable dependiente es *Y*. Verifique los supuestos del modelo. En
    caso de que alguno no se cumpla, realice transformaciones
    convenientes hasta que obtenga un modelo que parezca cumplir con los
    supuestos del modelo de regresión lineal múltiple.
2.  Con las variables transformadas en el inciso anterior, realice una
    selección de variables. Justifique su respuesta.
3.  Sólo para la variable X1, dé las expresiones del modelo que se está
    ajustando para cada uno de los niveles considerando el valores fijos
    para el resto de variables.

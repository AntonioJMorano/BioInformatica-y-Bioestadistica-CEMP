# https://fhernanb.github.io/Manual-de-R/ph.html
#CONSTRASTES DE HIPOTESIS

#Hay muchos diferentes tipos de contrastes, los cuales son:

# 1.- Prueba de hipótesis para μ de una población normal
#EJEMPLO
# Para verificar si el proceso de llenado de bolsas de café 
# con 500 gramos está operando correctamente se toman aleatoriamente 
# muestras de tamaño diez cada cuatro horas. Una muestra de bolsas 
# está compuesta por las siguientes observaciones: 
# 502, 501, 497, 491, 496, 501, 502, 500, 489, 490.
# ¿Está el proceso llenando bolsas conforme lo dice la envoltura? Use un nivel de significancia del 5%.

contenido <- c(510, 492, 494, 498, 492, 496, 502, 491, 507, 496)
#Sabemos que sigue distribucion normal, si no hacer test de normalidad

t.test(contenido, alternative='two.sided', conf.level=0.95, mu=500)
#Como p-value = 0.3155 > 0.05, no rechazamos hipotesis nula. por lo que no podemos
#afirmar que el proceso no esta cumpliendo con lo dicho en la envoltura.





#2.-Prueba de hipótesis para μ con muestras grandes
#EJEMPLO
# Se afirma que los automóviles recorren en promedio más de 20000 kilómetros por año pero usted cree 
# que el promedio es en realidad menor. Para probar tal afirmación se pide a una muestra de 
# 100 propietarios de automóviles seleccionada de manera aleatoria que lleven un registro de 
# los kilómetros que recorren.
# 
# ¿Estaría usted de acuerdo con la afirmación si la muestra aleatoria indicara un promedio de 19500
# kilómetros y una desviación estándar de 3900 kilómetros? Utilice un valor P en su conclusión 
# y use una significancia del 3%.

xbarra <- 19500  # Datos del problema
desvia <- 3900   # Datos del problema
n <- 100         # Datos del problema
mu <- 20000      # Media de referencia

est <- (xbarra - mu) / (desvia / sqrt(n))
pnorm(est)
#Como nos da 0.09 > 0.03 no tenemos evidencia suficiente para rechazar la hipotesis nula






#3.-Prueba de hipótesis para la proporción p
    
#3.1.-Prueba de Wald
#EJEMPLO
# Un fabricante de un quitamanchas afirma que su producto quita 90% de todas las manchas. 
# Para poner a prueba esta afirmación se toman 200 camisetas manchadas de las cuales a solo 
# 174 les desapareció la mancha.Pruebe la afirmación del fabricante a un nivel α=0.05.

z <- (174/200 - 0.90) / sqrt(0.90 * (1 - 0.90) / 200)

pnorm(q=z, lower.tail=TRUE)
#Como el p-value= 0.0786, no tenemos evidencia suficiente para rechazar la hipotesis nula.

#3.2.-Prueba χ2 de Pearson
#EJEMPLO ANTERIOR

resultado = prop.test(x=174, n=200, p=0.9, alternative='less',
          conf.level=0.95, correct=FALSE)

if (abs(resultado$p.value) > 0.05) {
  cat("No hay suficiente evidencia para rechazar la hipótesis nula.\n")
} else {
  cat("Rechazamos la hipótesis nula.\n")
}

#Nos sale el mismo p-value, con mismo razonamiento.

#3.3.-Prueba binomial exacta
#EJEMPLO
# Un asadero de pollos asegura que 90% de sus órdenes se entregan en menos de 10 minutos. 
# En una muestra de 20 órdenes, 17 se entregaron dentro de ese lapso. 
# ¿Puede concluirse en el nivel de significancia 0.05, que menos de 90% de las órdenes se entregan en menos de 10 minutos?

binom.test(x=17, n=20, p=0.9, alternative="less")
#Como el p-valor=0.3231 > 0.05 no tenemos evidencia suficiente para rechazar la
#hipotesis nula





#4.-Prueba de hipótesis para la varianza σ2 de una población normal
#EJEMPLO DE CHATGPT
set.seed(123)
datos <- rnorm(100, mean = 0, sd = 1)
# Varianza hipotética
varianza_hipotetica <- 1
# Tamaño de la muestra
n <- length(datos)
# Estadístico de prueba
estadistico_prueba <- (n - 1) * var(datos) / varianza_hipotetica
# Grados de libertad
grados_libertad <- n - 1
# Valor crítico (nivel de significancia del 5% para una prueba bilateral)
valor_critico <- qchisq(0.975, df = grados_libertad)
# Prueba de hipótesis
p_valor <- 2 * pchisq(estadistico_prueba, df = grados_libertad)
# Imprimir resultados
cat("Estadístico de prueba:", estadistico_prueba, "\n")
cat("Valor crítico:", valor_critico, "\n")
cat("P-valor:", p_valor, "\n")
# Tomar decisión
if (abs(estadistico_prueba) > valor_critico) {
  cat("Rechazamos la hipótesis nula.\n")
} else {
  cat("No hay suficiente evidencia para rechazar la hipótesis nula.\n")
}





#5.- Prueba de hipótesis para el cociente de varianzas σ21/σ22
# Para verificar si el proceso de llenado de bolsas de café está operando con la variabilidad 
# permitida se toman aleatoriamente muestras de tamaño diez cada cuatro horas. 
# Una muestra de bolsas está compuesta por las siguientes observaciones: 
#   502, 501, 497, 491, 496, 501, 502, 500, 489, 490. 
# El proceso de llenado está bajo control si presenta un varianza de 40 o menos. 
# ¿Está el proceso llenando bolsas conforme lo dice la envoltura? Use un nivel de significancia del 5%

contenido <- c(510, 492, 494, 498, 492, 496, 502, 491, 507, 496)
stats::var.test(contenido)

#EJEMPLO 1
# Se realiza un estudio para comparar dos tratamientos que se aplicarán a frijoles crudos 
# con el objetivo de reducir el tiempo de cocción. El tratamiento T1 es a base de 
# bicarbonato de sodio, el T2 es a base de cloruro de sodio o sal común. 
# La variable respuesta es el tiempo de cocción en minutos. Los datos se muestran abajo. 
# ¿Son las varianzas de los tiempos iguales o diferentes? Usar α=0.05.
# 
# T1: 76, 85, 74, 78, 82, 75, 82.
# T2: 57, 67, 55, 64, 61, 63, 63.

T1 <- c(76, 85, 74,78, 82, 75, 82) 
T2 <- c(57, 67, 55, 64, 61, 63, 63)

q1 <- qqnorm(T1, plot.it=FALSE)
q2 <- qqnorm(T2, plot.it=FALSE)
plot(range(q1$x, q2$x), range(q1$y, q2$y), type="n", las=1,
     xlab='Theoretical Quantiles', ylab='Sample Quantiles')
points(q1, pch=19)
points(q2, col="red", pch=19)
qqline(T1, lty='dashed')
qqline(T2, col="red", lty="dashed")
legend('topleft', legend=c('T1', 'T2'), bty='n',
       col=c('black', 'red'), pch=19)


stats::var.test(x=T1 , y=T2, null.value=1,alternative="two.sided",conf.level=0.95)
#Como p-value=0.9897 > 5, podemos afirmar que las varianzas con similares


#EJEMPLO 2
# El arsénico en agua potable es un posible riesgo para la salud. 
# Un artículo reciente reportó concentraciones de arsénico en agua potable 
# en partes por billón (ppb) para diez comunidades urbanas y diez 
# comunidades rurales. Los datos son los siguientes:
#   
# Urbana: 3, 7, 25, 10, 15, 6, 12, 25, 15, 7
# Rural: 48, 44, 40, 38, 33, 21, 20, 12, 1, 18

urb <- c(3, 7, 25, 10, 15, 6, 12, 25, 15, 7)
rur <- c(48, 44, 40, 38, 33, 21, 20, 12, 1, 18)

q1 <- qqnorm(urb, plot.it=FALSE)
q2 <- qqnorm(rur, plot.it=FALSE)
plot(range(q1$x, q2$x), range(q1$y, q2$y), type="n", las=1,
     xlab='Theoretical Quantiles', ylab='Sample Quantiles')
points(q1, pch=19)
points(q2, col="green", pch=19)
qqline(urb, lty='dashed')
qqline(rur, col="green", lty="dashed")
legend('topleft', legend=c('Urbana', 'Rural'), bty='n',
       col=c('purple', 'green'), pch=19)
#Observando el grafico resultante podemos decir qe sigue una distribucion normal

stats::var.test(x=urb,y=rur, null.value=1,alternative="two.sided",conf.level=0.95)
#Como p-value = 0.04936 < 0.05 , tenemos evidencia para rechazar la hipotesis nula

#6.- Prueba de hipótesis para la diferencia de medias μ1−μ2 con varianzas iguales
#Mismo  EJEMPLO1 apartado anterior pero
# Retomando el ejemplo de los fríjoles, ¿existen diferencias entre los tiempos de cocción 
# de los fríjoles con T1 y T2?   Usar un nivel de significancia del 5%
T1 <- c(76, 85, 74,78, 82, 75, 82) 
T2 <- c(57, 67, 55, 64, 61, 63, 63)

datos <- data.frame(tiempo=c(T1, T2), trat=rep(1:2, each=7))
boxplot(tiempo ~ trat, data=datos, las=1,
        xlab='Tratamiento', ylab='Tiempo (min)')

t.test(x=T1, y=T2, alternative="two.sided", mu=0, 
       paired=FALSE, var.equal=TRUE, conf.level=0.97)
#Nos da un p-value de 4,3 * 10^-6 , mucho menor que alpha. Podemos rechazar 
#hipotesis nula y afirmar que existe diferencia en los tiempos.






#7.-Prueba de hipótesis para la diferencia de medias μ1−μ2 con varianzas diferentes
#Mismo EJEMPLO2 que punto 5
# Retomando el ejemplo de la concentración de arsénico en el agua, ¿existen diferencias entre 
# las concentraciones de arsénico de la zona urbana y rural? Usar un nivel de significancia del 5%.

urb <- c(3, 7, 25, 10, 15, 6, 12, 25, 15, 7)
rur <- c(48, 44, 40, 38, 33, 21, 20, 12, 1, 18)

datos <- data.frame(Concentracion=c(urb, rur),
                    Zona=rep(c('Urbana', 'Rural'), each=10))
boxplot(Concentracion ~ Zona, data=datos, las=1,
        xlab='Zona', ylab='Concentración arsénico (ppb)')

t.test(x=urb, y=rur, alternative="two.sided", mu=0, 
       paired=FALSE, var.equal=FALSE, conf.level=0.95)
#p-value = 0.01, tenemos evidencia significativa para rechazar H0





#8.-Prueba de hipótesis para la diferencia de proporciones p1−p2
# Se quiere determinar si un cambio en el método de fabricación de una piezas ha sido efectivo o no. 
# Para esta comparación se tomaron 2 muestras, una antes y otra después del cambio en el proceso y 
# los resultados obtenidos son los siguientes.
# Num piezas 	Antes 	Después
# Defectuosas 	75 	80
# Analizadas 	1500 	2000

prop.test(x=c(75, 80), n=c(1500, 2000),
          alternative='greater', conf.level=0.90)
#El pvalor es de 0.09, por lo que no hya envidencia sufuciente para rechazar H0






#9.-Prueba de hipótesis para la diferencia de medias pareadas
# Diez individuos participaron de programa para perder peso corporal por medio de una dieta. 
# Los voluntarios fueron pesados antes y después de haber participado del programa y los datos en 
# libras aparecen abajo. ¿Hay evidencia que soporte la afirmación de la dieta disminuye el peso medio
# de los participantes? Usar nivel de significancia del 5%.
# Sujeto 	001 002 003 004 005 006 007 008 009 010
# Antes 	195 213 247 201 187 210 215 246 294 310
# Después 	187 195 221 190 175 197 199 221 278 285


antes <- c(195, 213, 247, 201, 187, 210, 215, 246, 294, 310)
despu <- c(187, 195, 221, 190, 175, 197, 199, 221, 278, 285)

dif <- antes - despu
qqnorm(dif, pch=19, main='')
qqline(dif)

t.test(x=antes, y=despu, alternative="greater", mu=0, 
       paired=TRUE, conf.level=0.95) 
#pValue= 7,6*10^-6, tenemos evidencia suficiente para rechazar H0

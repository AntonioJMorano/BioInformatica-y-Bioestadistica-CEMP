#Ajustes de regresion lineal 

#https://rpubs.com/joser/RegresionSimple

grasas <- read.table('http://verso.mat.uam.es/~joser.berrendero/datos/EdadPesoGrasas.txt', header = TRUE)
names(grasas)

pairs(grasas)

#Matriz de coeficientes de correlacion
cor(grasas) 

# y ~ x, donde y es variable dependiente, x regresora
regresion <- lm(grasas ~ edad, data = grasas)
summary(regresion)
#Viendo el summary, en estimate tenemos los valores, quedando la curva
#en la siguiente Y = 102,575 + 5,321 * X


plot(grasas$edad, grasas$grasas, xlab='Edad', ylab='Grasas')
abline(regresion)


#Para hacer calculo con predicciones, basta con crear un nuevo fichero

nuevasEdades <- data.frame(edad = seq(30, 50))
predict(regresion, nuevasEdades)

#Obtiene los residuos estandarizados
residuos <- rstandard(regresion) 
#Obtiene valores ajustados de y
valoresAjustados <- fitted(regresion)

plot(valoresAjustados,residuos)


qqnorm(residuos)
qqline(residuos)
#Si los puntos estan alineados en la diagonal, podemos pensar que es 
#probable estar ante una distribucion normal.




#EJERCICIOS
#Ajusta el modelo que explica la cantidad de grasas en función del peso.

#1.-Calcula y representa gráficamente la recta de regresión, junto con la correspondiente nube de puntos.
#Primero creamos la regresion entre ambas vairbales
regresion2 <- lm(grasas$grasas ~ grasas$peso)
summary(regresion2)
#La ecuacion de la curva quedaria: Y = 199'298 + 1,622 * X

plot(grasas$peso , grasas$grasas , main="Regresion lineal", xlab="Peso", ylab = "Grasas")
abline(regresion2)

#2.- ¿Cuánto vale el coeficiente de correlación al cuadrado en este caso?
#Lo vamos a buscar en regresion2
coeficiente <- summary(regresion2)$r.squared
print(paste("Coeficiente de correlacion = " , coeficiente))

#3.- ¿Cuánto valen los estimadores de todos los parámetros del modelo?
#Para sacar los estimadores usamos la funcion coef()
estimadores <- coef(regresion2)
print(paste("Los estimadores son: " , estimadores))


#4.- Contrasta la hipótesis de que la pendiente de la recta es cero a nivel 0.05.
contraste <- summary(regresion2)$Coefficients["Peso", c("Estimate", "Pr(>|t|)")]
print(paste("Contraste de hipótesis sobre la pendiente: ",contraste))



#5.- Calula un intervalo de confianza para la pendiente de la recta de nivel 90%
iConfianza <- confint(regresion2, level = 0.9)["Peso", ]
print("Intervalo de confianza para la pendiente (90%):")
print(iConfianza)


#6.- Calcula y representa los intervalos de confianza al 95% de la cantidad de grasas media para los individuos entre 30 y 90 kg.
# Intervalos de confianza al 95% para la cantidad de grasas media
interPredicc <- predict(regresion2, newdata = data.frame(Peso = seq(30, 54, by = 1)), interval = "confidence", level = 0.95)
interPredicc

# Graficar intervalos de confianza
matplot(seq(30, 54, by = 1), interPredicc[, c("lwr", "upr")], type = "l", col = "blue", lty = 1, xlab = "Peso", ylab = "Grasas", main = "Intervalos de Confianza")


#7.- Lleva a cabo el diagnóstico del modelo.
par(mfrow = c(2, 2))  #Crea 4 plots (residual vs fitted , qq residuals , scale-location , residual vs leverage)
plot(regresion2)



rm(list=ls())

#PARTE 2 
# Supongamos que la variable regresora toma los valores x=1,2,…,10. El siguiente código de R genera una muestra que sigue el modelo de regresión lineal 
# (cuando β0=0, β1=1 y σ=0.3), extrae el valor de la pendiente estimada β^1 y resume los principales resultados.
# Anota el valor del estimador de la pendiente que has obtenido y su error típico. ¿Has obtenido una buena estimación?
# Repite el procedimiento anterior 1000 veces y haz un estudio descriptivo de las 1000 pendientes estimadas resultantes. 
# Estudia si se corresponden los resultados de la simulación con las propiedades teóricas del estimador de la pendiente. 
# El error típico obtenido en el problema anterior, ¿refleja adecuadamente la variabilidad del estimador observada en la simulación?
# Repite la simulación, pero generando los datos de manera que las variables de error ϵi proceden de una distribución 
# t de Student con 5 grados de libertad en lugar de una distribución normal. Describe las propiedades de la distribución del estimador 
# que se deducen de la simulación.

# Configuración de la simulación
set.seed(123)  # Para reproducibilidad
n_simulaciones <- 1000
n_obs <- 10
beta_0 <- 0
beta_1 <- 1
sigma <- 0.3
grados_libertad_t <- 5

# Inicializar vectores para almacenar resultados
pendientesEst <- numeric(n_simulaciones)
erroresTipicos <- numeric(n_simulaciones)

# Simulación
for (i in 1:n_simulaciones) {
  # Generar datos de acuerdo con el modelo de regresión lineal
  x <- 1:n_obs
  epsilon <- rnorm(n_obs, mean = 0, sd = sigma)  # Cambiar a rt() para distribución t de Student
  y <- beta_0 + beta_1 * x + epsilon
  
  # Ajustar el modelo de regresión lineal
  modeloReg <- lm(y ~ x)
  
  # Almacenar resultados
  pendientesEst[i] <- coef(modeloReg)[2]  # Pendiente estimada
  erroresTipicos[i] <- summary(modeloReg)$Coefficients[2, "Std. Error"]  # Error típico
}

# Resultados individuales
print("Resultados individuales:")
print(data.frame(pendienteEst = pendientesEst, errorTipico = erroresTipicos))

# Resumen estadístico
print("Resumen estadístico de pendientes estimadas:")
summary(pendientesEst)

# Gráfico de densidad de las pendientes estimadas
hist(pendientesEst, main = "Densidad de Pendientes Estimadas", xlab = "Pendiente Estimada", col = "lightblue", border = "black")

# Comparación con propiedades teóricas
teorico_error_tipico <- sd(pendientesEst)  # Desviación estándar de las pendientes estimadas
print(paste("Error típico teórico:", teorico_error_tipico))

















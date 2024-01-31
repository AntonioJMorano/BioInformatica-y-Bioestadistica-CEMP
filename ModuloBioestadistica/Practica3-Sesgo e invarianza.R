# Ejemplos chatgpt

#SESGO 
#El sesgo de un estimador mide cuán lejos está el valor 
#esperado del estimador del verdadero valor del parámetro.
#Calculas el sesgo suponiendo que estamos estimando la media de una poblacion
# y la media de la muestra, esto lo hacemos restando ambas medias

# Crear una población
set.seed(123)
poblacion <- rnorm(1000, mean = 5, sd = 2)

# Tomar una muestra
muestra <- sample(poblacion, 100)

# Calcular la media de la población y la media de la muestra
media_poblacion <- mean(poblacion)
media_muestra <- mean(muestra)

# Calcular sesgo
sesgo <- media_muestra - media_poblacion
print(paste("Sesgo:", sesgo))

# VARIANZA
# La varianza de un estimador mide cuánto varía el estimador alrededor de su valor esperado.

#Podemos calcular la varianza del ejercicio anterior rapidamenete con el comando 'var' : 
varianza_muestra <- var(muestra)
print(paste("Varianza:", varianza_muestra))

# INTERVALO DE CONFIANZA:   
# Un intervalo de confianza proporciona un rango de valores en el cual es probable que se encuentre
# el verdadero parámetro con un cierto nivel de confianza.

# Calcular intervalo de confianza del 95% para la media del ejercicio de sesgo

intervalo_confianza <- t.test(muestra)$conf.int  #Por defecto el nivel de confianza es 95%
intervalo_confianza2 <- t.test(muestra, conf.level = 0.9)$conf.int
alpha <- t.test(muestra)$parameter

# La funcion t.test() , crea un objeto htest con los atributos:     
# statistic: El valor estadístico del test t.
# parameter: El número de grados de libertad del test t.
# p.value: El valor p del test t.
# conf.int: El intervalo de confianza para la media (si es aplicable). 
# estimate: La estimación de la media o diferencia de medias, dependiendo del tipo de test.

print("Intervalo de Confianza:")
print(intervalo_confianza)
print(intervalo_confianza2)
print(alpha)

#Existen varios comandos para los diferentes tipos de intervalos q podemos encontrar
# La función prop.test se usa para calcular intervalos de confianza para la porporción y diferencia de proporciones

prop.test(hombres,mujeres, p=NULL,alternative=c("two.sided", "less", "greater"),conf.level=0.95, correct=TRUE)

# Para construir intervalos de confianza para la varianza se puede usar la función var.test

res2 <- stats::var.test(x=hombres$altura, y=mujeres$altura, conf.level=0.95)
res2$conf.int



#--------------------------------------------------------------------------------------------------------
#https://estadistica-dma.ulpgc.es/estadFCM/html/Practica8-Desarrollo.html#ejercicio_6

url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
datos <- read.table(file=url, header=TRUE)
hombres <- datos[datos$sexo=="Hombre", ]
mujeres <- datos[datos$sexo=="Mujer", ]

hist(hombres$altura, las=1, xlab='Altura', ylab='Frecuencia',
     main='Histograma altura hombres')

hist(mujeres$altura, las=1, xlab='Altura', ylab='Frecuencia',
     main='Histograma altura mujeres')

# Si tuvieramos una grafica sobre los cuantiles, deberiamos fijarnos en que si se 
# crea una linea recta quiere decir que la muestra sigue una distribucion
# teorica
muestra2 <- sample(poblacion,100)
qqplot(muestra2,muestra , main = "Q-Q Plot", col = 2)


# https://marcos-marva.web.uah.es/files_bioest/2016-17/Tutorial-04.pdf
#Variables aleatorias con R
#EJERCICIO 1 
#Ejercicio 1.
# Simular n = 1000000 tiradas de dos dados y calcular la
# tabla de frecuencias relativas

set.seed(2014)
n = 1000000
dado1 = sample(1:6, n, replace=TRUE)
dado2 = sample(1:6, n, replace=TRUE)
suma = dado1 + dado2
table(suma)/n

rm(n,dado1,dado2,suma)

#EJERCICIo 1 pag 24

# 1. Una compañía de seguros posee la siguiente tabla sobre seguros para pérdidas en cosechas. 
# Porcentaje de Ha perdidas 0 25 50 100
# Probabilidad              0.9 0.05 0.03 ??
# Si ofrecen una poliza que paga 150 euros por cada hectárea perdida, ¿cuál es la indemnización
# media (en euros/hectárea) que esperan pagar?

#Sabemos que la prob total = 1
porcentajes = c(0,25,50,100)
prob = c(0.9,0.05,0.03,0.02)

vEsperado = sum(porcentajes * prob)#Porcentaje espeardo a perder por hectarea. (Media)
sol= (vEsperado * 150) / 100


#EJERCICIO 2 pag 24
# Sea X una variable aleatoria discreta cuya distribución viene dada por esta tabla:
# Valor         0    1   2   3   4   5
# Probabilidad 1/6 1/12 1/4 1/4 1/12 1/6
# Calcular la media de las variables 5X + 1 y X^2 

# E[aX] = aE[X]
x=c(0,1,2,3,4,5)
probs= c(1/6,1/12,1/4,1/4,1/12,1/6)

5*sum(x*probs)+1

x2 = (c(0, 1, 2, 3, 4, 5))^2
sum(x2*probs)

  
#EJERCICIO 3 pag 24
# 3. En el chuck-a-luck,el croupier lanza tres dados.
# Cada jugador apuesta por un número entre 1 y 6 y recibe una cantidad igual a su apuesta
# si el número aparece una vez, el doble si aparece dos veces y el triple si aparece tres veces.
# Si su número no figura entre los resultados, pierde su apuesta. Calcular el benefcio esperado
# del jugador

dbinom(0,size=3,prob=1/6) #Prob de 0 veces el numero que queremos
dbinom(1,size=3,prob=1/6) #Prob de 1 vez
dbinom(2,size=3,prob=1/6) #Prob de 2 veces
dbinom(3,size=3,prob=1/6) #Prob de 3 veces

probsChuck = dbinom(0:3,size=3,prob=1/6)
probsChuck

beneficioEsperado = sum(c(-1,1,2,3)*probsChuck)
beneficioEsperado #Es negativo ya que gana la banca casi un 8%


#Ejercicio 6 pag 24

# Sea X una variable aleatoria discreta cuya densidad de probabilidad viene dada por esta
# tabla: 
# Valor         6    7    8   9   10
# Probabilidad 0.05 0.1 0.6 0.15 0.1
# a) Hallar la función de distribución acumulada F .
# b) Usar F para calcular P (X ≤ 8).
# c) Usar F para calcular P (X < 8). Usar F para calcular P (X > 7). Usar F para calcular
# P (7 ≤ X ≤ 9).

#La función de distribución está relacionada con las probabilidades acumuladas, que puedes
#calcular como si se tratara de las frecuencias relativas acumuladas:

probsACumuladas <- cumsum(c(0.05,0.1,0.6,0.15,0.1))
probsACumuladas

#B -> P(X <= 8) = 0.75
#C -> P(x < 8) = P(x <= 7) = 0.15
#P(x > 7) = 1-P(X<=6) = 1- 0.05 = 0.95
#P(7<= X <= 9) = P(x<=9)-P(X<7) = P(X<=9) - P(X<=6) = 0.9-0.05 = 0.85

rm(list = ls())#Borra el enviroment

var1 <- 4
var2<- 8
save.image(file = "Variables.RData") #Guarda todo el entorno
save(var1,var2,file = "Variables.RData") #Guarda ciertas variables

load("Variables.RData") #Carga las variables guardadas siempre que el fichero
#este en la carpeta en la que se encuentra el entorno del programa 

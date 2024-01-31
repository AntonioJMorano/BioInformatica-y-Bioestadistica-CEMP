# https://rpubs.com/joser/objetos
#Principales objetos de R
#VECTORES-----------------------------------------------------------

# Valores especiales
x <- c(NA,NaN,Inf,0,3,NULL)

#GENERACION

x <- c(1,2,3,15)
y <- c(5,0,2,2)
c(x,y)

#Para crear secuencias podemos usar:

1:10 #Del 1 al 10
seq(1,13,3) #Del 1 a 13 de 3 en 3
rep(c(1,3),2) #Repite el vector en C 2 veces


#OPERACION

3 + c(1,2,3)#Sumamos 3 a cada pos del vector

c(1,2,1) + c(7,8,9)#Sumamos ambos vectores

x[2] #Muestra el 2 valor del vector
x[c(2,3)] #Muestra los valores en las posiciones 2 y 3 del vector
x[-c(2,3)] #Muestra todas las posiciones del vector menos la 2 y 3

x[x < 14] #Devuelve los valores que cumplen la condicion
 
which(x > 14)#Devuelve las posiciones que cumplen la condicion


#MATRICES------------------------------------------------------------

m <- matrix(1:8, 3,4)#3 filas y 4 columnas
m
m + c(1,2,3) #Se suma este vector a cada columna

m[1,2] #Valor en la pos (1,2)
m[2,]#Muestra la fila 2
m[,3]#Muestra la columna 3
m[3, -1]#Muestra la fila 3, quitando los valores de la columna 1

#Crear matrices a traves de union de vectores
v1 <- 1:3
v2 <- 4:6

cbind(v1,v2)#Une los vectores en columnas
rbind(v1,v2)#Une los vectores en filas

mean(m)

apply(m,1,mean) #Hace la media a las filas
apply(m,2,mean) #Hace la media a las columnas

apply(m,1,sort)
apply(m,2,sort)

#Principales funciones de las matrices
# 
# Producto matricial: A %*% B
# Producto elemento a elemento: A*B
# Traspuesta: t(A)
# Determinante: det(A)
# Extraer la diagonal: diag(A)
# Resolver un sistema de ecuaciones lineales (\( Ax=b \)): solve(A,b)
# Inversa: solve(A)
# Autovalores y autovectores: eigen(A)



#LISTAS--------------------------------------------------------

Lista <- list(nombre="Antonio",edad = 23, notas = c(4,7,6))
str(Lista)

Lista$nombre#Accede al elemento de la lista con dicho nombre
Lista[[1]]#Accede a la posicion 1 de la lista
Lista$notas[2]#Accede a pos 2 del elemento notas

#DATAFRAMES----------------------------------------------------------
edades = 6:9
grupos <- c("a","b","C","e")

fichero <- data.frame(edad = edades , grupo = grupos) #Creacion 
fichero

fichero$edad #Buscar el elemento edad en el fichero

fichero[2,] #Segunda fila del fichero

#FACTORES------------------------------------------------------------
v <- c(2,2,3,4,2,5)
vfactor <- factor(v)
vfactor



rm(list = ls()) #Borra el enviroment
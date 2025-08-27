##############################################################################
# Microsimulación - 2025                                                     #
# Daniel Ciganda                                                             #
# LABORATORIO: MODELO DE SCHELLING                                           #
##############################################################################

# La idea de los siguientes ejercicios es aproximarse paso a paso a la construcción
# del modelo de Schelling. Comenzaremos por definir la ciudad y asignar los
# agentes de cada color en distintas posiciones. Para simplificar el ejercicio, de
# momento trabajaremos en una ciudad sin espacios vacios, es decir, cada lugar
# estará ocupado por un agente de uno de los dos tipos ("colores") que definen el
# modelo de Schalling clásico. 

# Ejercicio: crear una matriz "city" de 8x8 y correr un loop para 
# asignarle valores del 1 a 64.
# ayuda: los argumentos ncol y nrow en matrix() definen el tamano de la matriz.

city <- 
for (i in 1:64){
  
  city[i] <- 
  
}

# a notar: la forma en que R completa la matriz, el orden de los valores.

# ----------------------------------------------------------------------------
# Indices
# Ejercicio: sustituir los valores en "city" por una cadena consecutiva de 1s y 2s.
# ayuda: esto puede realizarse con una sola linea de codigo utilizando los 
# parentesis rectos y  el reciclaje de valores que R realiza por default.



# creamos un vector "ind" que contiene los indices de la matriz city, en orden.
# creamos otro vector "side" que contiene el largo de uno de los lados de city.

ind <- 1:dim(city)[1]^2
side <- dim(city)[1]

# ----------------------------------------------------------------------------
# Repaso: Módulo y División Entera
# ----------------------------------------------------------------------------
# Para entender cómo R (y muchos otros lenguajes de programación) manejan las
# posiciones en una matriz o cualquier estructura de datos secuencial, es
# fundamental comprender dos operadores aritméticos clave: 
# la división entera (%/%) y el módulo (%%).
#
# Estos operadores son muy útiles cuando necesitamos convertir un índice lineal
# (como los números del 1 al 64) en coordenadas de dos dimensiones (fila y
# columna), una tarea central en el modelo de Schelling.

# La división entera nos devuelve la parte entera del cociente de una
# división, descartando el resto. 

7 %/% 5 
5 %/% 7 

# El operador módulo nos devuelve el **resto** de una división entre enteros.
# Es el complemento de la división entera. 

7 %% 5 
5 %% 7 
# ----------------------------------------------------------------------------

# Ejercicio: crear un loop que obtenga el resto de la division entre enteros
# de cada elemento de ind entre side e imprimir los resultados

for (){
  print()
}

# repetir la operacion restando 1 a cada i.

for (){
  print()
}

# sumar 1 al resultado de la division anterior e imprimir el resultado
# Notar que los valores obtenidos corresponden a las filas que correspondedn a cada indice

for (){
  print()
}

# crear un loop similar al primero pero que en vez de devolver el resto, 
# nos devuelva el resultado de la division entre enteros

for (){ 
  print() 
}

# repetir la operacion restando 1 a cada i.

for (){ 
  print() 
}

# sumar 1 a la division anterior e imprimir el resultado: a que corresponden los valores obtenidos?
for (){ 
  print() 
}

# Ceamos una funcion "mod" y otra "div" con dos argumentos (a,b) que devuelvan
# los resultados obtenidos antes.

mod <- function(a, b){ 
  1 + ((a - 1) %% b)
}
div <- function(a, b){ 
  1 + ((a - 1) %/% b) 
}

#############################################################################
# Definiendo coordenadas y vecindario de cada agente  ----------------------#
# ###########################################################################

# Para recrear la dinámica del modelo de Schelling es clave poder determinar 
# la composición del vecindario de cada agente, para deterimnar si están satisfechos
# o insatisfechos en ese lugar, y moverlos en caso de que estén insatisfechos.
# Una forma posible de hacer esto es determinar las cordenadas (fila, columna) y 
# a partir de esta información, determinar las coordenadas del vecnidario.

# Ejercicio: Utilizar las funciones mod y div definidas antes para obtener las 
# coordenadas de la posición 32 en city. 
# Guardar la fila en un objeto "x" y la columna en un objeto "y".

x <- 
y <- 


# Definimos un vector auxiliar check que va de -1 a 1.
check <- -1:1

# Ejercicio: Utilizando la funcion mod junto con los vectores check y side y las coordenadas 
# obtenidas arriba, obtener las filas y columnas del vecindario de la posicion 32
# y guardarlo en las objeto "xregion" e "yregion".


xRegion <- 
yRegion <- 

# crear una matriz "region" con el vecindario de la posición 32
region <- 

# una vez obtenido el vecindario ("region") obtener el nr de vecinos similares y
# el numero de vecinos totales del agente en la posición 32 utilizando sum()

sSimilar <- 
total <- sum(region > 0) - 1 # En el modelo final puede haber lugares vacios

# definir si el agente en la posición 32 está insatisfecho con la comppsición de su
# barrio asumiendo una tolerancia de hast 50% de vecinos de otro color.
unhappy <- ( <   * 0.5)

#############################################################################
# Loop Central -------------------------------------------------------------#
# ###########################################################################

# Ahora que sabemos como determinar el vecindario de un agente y su composición
# podemos determinarlo para cada uno de los agentes en la ciudad con un bucle.
# Para guardar esta información definimos dos vectores: uno que contendrá las 
# posiciones de los agentes insatisfechos y otro que contendrá la proporción de 
# agentes similares en cada vecindario

unhappy <- rep(NA, length(city))
similar <- rep(NA, length(city))

# asumimos una tolerancia de 50%
alikePref <- 0.5

for (n in which(city > 0)) {
  
}

#############################################################################
# Moviendo Agentes Insatisfechos  ------------------------------------------#
# ###########################################################################

# Una vez que tenemos la información sobre que agentes están insatisfechos y
# podemos proceder a "mudarlos" a otro vecindario.

# Ejercicio: Agregar un comentario explicando como funciona cada una de las
# líneas siguientes: 

whoUnhappy <- which(unhappy) 
randUnhappy <- whoUnhappy[sample.int(length(whoUnhappy))] 
empty <- which(city == 0) 

for (i in randUnhappy) {
  dest <- sample.int(length(empty), 1) 
  city[empty[dest]] <- city[i] 
  city[i] <- 0 
  empty[dest] <- i 
}




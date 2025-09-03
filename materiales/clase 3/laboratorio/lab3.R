##############################################################################
# Microsimulación - 2025                                                     #
# Daniel Ciganda                                                             #
# LABORATORIO: MODELO DE SCHELLING                                           #
##############################################################################

# El objetivo del laboratorio es terminar de comprender la implementación 
# del modelo de Schelling y generar las herramientas computacionales para
# analizarlo.
# Comenzamos con la implementación completa, seguida de una serie de ejercicios.

############################
# Inicializando el Modelo  #
############################

# Definimos los parámetros del modelo
density <- 0.75
citySize <- 51
years <- 100
alikePref <- 0.7

# Definimos las funciones para extraer las coordenadas y regiones de c/agente.
mod <- function(a, b){ 
  1 + ((a - 1) %% b)
}
div <- function(a, b){ 
  1 + ((a - 1) %/% b) 
}

# Definimos la composición de la ciudad al inicio de la simulación.
full <- floor(citySize ^ 2 * density)
city <- matrix(0, citySize, citySize)
occupied <- sample(1:(citySize ^ 2), full)
city[occupied] <- c(1,2)

# definimos los Vectores auxiliares  
side <- dim(city)[1]
check <- -1:1
unHappyMonitor <- c()
similarMonitor <- c()

# definimos el bucle central

for (t in 1:years) {
  
  lastCity <- city
  unhappy <- rep(NA, length(city))
  similar <- rep(NA, length(city))

# Definiendo estado del agente y composición de los barrios. 
  
for (n in which(city > 0)) {
  
  x <- mod(n, side) # fila del agente de referencia
  y <- div(n, side) # columna del agente de referencia
  
  # Región
  xRegion <- mod(check + x, side) # filas vecinas
  yRegion <- mod(check + y, side) # columnas vecinas
  region <- city[xRegion, yRegion]
  
  # cantidad de agente iguales a ego
  sSimilar <- sum(region == city[n]) - 1
  
  # total de agentes
  total <- sum(region > 0) - 1
  similar[n] <- sSimilar / total
  unhappy[n] <- (sSimilar < total * alikePref)
}
  
# Monitores
whoUnhappy <- which(unhappy)
unHappyMonitor <- c(unHappyMonitor,
                    length(whoUnhappy)/(length(which(!unhappy)) + length(whoUnhappy)))
similarMonitor <- c(similarMonitor, mean(similar, na.rm = T))
  
# Moviendo Agentes Insatisfechos  
randUnhappy <- whoUnhappy[sample.int(length(whoUnhappy))]
empty <- which(city == 0)
  
for (i in randUnhappy) {
  dest <- sample.int(length(empty), 1) # Lugar vacio de destino
  city[empty[dest]] <- city[i] # asigno valor del agente al lugar de destino
  city[i] <- 0 # vacio el lugar original
  empty[dest] <- i # agrego el lugar que quedo vacio al vector de lugares vacios
}

Sys.sleep(0.1)

# visualización
par(mfrow = c(1,3))
image(city, col = c("black","red","green"), axes = F)

plot(runif(years,0,1), ylab = "Proportion Unhappy",
     xlab = "Years", col = "white", ylim = c(0,1))
lines(unHappyMonitor, oma = c(0, 0, 2, 0), col = "red")

plot(runif(years,0,1), ylab = "Proportion Similar",
     xlab = "Years", col = "white", ylim = c(0,1))
lines(similarMonitor, oma = c(0, 0, 2, 0), col = "red")

  if (identical(lastCity, city)) { break }
}

##############
# Ejercicios #
##############

# 0) Correr el modelo modificando los parametros "alikePref" y "density" para
#   obtener una intuición de como cada uno de estos parámetros incide en los
#   resultados del modelo.  

# 1) Definir una función "schelling" que encapsule el modelo y tome como 
#    argumentos los parámetros years, density, alikePref y citySize.
#    Asignar los valores por defecto 100 y 51 a "years" y "citySize" respectivamente. 
#    El objetivo es facilitar la inspección de los resultados al manipular los parámetros.

# 2) Pedir a la función que devuelva el valor final de la homogeneidad promedio de los barrios. 

schelling <- # completar

# 3) Correr el modelo para los siguientes valores de:
#    density - seq(0.45, 0.95, .05) - dejando alikePref fijo en 0.5
#    alikePref  - seq(0, 1, .1) - dejando density fijo en 0.7


pref_vals <- seq(0, 1, .1)
out_1 <- # completar con sapply

dens_vals <- seq(0.45, 0.95, .05)

out_2 <- # completar con sapply

# 4) Graficar la relación de cada uno de lo parámetros con el promedio de homogeniedad de los barrios

plot(, , main = paste("Resultados por Preferencias","-", "Densidad fija en 0.7"),
     ylab = "Proporcion Similar", xlab = "Preferencias Similaridad")


plot(, , main = paste("Resultados por Densidad","-", "Preferencias fijas en 0.5"),
     ylab = "Proporcion Similar", xlab = "Densidad")


# 5) Definir  la siguiente combinación de parámetros a explorar: 
#   expand.grid(pref = seq(0, 1, .1), dens = seq(0.45, 0.95, .05))
#   computar el resultado en cada combinación y graficar la relación entre 
#   los inputs y outputs del modelo usando persp()

# Para exlplorar una mayor parte de la superficie del espacio usamos un
# diseño factorial (con 5 niveles).
d_fact <- # completar con expand.grid()
plot(d_fact)

# La idea es mapear las 121 combinaciones en d_fact al resultado del modelo 
# es decir, el nivel de segregacion.
# Correr el modelo todas estas veces lleva tiempo asi que vamos a computar en paralelo

# llamar "parallel" y definir el cluster
library(parallel)
crs <- detectCores()
cl <- makeCluster(crs-1, type = "PSOCK")

# exportar parametros
param_list <- split(d_fact, sort(1:dim(d_fact)[1] %% crs))
clusterExport(cl, "param_list")

# exportar funcion
invisible(
  clusterCall(cl, function() {
    source("schelling_fun.R") 
  }))

out <- #completar con parlapply y mapply

d_fact$out <- unlist(out)

dev.off()
# Grafiquemos las tres dimensiones
persp(
  # tomamos los valores unicos de cada factor
  unique(d_fact$pref),
  unique(d_fact$dens), 
  # creamos una matriz con los outputs
  matrix(d_fact$out, nrow = length(unique(d_fact$pref))),
  xlab = "Preferencias",
  ylab = "Densidad",
  zlab = "Resultado",
  # parametros graficos
  theta = 200,
  phi = 30,
  shade = 0.6,
  col = "orange"
)

plot_3D <- function(x){
  persp(
    # tomamos los valores unicos de cada factor
    unique(d_fact$pref),
    unique(d_fact$dens), 
    # creamos una matriz con los outputs
    matrix(d_fact$out, nrow = length(unique(d_fact$pref))),
    xlab = "Preferencias",
    ylab = "Densidad",
    zlab = "Resultado",
    # parametros graficos
    theta = x,
    phi = 30,
    shade = 0.6,
    col = "orange"
  )
  Sys.sleep(0.5)
}

lapply(rev(seq(0, 200, 10)), plot_3D)

filled.contour(unique(d_fact$pref),
               unique(d_fact$dens), 
               matrix(d_fact$out, nrow = length(unique(d_fact$pref))),
               xlab = "Preferencias",
               ylab = "Densidad",
               main = "Proporcion Media de homogeneidad en los barrios por preferencias y densidad",
               cex.main = 0.9)


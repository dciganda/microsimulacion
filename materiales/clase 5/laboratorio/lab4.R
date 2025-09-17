##############################################################################
# Microsimulación - 2025                                                     #
# Daniel Ciganda                                                             #
# LABORATORIO: ANALISIS MODELO DE SCHELLING                                  #
##############################################################################
# El objetivo del laboratorio es profundizar en algunas técnicas para realizar
# analisis de modelos computacionales a partir de la construcción de metamodelos.


###################
# MODELO BASE     #
###################
# Comenzamos definiendo la funcion que implementa nuestro modelo de Schelling.
# Esta función agrega un argumento "parameters" que da la opción de imprimir 
# en la consola el valor de los parámetros (preferencias, densidad) en cada 
# corrida del modelo.

schelling <- function(alikePref, density, years = 100,
                      citySize = 51, parameters = F){
  
  mod <- function(a, b) { 1 + ((a - 1) %% b) }
  div <- function(a, b) { 1 + ((a - 1) %/% b) }
  
  # Definiendo la composición de la ciudad al inicio de la simulación.
  full <- floor(citySize ^ 2 * density)
  city <- matrix(0, citySize, citySize)
  occupied <- sample(1:(citySize ^ 2), full)
  city[occupied] <- c(1,2)
  
  # Vectores  
  side <- dim(city)[1]
  check <- -1:1
  
  # Prealocamos los monitores
  unHappyMonitor <- vector("numeric", years)
  similarMonitor <- vector("numeric", years)
  
  for (t in 1:years) {
    
    lastCity <- city
    unhappy <- rep(NA, length(city))
    similar <- rep(NA, length(city))
    
    # Definiendo estado del agente y composición de los barrios.  
    for (n in which(city > 0)) {
      x <- mod(n, side)
      y <- div(n, side)
      xRegion <- mod(check + x, side)
      yRegion <- mod(check + y, side)
      region <- city[xRegion, yRegion]
      sSimilar <- sum(region == city[n]) - 1
      total <- sum(region > 0) - 1
      
      similar[n] <- sSimilar / total
      unhappy[n] <- (sSimilar < total * alikePref)
    }
    
    
    # Monitores
    whoUnhappy <- which(unhappy)
    
    unHappyMonitor[t] <- length(whoUnhappy) / length(which(!is.na(unhappy)))
    similarMonitor[t] <- mean(similar, na.rm = TRUE)
    
    # Moviendo Agentes Insatisfechos  
    randUnhappy <- whoUnhappy[sample.int(length(whoUnhappy))]
    empty <- which(city == 0)
    
    for (i in randUnhappy) {
      dest <- sample.int(length(empty), 1)
      city[empty[dest]] <- city[i]
      city[i] <- 0
      empty[dest] <- i
    }
    
    if (identical(lastCity, city)) {  
      cat(paste("\n Everybody Happy after", t, "years"))
      break  
    }
    
    if (t == years){
      cat("\n End Iterations")
    }
    
  }
  
  # Using 't' as the index to get the value from the last completed iteration
  last_similarity <- similarMonitor[t]
  
  cat(paste("\n Proportion Similar", last_similarity))
  if(parameters){
    cat(paste("\n Preferences:", alikePref, "\n Density:", density))
    
  }
  return(invisible(last_similarity))
  
}

# En el laboratorio anterior comenzamos a explorar la relación entre los 
# inputs y outputs de nuestro modelo a partir de un diseño factorial (una grilla
# discreta con combinaciones de valores de nuestros parámetros).

d_fact <- expand.grid(pref = seq(0, 1, .1), dens = seq(0.45, 0.95, .05))
plot(d_fact)

# Lo que hicimos fue mapear las 121 combinaciones en d_fact al resultado del
# modelo, es decir, al nivel de segregacion.
# Correr el modelo todas estas veces lleva tiempo asi que lo computamos en paralelo

# llamar "parallel" y definir el cluster
library(parallel)
crs <- detectCores()
cl <- makeCluster(crs-1, type = "PSOCK")

# exportar parametros y función
param_list <- split(d_fact, sort(1:dim(d_fact)[1] %% (crs-1)))
clusterExport(cl, c("param_list", "schelling"))

out <- parLapply(cl, param_list, function(x) mapply(schelling, x$pref,
                                                    x$dens, parameters = T))

d_fact$out <- unlist(out)

stopCluster(cl)

dev.off()

# Definimos una función para obtener nuestro gráfico en tres dimensiones
plot_schelling_surface <- function(x_vals, y_vals, z_vec,
                                   title = "", theta = 200, phi = 30) {
  
  # Reshape the output vector into a matrix suitable for persp()
  z_matrix <- matrix(z_vec, 
                     nrow = length(x_vals), 
                     ncol = length(y_vals))
  
  # Create the 3D perspective plot
  persp(
    x = x_vals,
    y = y_vals,
    z = z_matrix,
    main = title,
    xlab = "Preferencias",
    ylab = "Densidad",
    zlab = "Resultado",
    theta = theta,
    phi = phi,
    shade = 0.6,
    col = "orange"
  )
}


# Graficamos:
plot_schelling_surface(
  x_vals = unique(d_fact$pref), 
  y_vals = unique(d_fact$dens), 
  z_vec = d_fact$out,
  title = "Resultados de Simulación (Original)"
)


#########################
# META MODELOS          #
#########################

# En esta sección no enfocamos en la construcción de meta modelos (o emuladores)
# para el modelo de Schelling. Un meta-modelo es un modelo de la relación entre
# los inputs y los outputs de un modelo base, en este caso la relación entre
# preferencias, densidad y la segregación en la cidad.
# Los meta-modelos pueden ser muy últiles porque nos permiten, una vez entrenados,
# obtener predicciones para combinaciones de paramteros sin tener que computar
# el modelo en esas combinaciones. El output en las combinaciones que no 
# observamos es aproximado por una funcion (nuestro meta-modelo).
# El objetivo es obtener una buena funcion, que no imponga demasiadas 
# restricciones a la forma de la relación inputs-outputs (parámetros - segregación
# en el caso del modelo de shcelling).

##############################
# Entrenamiento / Validacion #
##############################
# Comenzamos por particionar nuestros datos (simulaciones del modelo base) en 
# un conjunto de entrenamiento y un conjunto de validación. Con el primero vamos
# a ajustar distintos modelos y con el segundo vamos a evaluar su ajuste.

# Definimos la fracción a utilizar para la validación
val_frac <- 0.2

# Sampleamos los puntos (indices) para cada conjunto de datos
val_idx <- # completar usando sample()
train_idx <- setdiff(seq_len(nrow(d_fact)), val_idx)

# realizamos la partición
d_train <- # completar
d_val   <- # completar


#####################
# Modelo lineal     #
#####################

# Ahora ajustemos un modelo lineal simple utilizando el conjunto de entrenamiento.
# En R los modelos lineales se ajustan con la funcion lm(), ej. lm(y ~ x_1 + x_k)
model1 <- # completar

# Ahora generamos una grilla (combinaciones de valores de nuestros parámetros)
# donde vamos a generar predicciones a partir de nuestros metamodelos, esta grilla
# puede ser arbitrariamente fina (ajustando length.out)
new_pref <- seq(min(d_fact$pref), max(d_fact$pref), length.out = 30)
new_dens <- seq(min(d_fact$dens), max(d_fact$dens), length.out = 30)
new_space <- expand.grid(pref = new_pref, dens = new_dens)
plot(new_space)

# Obtenemos predicciones a partir de nuestro metamodelo para cada uno de los
# puntos en nuestra grilla
predictions_lm <- #completar con predict()

# Graficamos los resultados. 
# Definimos tres columnas en nuestro display gráfico y graficamos primero 
# la supericie original
par(mfrow = c(1, 3))

plot_schelling_surface(
  x_vals = unique(d_fact$pref), 
  y_vals = unique(d_fact$dens), 
  z_vec = d_fact$out,
  title = "Resultados de Simulación (Original)"
)

# Ahora el modelo lineal (completar)
plot_schelling_surface(
  x_vals = , 
  y_vals = , 
  z_vec = ,
  title = "Meta-Modelo Lineal Simple"
)

# Describir los que se observa en el gráfico





#############################################################
# Modelo lineal con interacciónes y términos cuadráticos    #
#############################################################
# Ahora definimos un modelo también lineal pero un poco más flexible, 
# incorporando un término cuadrático para pref y la interaccion pref * dens.
# los terminos de orden superior se definen con la ayuda de I(), ej I(x^2).
model2 <- # completar

predictions_lm2 <- #completar

plot_schelling_surface(
  x_vals = , 
  y_vals = , 
  z_vec = ,
  title = "Meta-Modelo Cuadrático"
)

# Describir los que se observa en el gráfico




# reseteamos los parámetros gráficos
par(mfrow = c(1, 1)) 

#############################################################
# Modelo no paramétrico (Proceso Gaussiano)                 #
#############################################################
# Ahora vamos a ajustar un modelo no paramétrico para ver si podemos representar 
# mejor la superficie de referencia. Vamos a utilizar un proceso gaussiano, un 
# algoritmo muy utilizado en la literatura del análisis de modelos computacionales
# por su capacidad para aproximar funciones complejas.
library(GPfit)

# Entrenamiento
# Esta implemantación necesita los inputs en una matriz
X_train <- as.matrix(d_train[, c("pref", "dens")])
y_train <- d_train$out

model3 <- GP_fit(X_train, y_train)

# Predicción del GP en la misma grilla
predictions_gp <- predict(model3, xnew = as.matrix(new_space))$Y_hat

plot_schelling_surface(
  x_vals = ,
  y_vals = ,
  z_vec  = ,
  title  = "Meta-Modelo GP"
)

# Describir los que se observa en el gráfico





################################################################
# VALIDACIÖn                                                   #
################################################################
# Otra forma de comprobar que nuestro meta modelo representa adecuadamente la
# superficie que define la relacion entre inputs y outputs es generando 
# predicciones con el modelo en nuevas combinaciones de parametros y 
# comparandolas con los valores reales del output en esas combinaciones
# (es decir, valores simulados a partir del modelo base)

# Obtenemos las predicciones de nuestros metamodelos para el conjunto de 
# entrenamiento y el de validacion
pred_m1_tr <- # completar
pred_m1_va <- # completar

pred_m2_tr <- # completar
pred_m2_va <- # completar 

pred_m3_tr <- predict(model3, xnew = as.matrix(d_train[, c("pref", "dens")]))$Y_hat
pred_m3_va <- predict(model3, xnew = as.matrix(d_val[,   c("pref", "dens")]))$Y_hat


# Graficamos
# La idea es graficar las predicciones del meta-modelo contra los valores 
# observados.Las puntos deberían concentrarse en la diagonal central


# Definir un rango COMÚN de ejes para los tres gráficos (mejor comparabilidad)
rng <- range(c(
  d_train$out, d_val$out,
  pred_m1_tr, pred_m1_va,
  pred_m2_tr, pred_m2_va,
  pred_m3_tr, pred_m3_va
))

par(mfrow = c(1, 3))

# -----------------------
# 1) Modelo Lineal
# -----------------------
plot(, , type = "n",
     xlim = rng, ylim = rng,
     xlab = "Resultado Real (Simulación)",
     ylab = "Predicción del Meta-Modelo",
     main = "Validación: Lineal")
abline(0, 1, col = "gray", lty = 2, lwd = 2)
points(, , col = "blue", pch = 16, cex = 1.2)  # entrenamiento
points(,  , col = "red",  pch = 16, cex = 1.2) # validación
legend("bottomright",
       legend = c("Entrenamiento", "Validación", "Perfecto"),
       col = c("blue", "red", "gray"),
       pch = c(16, 16, NA),
       lty = c(NA, NA, 2),
       bty = "n")

# Describir los que se observa en el gráfico



# -----------------------
# 2) Modelo Cuadrático
# -----------------------
plot(, , type = "n",
     xlim = rng, ylim = rng,
     xlab = "Resultado Real (Simulación)",
     ylab = "Predicción del Meta-Modelo",
     main = "Validación: Cuadrático")
abline(0, 1, col = "gray", lty = 2, lwd = 2)
points(, , col = "blue", pch = 16, cex = 1.2)  
points(,, col = "red",  pch = 16, cex = 1.2)  
legend("bottomright",
       legend = c("Entrenamiento", "Validación", "Perfecto"),
       col = c("blue", "red", "gray"),
       pch = c(16, 16, NA),
       lty = c(NA, NA, 2),
       bty = "n")

# Describir los que se observa en el gráfico




# -------------------------
# 3) Modelo No Paramétrico
# -------------------------
plot(, , type = "n",
     xlim = rng, ylim = rng,
     xlab = "Resultado Real (Simulación)",
     ylab = "Predicción del Meta-Modelo",
     main = "Validación: No paramétrico")
abline(0, 1, col = "gray", lty = 2, lwd = 2)
points(, , col = "blue", pch = 16, cex = 1.2)  
points(, , col = "red",  pch = 16, cex = 1.2)  
legend("bottomright",
       legend = c("Entrenamiento", "Validación", "Perfecto"),
       col = c("blue", "red", "gray"),
       pch = c(16, 16, NA),
       lty = c(NA, NA, 2),
       bty = "n")

# Describir los que se observa en el gráfico








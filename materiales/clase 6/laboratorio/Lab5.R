##############################################################################
# Microsimulación - 2025                                                     #
# Daniel Ciganda                                                             #
# LABORATORIO: ESTIMACIÓN DE PARÁMETROS CON ABC                              #
##############################################################################
#
# --- OBJETIVO ---
#
# El objetivo de este laboratorio es aprender a "invertir" un modelo 
# computacional para hacer inferencia sobre los parámetros que podrían haber 
# generado un dato observado.
#
# --- CONTENIDO ---
#
# 1. Un ejemplo con una muestra limitada de simulaciones para entender el 
#    flujo de trabajo de ABC y realizar un diagnóstico de resultados.
# 2. Un análisis más robusto cargando un set de datos pre-computado y de 
#    mayor tamaño para ver cómo la calidad de nuestra inferencia cambia con el
#    tamanio de la muestra.
#
##############################################################################
# --- 1. SETUP: MODELO BASE ---
schelling <- function(alikePref, density, years = 100,
                      citySize = 51){
  mod <- function(a, b) { 1 + ((a - 1) %% b) }
  div <- function(a, b) { 1 + ((a - 1) %/% b) }
  full <- floor(citySize ^ 2 * density)
  city <- matrix(0, citySize, citySize)
  occupied <- sample(1:(citySize ^ 2), full)
  city[occupied] <- c(1,2)
  side <- dim(city)[1]
  check <- -1:1
  similarMonitor <- vector("numeric", years)
  for (t in 1:years) {
    lastCity <- city
    unhappy <- rep(NA, length(city))
    similar <- rep(NA, length(city))
    for (n in which(city > 0)) {
      x <- mod(n, side); y <- div(n, side)
      xRegion <- mod(check + x, side); yRegion <- mod(check + y, side)
      region <- city[xRegion, yRegion]
      sSimilar <- sum(region == city[n]) - 1
      total <- sum(region > 0) - 1
      if (total > 0) { similar[n] <- sSimilar / total } else { similar[n] <- 0 }
      unhappy[n] <- (sSimilar < total * alikePref)
    }
    similarMonitor[t] <- mean(similar, na.rm = TRUE)
    whoUnhappy <- which(unhappy)
    randUnhappy <- whoUnhappy[sample.int(length(whoUnhappy))]
    empty <- which(city == 0)
    for (i in randUnhappy) {
      dest <- sample.int(length(empty), 1)
      city[empty[dest]] <- city[i]; city[i] <- 0
      empty[dest] <- i
    }
    if (identical(lastCity, city)) { break }
  }
  return(invisible(similarMonitor[t]))
}


# --- PARTE 1: EJEMPLO CON MUESTRA PEQUENIA ---

# PASO A: Construir un diseño factorial de 50 x 50. Vamos a utilizar 
# a prioris uniformes en los rangos que definimos en el lab anterior. 
full_design <- 
  
# Obtener uns muestra de 200 puntos de nuestro full_design  
n_small <- 
small_sample <- 
  
# PASO B: Correr las simulaciones en paralelo
# -------------------------------------------
library(parallel)
cl <- 
results_small <- 
stopCluster(cl)

small_sample$sim_out <- unlist(results_small)

# PASO C: Filtrado ABC
# --------------------
# El dato observado: Asumimos que observamos este nivel de segregación en 
# una ciudad real
observed_segregation <- 0.95


# Tomamos la distancia euclideana entre nuestras segregación observada y simulada
small_sample$distance <- 

# Aceptamos el 5% de los puntos que tienen menor distancia
acceptance_prop <- 
posterior_small <- 

# PASO D: Análisis de la posterior (Muestra Pequeña)
# -----------------------------------------------------------
cat(paste("Número de muestras aceptadas:", , "\n\n"))

# Visualización 
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# 1. Posterior Conjunta
plot(,,
     col = "blue", pch = 16,
     xlab = "Preferencia", ylab = "Densidad", main = "Posterior Conjunta (Pequeña)")


# Describir la que se observa en el gráfico




# 2. Posterior vs. Prior (Preferencia)
plot(density(small_sample$pref), col = "gray", lwd = 2, 
     main = "Posterior vs. Prior (Preferencia)", xlab = "Valor de 'pref'", ylim = c(0, 15))
lines(density(posterior_small$pref), col = "blue", lwd = 2)
legend("topright", legend=c("Prior", "Posterior"), col=c("gray", "blue"), lty=1, bty="n")


# Describir la que se observa en el gráfico




# 3. Posterior vs. Prior (Densidad)
plot(density(small_sample$dens), col = "gray", lwd = 2,
     main = "Posterior vs. Prior (Densidad)", xlab = "Valor de 'dens'", ylim=c(0,8))
lines(density(posterior_small$dens), col = "blue", lwd = 2)

# Describir la que se observa en el gráfico




# 4. Gráfico en blanco para la leyenda general o notas
plot(1, type="n", axes=FALSE, xlab="", ylab="")
text(1, 1, "Análisis de la Muestra Pequeña\n(n=1000, aceptado 5%)")

par(mfrow = c(1, 1))

# --- PARTE 2: ANÁLISIS ROBUSTO CON DATOS PRE-COMPUTADOS ---
cat("\n--- Iniciando Parte 2: Análisis con Muestra Grande ---\n")

# PASO E: Cargar la tabla pre-computada
# ---------------------------------------
full_simulation_table <- readRDS("schelling_abc_FULL.rds")

# PASO F: Filtrado ABC con la muestra grande
# ------------------------------------------
full_simulation_table$distance <- 
posterior_large <- 
  
# PASO G: Análisis completo de la posterior (Muestra Grande)
# -----------------------------------------------------------
cat(paste("Número de muestras aceptadas:", , "\n\n"))

# Visualización de Diagnóstico
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# 1. Posterior Conjunta
plot(posterior_large$pref, posterior_large$dens,
     col = "darkgreen", pch = 16,
     xlab = "Preferencia", ylab = "Densidad", main = "Posterior Conjunta (Grande)")


# Describir la que se observa en el gráfico




# 2. Posterior vs. Prior (Preferencia)
plot(density(full_simulation_table$pref), col = "gray", lwd = 2, 
     main = "Posterior vs. Prior (Preferencia)", xlab = "Valor de 'pref'", ylim = c(0, 15))
lines(density(posterior_large$pref), col = "darkgreen", lwd = 2)
legend("topright", legend=c("Prior", "Posterior"), col=c("gray", "darkgreen"), lty=1, bty="n")


# Describir la que se observa en el gráfico




# 3. Posterior vs. Prior (Densidad)
plot(density(full_simulation_table$dens), col = "gray", lwd = 2,
     main = "Posterior vs. Prior (Densidad)", xlab = "Valor de 'dens'", ylim=c(0,8))
lines(density(posterior_large$dens), col = "darkgreen", lwd = 2)


# Describir la que se observa en el gráfico




# 4. Gráfico en blanco para la leyenda general o notas
plot(1, type="n", axes=FALSE, xlab="", ylab="")
text(1, 1, paste("Análisis de la Muestra Grande\n(n=", nrow(full_simulation_table), ", aceptado 5%)"))

par(mfrow = c(1, 1))

# --- 4. COMPARACIÓN FINAL ---
# Comparación final de las densidades marginales para ver el efecto del tamaño de la muestra
plot(density(posterior_large$pref), col = "blue", lwd = 2, 
     main = "Comparación de Posteriores para 'Preferencia'", xlab = "Valor de 'pref'", ylim = c(0, 15))
lines(density(posterior_small$pref), col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Muestra Grande", "Muestra Pequeña"), 
       col = c("blue", "red"), lty = c(1, 2), bty = "n")


# Describir la que se observa en el gráfico



# Ejercicio: Volver a realizar el análisis para un dato de segregación observada
# de 0.65, Describir los resultados.





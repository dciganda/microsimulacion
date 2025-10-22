##############################################################################
# Microsimulación - 2025                                                     #
# Daniel Ciganda                                                             #
# LABORATORIO: MICROSIMULACION FECUNDIDAD                                    #
##############################################################################
########################
# 1ra PARTE            #
########################

source("plot_fun.R") # funciones para graficar

###############################################################
# Modelos del proceso reproductivo                            #
###############################################################

# El objetivo del laboratorio es implementar un modelo del proceso 
# reproductivo para una cohorte de mujeres en régimen de fecundidad natural.

# Número de mujeres en la cohorte a simular 
n <- 8

# Comensamos con un número bajo de mujeres para poder inspeccionar mejor los 
# objetos y entender la estructura de los modelos a implementar

###############################################################
# Inicio del proceso: Edad a la Unión                         #
###############################################################

# En régimen de "fecundidad natural" (poblaciones históricas), el proceso
# reproductivo comienza en la unión (matrimonio).
# Modelamos el "tiempo de espera" a la unión con una distribución log-normal.
# Elegimos la log-normal porque:
# - Impone positividad de tiempos,
# - Es flexible y frecuentemente usada para tiempos de espera

# Parámetros en "escala natural" (años): media y desvío estándar objetivo.
mu_m <- 20   # media de la distribución de edad a la unión (años)
sigma_m <- 1.1  # desvío estándar de la edad a la unión (años)

# Conversión a parámetros de la log-normal: meanlog y sdlog.
sdlog   <- sqrt(log1p((sigma_m / mu_m)^2))
meanlog <- log(mu_m) - 0.5 * sdlog^2

# Simulamos el tiempo de espera a la primera unión en MESES.
# (wt_u: waiting time to union). Partimos del nacimiento, por eso el tiempo
# al matrimonio equivale a la edad al matrimonio.

wt_u <- 

###############################################################
# Fecundabilidad y Período de No Suceptibilidad               #
###############################################################
# De momento vamos a asumir que la probabilidad de concebir es fija, tanto
# entre mujeres como en el tiempo (edades)

fi <- 0.2 # probabilidad de concebir

# Definimos un período de no suceptibilidad (amenorrea postparto) también fijo

delta <- 6 # período de no suceptibilidad - en meses - 

# simulamos el tiempo de espera en meses a la 1era concepción utilizando la
# distribución geométrica, para las n mujeres en la cohorte

wt_1c <- 

# calculamos la edad al primer nacimiento

wt_1b <- 

# ponemos todo en un data frame y graficamos
hst <- as.data.frame(cbind(id = 1:n,
                           edad = wt_1b/12,
                           paridad = 1))

plot_hst(hst, c(0.5, n), n)

# describa brevemente lo que se observa en el gráfico 




# Ejercicio:
# Simular la edad al segundo nacimiento para este cohorte de mujeres. Graficar.

wt_2c <- 
wt_2b <-  

hst <- as.data.frame(cbind(id = rep(1:n, 2),
                           edad = c(wt_1b, wt_2b)/12,
                           paridad = rep(1:2,each = n)))

plot_hst(hst, c(0.5, n), n)

########################
# 2nda PARTE           #
########################
###############################################################
# FECUNDABILIDAD: patrón por edad 
###############################################################

# Sabemos que la probabilidad de concebir tiene un patrón por edad, es 0 hasta
# la pubertad, luego crece rápidamente hasta alcanzar un máximo entre los 18 y
# los 30 años, y luego decrece hasta volver a 0 a edad 50 aproximadamente. 

# La intención es modelar este patrón con un modelo simple y el menor número de 
# parámetros posibles. Para esto vamos a utilizar dos bases polinómicas y 2 
# coefficientes, que nos permitiran generar un rango amplio de curvas.


# Definir edades reproductivas (10 - 50) en meses
a_min <- 10 * 12
a_max <- 50 * 12

# Escalamos la edad entre 0 y 1
as  <- 0:(a_max - a_min - 1) /  (a_max - a_min - 1)

# Definimos las bases
b1 <- 3 * as * (1 - as)^2
b2 <- 3 * as^2 * (1 - as)

# Asignamos un valor inicial a los coefficientes

beta1 = 0.4221214
beta2 = -0.06294189

# Computamos el patrón de la probabilidad de concebir por edad y graficamos

phi <- b1*beta1 + b2*beta2
plot((a_min:(a_max-1))/12,phi)

# Ejercicio:
# Explorar cambios en el patrón por edades de la probabilidad de concebir al 
# cambiar los valores de los coeficientes





###############################################################
# Modelo del Proceso Reproductivo 
###############################################################
# Ahora el objetivo es poner todos los conceptos anteriores en un modelo 
# que nos permita simular el proceso reproductivo de una cohorte de mujeres
# y a partir de esas trayectorias, computar unas tasas específicas de 
# fecundidad

# Ejercicio: completar todos los comentarios vacios, explicando que hace cada 
# línea. Para eso se sugiere explorar cada uno de los objetos. El objetivo es 
# entender en profunidad la lógica del modelo.

# Definimos:
ini_c = 8 # tamaño de la cohorte (nr de mujeres a simular)
beta1 = 0.4221214 # coefficiente fecundabilidad
beta2 = -0.06294189 # coefficiente fecundabilidad
delta = 8.621253 # duración amenorrea postparto
mu_m = 20.32025 # media de la edad a la union
sigma_m = 1.183069 # desvío de la distribcuión de eades a la unión
a_min = 10*12 # edad a partir de la cual la fecundabilidad > 0
a_max = 50*12 # edad fin del proceso reproductivo

# Fecundabilidad ---
as  <- (0:(a_max - a_min - 1)) /  (a_max - a_min - 1) # 
phi <- c(rep(0, 120), 3*as*(1 - as) * (beta1*(1 - as) + beta2*as)) #

# Edad a la unión
sdlog   <- sqrt(log1p((sigma_m / mu_m)^2)) 
meanlog <- log(mu_m) - 0.5 * sdlog^2
wt_m <- round(rlnorm(ini_c, meanlog, sdlog) * 12L)     # 

phi_i <- lapply(wt_m, function(x) phi[x:length(phi)]) # 

maxt  <- max(lengths(phi_i)) #
id   <- integer(0L)          # 
wt_c <- integer(0L)          # 

for (t in 1:maxt) {
  
  ids <- which(runif(ini_c) < vapply(phi_i, `[`, t, FUN.VALUE = numeric(1))) # 
  
  if (length(ids) != 0) {
    
    wts <- wt_m[ids] - 1L + t #
    id   <- c(id, ids)        #
    wt_c <- c(wt_c, wts)      #
    
    blk <- as.integer(round(9 + delta)) - 1L #  
    phi_i[ids] <- lapply(phi_i[ids], function(x){  #
      x[t: min(length(x), t + blk)] <- NA
      x
    })
  }
}

if (length(wt_c) == 0L) return(data.frame(age = 10:49, fx = 0))

age_birth <- (wt_c + 9L) / 12                     # 
births    <- tabulate(floor(age_birth) + 1L, nbins = 50L)  # 
fx_vec    <- births / ini_c #

out <- data.frame(age = 10:49, fx = fx_vec[11:50], row.names = NULL) #

###################
# Ejercicios      #
###################

# 1) Cuál es la diferencia en la forma de modelar las concepciónes entre
# la primera parte del laboratorio y el último modelo implementado?


# 2) Crear una función "comfert_natural" que reciba los inputs necesarios y devuelva 
# una serie de tasas específicas de fecundidad por edad.

comfert_natural <- function(ini_c = 30000, # tamaño de la cohorte (nr de mujeres a simular)
                            beta1 = 0.42, # coefficiente fecundabilidad
                            beta2 = -0.06, # coefficiente fecundabilidad
                            delta = 8.6, # duración amenorrea postparto
                            mu_m = 20.3, # media de la edad a la union
                            sigma_m = 1.1 # desvío de la distribcuión de eades a la unión
){
  
  # Fecundabilidad ---
  # Escalamos la edad entre 0 y 1
  as  <- (0:479)/479                        
  phi <- c(rep(0, 120), 3*as*(1 - as) * (beta1*(1 - as) + beta2*as))  
  
  # Edad a la unión
  sdlog   <- sqrt(log1p((sigma_m / mu_m)^2))
  meanlog <- log(mu_m) - 0.5 * sdlog^2
  wt_m <- round(rlnorm(ini_c, meanlog, sdlog) * 12L)     # marriage month (since age 0)
  wt_m <- pmin(length(phi), pmax(1L, wt_m))              # keep indices in 1..length(phi)
  
  # Períodos de exposición al riesgo para cada mujer (luego de la unión)
  phi_i <- lapply(wt_m, function(x) phi[x:length(phi)])
  
  # Definimos la exposición al riesgo con mayor duración
  maxt  <- max(lengths(phi_i))
  id   <- integer(0L)                       # ids of conceptions (who conceives this month)
  wt_c <- integer(0L)                       # conception month (absolute, since age 0)
  
  for (t in 1:maxt) {
    
    # Conception draws this month (NA past end are fine: comparison yields NA/ignored)
    ids <- which(runif(ini_c) < vapply(phi_i, `[`, t, FUN.VALUE = numeric(1)))
    
    if (length(ids) != 0) {
      
      # Absolute month of conception = marriage_month - 1 + offset t
      wts <- (wt_m[ids] - 1L) + t
      id   <- c(id, ids)
      wt_c <- c(wt_c, wts)
      
      # Post-conception non-susceptible window:
      # block length = round(gestation+postpartum) months; inclusive indexing
      blk <- as.integer(round(9 + delta)) - 1L
      phi_i[ids] <- lapply(phi_i[ids], function(x){
        x[t: min(length(x), t + blk)] <- NA
        x
      })
    }
  }
  
  # --- Aggregate to ASFR (ages 10..49) ---
  if (length(wt_c) == 0L) return(data.frame(age = 10:49, fx = 0))
  
  age_birth <- (wt_c + 9L) / 12                     # birth at ~9 months after conception
  births    <- tabulate(floor(age_birth) + 1L, nbins = 50L)  # bins 0..49 (start-of-interval)
  fx_vec    <- births / ini_c
  
  out <- data.frame(age = 10:49, fx = fx_vec[11:50], row.names = NULL)
  
  return(out)
  
}

###############################################################
# EJERCICIO :
# Objetivo: entender el mapeo INPUTS → OUTPUTS del modelo.
# La idea es Variar un parámetro a la vez y observar:
# (i) la curva ASFR (fx) y (ii) la edad de máxima fecundidad
# Nos concentramos en: 
# μ_m (edad media a la unión) y 
# δ (meses no-susceptibles posparto).
###############################################################

# Paletas de color (base R)
pal_mu <- function(n)  hcl.colors(n, palette = "Zissou1")
pal_dl <- function(n)  hcl.colors(n, palette = "TealGrn")

# --- 1) Escenario base ---------------------------
asfr_base <- comfert_natural()

################################################################
# Sensibilidad en mu_m (edad media a la unión)
################################################################
mu_m = 20.3
mu_grid <- seq(mu_m - 3, mu_m + 3, by = 1)

# computar el modelo en los distintos valores de mu_m usando lapply
asfr_mu_list <- 
  
fx_mu_mat <- do.call(cbind, lapply(asfr_mu_list, `[[`, "fx")) # completar descripción
col_mu    <- pal_mu(length(mu_grid))
lbl_mu    <- paste0("\u03BC","[m] = ", format(mu_grid, nsmall = 1))

# ASFRs
plot(asfr_base$age, asfr_base$fx, type = "l", lwd = 3, col = "black",
     xlab = "Edad", ylab = "ASFR", bty = "n",
     main = expression(paste("ASFR al variar  ", mu[m])))
matlines(asfr_base$age, fx_mu_mat, lty = 1, lwd = 2, col = col_mu)
legend("topright", bty = "n",
       legend = c(paste0("base: \u03BC[m] = ", format(mu_m, nsmall = 1)), lbl_mu),
       lwd = c(3, rep(2, length(mu_grid))),
       col = c("black", col_mu),
       title = expression(mu[m]))

# edad máxima vs mu_m 
peak_mu <- sapply(asfr_mu_list, function(df) df$age[which.max(df$fx)])
par(mar = c(4,4,2.5,1))
plot(mu_grid, peak_mu, type = "b", pch = 16, lwd = 2, bty = "n",
     xlab = expression(mu[m]), ylab = "Edad del pico",
     main = expression(paste("Edad del pico vs.  ", mu[m])),
     col = col_mu)
abline(h = asfr_base$age[ which.max(asfr_base$fx)],
       lty = 3, col = "grey60")
legend("topleft", bty = "n",
       legend = c(paste0("base: ",
                         round(asfr_base$age[which.max(asfr_base$fx)],1)),
                  lbl_mu),
       col = c("grey60", col_mu), lwd = c(1,2), pch = c(NA,16),
       title = "peak_age")

################################################################
# Sensibilidad en delta (no-susceptibilidad posparto)
################################################################
delta = 8.6
delta_grid <- seq(4, 14, by = 2)

asfr_dl_list <- 

fx_dl_mat <- do.call(cbind, lapply(asfr_dl_list, `[[`, "fx"))
col_dl    <- pal_dl(length(delta_grid))
lbl_dl    <- paste0("\u03B4"," = ", delta_grid, " m")

# ASFRs
plot(asfr_base$age, asfr_base$fx, type = "l", lwd = 3, col = "black",
     xlab = "Edad", ylab = "ASFR", bty = "n", ylim = c(0,0.7),
     main = expression(paste("ASFR al variar  ", delta)))
matlines(asfr_base$age, fx_dl_mat, lty = 1, lwd = 2, col = col_dl)
legend("topright", bty = "n",
       legend = c(paste0("base: \u03B4 = ", format(delta, nsmall = 1), " m"), lbl_dl),
       lwd = c(3, rep(2, length(delta_grid))),
       col = c("black", col_dl),
       title = expression(delta))

# edad máxima vs delta 
peak_dl <- sapply(asfr_dl_list, function(df) df$age[which.max(df$fx)])
par(mar = c(4,4,2.5,1))
plot(delta_grid, peak_dl, type = "b", pch = 16, lwd = 2, bty = "n",
     xlab = expression(delta~"(meses)"), ylab = "Edad del pico",
     main = expression(paste("Edad del pico vs.  ", delta)),
     col = col_dl)
abline(h = asfr_base$age[which.max(asfr_base$fx)],
       lty = 3, col = "grey60")
legend("topleft", bty = "n",
       legend = c(paste0("base: ",
                         round(asfr_base$age[which.max(asfr_base$fx)],1)),
                  lbl_dl),
       col = c("grey60", col_dl), lwd = c(1,2), pch = c(NA,16),
       title = "peak_age")


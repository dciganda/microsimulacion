################################################################################
# Microsimulación - Licenciatura en Estadística                                #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de trajectorias demográficas compatibles con datos   #
# agregados                                                                    #  
# 26 de Noviembre de 2025                                                      #   
################################################################################

# El objetivo es generar un conjunto de microdatos (historias de vida) simulados,
# que sean compatibles con unas las tasas de fecundidad y mortalidad observadas 
# para una cohorte dada.

# El modelo a utilizar combina el modelo mecanístico del proceso reproductivo
# visto en labortorios anteriores, con el uso del método de la transformada 
# inversa para generar tiempos de espera a la muerte. De esta forma se obtiene
# un modelo de riesgos competitivos en el que el proceso reproductivo puede 
# censurarse con el proceso de mortalidad.


# Comenzamos cargando los datos, que corresponden a las tasas específicas de 
# mortalidad y fecundidad de una cohorte de nacimiento.
mort <- read.csv(file.path("datos","mx.csv"))
fec <- read.csv(file.path("datos","fx.csv"))

# Exploración rápida
plot(mort)
plot(fec)

# --- Definición del Modelo ---
sim_t <- function(ini_c = 5000,
                  beta1 = 0.22,
                  beta2 = -0.06,
                  delta = 11,
                  mu_m = 20.5,
                  sigma_m = 2.1,
                  mx_ages,
                  mx_rates) {
  
  # --- 1. Mortalidad (Muestreo por Transformada Inversa) ---
  ste <- function(n, edades, lambda, Haz = F){
    
    inf <- seq(0, length(edades)-1,1)
    sup <- seq(1, length(edades))
    
    # Función de riesgo acumulado por tramos
    H.pw <- function(t, inf, sup, lambda){  
      p1 <-  pmax((t-inf), 0)
      p2 <-  pmin(p1, sup-inf)
      return(sum(lambda*p2)) 
    }
    
    H <- rep(NA, length(edades))
    x <- min(inf):max(sup)
    
    for (i in 1:length(H)){
      H[i] <- H.pw(x[i], inf, sup, lambda)
    }
    
    # Función objetivo para encontrar la raíz: H(t) = -log(u)
    f <- function(t, inferior, superior, nivel, u){
      return(H.pw(t, inf=inferior, sup=superior, lambda=nivel) + log(u))
    }
    
    # Probabilidad de sobrevivir más allá de la última edad
    ne <- exp(-H)[length(exp(-H))]
    # Número de individuos que mueren dentro del rango de la tabla
    nn <- n - sum(runif(n) < ne)
    
    if(nn == 0){
      t <- rep(Inf, n)
      return(t); break()
    }
    
    # Resolución numérica para encontrar el tiempo exacto
    root <- function(nn, inf, sup, lambda){
      u <- runif(nn, min=exp(-H)[length(exp(-H))])
      times <- rep(NA, nn)
      for(i in 1:nn){
        result <- uniroot(f, interval=c(0, length(lambda)),
                          u=u[i], inferior=inf, superior=sup, nivel=lambda)
        times[i] <- result$root
      }
      return(times)
    }
    
    t_e <- root(nn, inf, sup, lambda)
    
    # Mezclar tiempos encontrados con infinitos (sobrevivientes)
    if(n-length(t_e)!=0){
      t <- sample(c(t_e, rep(Inf, n-length(t_e))))
    }else{
      t <- t_e
    }
    
    if(min(edades)!=0){ 
      t <- t + min(edades)  
    }
    
    if(Haz){ return(list(t, H)) }else{ return(t) }
  }
  
  # --- 2. Generar Tiempos a Eventos ---
  
  # Edad a la Muerte 
  time_death_years <- ste(n = ini_c, edades = mx_ages, lambda = mx_rates)
  wt_d <- floor(time_death_years * 12L)
  
  # Edad al Matrimonio
  sdlog <- sqrt(log1p((sigma_m / mu_m)^2))
  meanlog <- log(mu_m) - 0.5 * sdlog^2
  wt_m <- round(rlnorm(ini_c, meanlog, sdlog) * 12L) 
  
  # Patrón de Fecundabilidad (prob. concebir) por edad
  ts  <- (0:479)/479  
  phi <- c(rep(0, 120), 3*ts*(1 - ts) * (beta1*(1 - ts) + beta2*ts))
  
  # --- 3. Construir Trayectorias de Riesgo Censuradas ---
  
  # Calcular fecha efectiva de fin del proceso reproductivo: Muerte menos
  # 9 meses (gestación) o edad 50
  end_dates <- pmin(length(phi), wt_d - 9L)
  
  # Usar Map para recortar 'phi' para cada individuo según su inicio/fin
  phi_i <- Map(function(start, end) {
    if (start > end) return(numeric(0)) # Muere antes de casarse
    phi[start:end] }, wt_m, end_dates)
  
    # --- 4. Bucle de Simulación ---
  
  maxt  <- max(lengths(phi_i))
  id    <- integer(0L)
  wt_c  <- integer(0L)
  
  if(maxt > 0) {
    for (t in 1:maxt) {
      
      # Sorteos de concepción
      # Compara un aleatorio uniforme 0-1 contra la probabilidad phi en el tiempo t
      ids <- which(runif(ini_c) < vapply(phi_i, `[`, t, FUN.VALUE = numeric(1)))
      
      if (length(ids) != 0) {
        wts  <- (wt_m[ids] - 1L) + t
        id   <- c(id, ids)
        wt_c <- c(wt_c, wts)
        
        # Actualizar ventanas de no-susceptibilidad (embarazo + postparto)
        blk <- as.integer(round(9 + delta)) - 1L
        phi_i[ids] <- lapply(phi_i[ids], function(x){
          x[t: min(length(x), t + blk)] <- NA
          x
        })
      }
    }
  }
  
  # --- 5. Construir Trayectorias (Output) ---
  
  # Convertir meses a años
  age_d_yrs <- wt_d / 12
  age_m_yrs <- wt_m / 12
  
  # Si muere antes de casarse, la edad al matrimonio es NA
  age_m_yrs[age_d_yrs < age_m_yrs] <- NA
  
  # A. Datos base 
  pop_data <- data.frame(
    id = 1:ini_c,
    age_death = age_d_yrs,      
    age_marriage = age_m_yrs    
  )
  
  # B. Datos de Nacimientos 
  if (length(wt_c) > 0) {
    birth_data <- data.frame(
      id = id,
      age_birth = (wt_c + 9) / 12
    )
    
    # Ordenar y generar paridad (1er hijo, 2do hijo...)
    birth_data <- birth_data[order(birth_data$id, birth_data$age_birth), ]
    birth_data$parity <- ave(birth_data$id, birth_data$id, FUN = seq_along)
    
    # Transformar a formato ancho (una columna por hijo)
    births_wide <- reshape(
      birth_data, 
      idvar = "id", 
      timevar = "parity", 
      direction = "wide", 
      sep = "_"
    )
    
    # Unir todo
    final_trajectories <- merge(pop_data, births_wide, by = "id", all.x = TRUE)
    
  } else {
    final_trajectories <- pop_data
  }
  
  return(final_trajectories)
}


# --- EJECUCIÓN Y VALIDACIÓN ---

# 1. Simular 
sim_n <- 25000
datos_sim <- sim_t(ini_c = sim_n, 
                   mx_ages = mort$edad, 
                   mx_rates = mort$mx)


# -- Ejercicio: Describir en detalle los datos simulados





# --- Ejercicio A: Validación de Mortalidad ---

# Definir edades a analizar 
edades <- mort$edad 

# Calcular Muertes (Numerador)
muertes_x <- table(factor(floor(datos_sim$age_death), levels = edades))

# Calcular Exposición (Denominador) 
exposure_x <- vapply(edades, function(a) {
  # Tiempo vivido en el intervalo [a, a+1)
  tiempos <- pmin(1, pmax(0, datos_sim$age_death - a))
  sum(tiempos)
}, numeric(1))


# Ejercicio: Describir el cálculo de la exposición al riesgo.




# Calcular Tasas Simuladas (mx_sim)
mx_sim <- # completar

# Graficar Comparación
plot(edades, mort$mx, type = "l", col = "red", lwd = 2, log = "y",
     main = "Validación Mortalidad: Tasas Observadas vs. Simuladas",
     ylab = "Tasa de Mortalidad (mx) - Escala Log", xlab = "Edad")
points(edades, mx_sim, col = "blue", pch = 19, cex = 0.6)
legend("bottomright", legend = c("Input (Teórico)", "Simulado (Microdatos)"),
       col = c("red", "blue"), lty = c(1, NA), pch = c(NA, 19))


# Describir lo que se observa en el gráfico




# --- Ejercicio B: Validación de Fecundidad ---

# 1. Numerador: Nacimientos por edad
cols_nacimientos <- grep("age_birth", names(datos_sim))
todas_edades_nacimiento <- unlist(datos_sim[, cols_nacimientos])
todas_edades_nacimiento <- todas_edades_nacimiento[!is.na(todas_edades_nacimiento)]

# Definimos el rango de interés basado explícitamente en el archivo de input 'fec'
rango_edades_fec <- fec$edad  # Asumiendo que la columna se llama 'age' o 'edad'

# Tabulamos usando los niveles exactos del archivo input
nacimientos_x <- table(factor(floor(todas_edades_nacimiento), levels = rango_edades_fec))

# 2. Denominador: Exposición
# Asignamos nombres al vector exposure_x para evitar confusión de índices
names(exposure_x) <- as.character(edades)

# Extraemos la exposición usando los NOMBRES (edades), no posiciones
exposure_fec <- exposure_x[rango_edades_fec+1]

# 3. Tasas Simuladas
fx_sim <- # completar


  
  
# 4. Graficar
plot(fec$edad, fec$fx, type = "l", col = "red", lwd = 2,
     main = "Validación Fecundidad: Tasas Observadas vs. Simuladas",
     ylab = "Tasa Específica de Fecundidad (fx)", xlab = "Edad")
points(rango_edades_fec, fx_sim, col = "blue", pch = 19, cex = 0.6)
legend("topright", legend = c("Input (Observado)", "Simulado (Microdatos)"),
       col = c("red", "blue"), lty = c(1, NA), pch = c(NA, 19))


# Describir lo que se observa en el gráfico






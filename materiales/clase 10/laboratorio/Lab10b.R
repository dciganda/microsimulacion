################################################################################
# Microsimulación - Licenciatura en Estadística                                #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de tiempo a la muerte                                # 
# 19 de Noviembre de 2025                                                       #   
################################################################################

# El objetivo de este módulo es simular tiempos de espera a la muerte.
# Para esto partimos de unas tasas específicas de mortalidad.
# Se asume que el riesgo de muerte es *constante* en cada intervalo (edades)
# Bajo este supuesto, las tasas de ocurrencia/exposición representan 
# el riesgo del evento. Es decir que el vector de tasas específicas
# representa la función de riesgo del evento.

###############
#    DATA     #
###############
# Cargamos las tasas de mortalidad por edad para una cohorte
mort <- read.csv(file.path("datos","mx.csv"))
plot(mort)

# Estas tasas van a representar nuestra función de riesgo constante a intervalos para 
# las variables aleatorias "Tiempo al Fallecimiento"  

###########################################
#    SIMULACION DE TIEMPOS DE ESPERA      #
###########################################
# Comenzamos por definir los inputs para el procedimiento.
# Definimos el limite superior e inferior de los intervalos
# esto es importante si queremos llegar a una función que 
# acepte intervalos de diferentes largos

# intervalos
edades <- mort$edad            
inf <- edades
sup <- inf + c(diff(edades), diff(edades)[length(diff(edades))])
lambda <- mort$h

# función de riesgo por intervalos
# Describir que hacen cada uno de los pasos
h.pw <- function(t, inf, sup, lambda){
  
  lower_int <- (t-inf)>=0 # genera un vector con TRUE en los limites inferiores de los intervalos hasta el que incluye t 
  upper_int <- (t-sup)<0 # genera un vector con FALSE en los limites superiores de los intervalos hasta el que incluye t
  indicator <- lower_int * upper_int # identifica el intervalo relevante
  
  return(max(lambda * indicator)) # multiplica por 1 el lambda relevante y el resto por 0 - max devuelve el lambda relevante 
  
}

# Ejemplo
h.pw(t=100, inf, sup, lambda)

# Que devuelve la función en este caso?


# función de riesgo acumulado
# Describir cada uno de los pasos
H.pw <- function(t, inf, sup, lambda){  
  
  p1 <-  pmax(t-inf, 0) # 
  p2 <-  pmin(p1, sup-inf) #
  
  return(sum(lambda*p2)) # devuelve el riesgo acumulado hasta t
  
}

# Ejemplo
H.pw(t=101, inf, sup, lambda)

# Que devuelve la función en este caso?

# plot
x <- min(inf):max(sup) # 102 valores porque es el acumulado hasta edad exacta x (incluye 0 al incio)
H <- rep(NA, length(x))

for (i in 1:length(x)){
  H[i] <- H.pw(x[i], inf, sup, lambda)
}
H
plot(x, H, type="l", lwd=3, col=2)

# Graficar la función de supervivencia
S <- exp(-H)
plot(x, S, typ="l",lwd=3, col=2)

# Describir lo que se observa en el gráfico


# Ahora que tenemos nuestra función de riesgo acumulado, necesitamos
# definir la función para la cual vamos a encontrar la raiz 

f <- function(t, inf, sup, lambda, u){
  
  res <- H.pw(t, inf, sup, lambda) + log(u)
  
  return(res)
}

# Ahora definimos la función que genera los tiempos de espera al evento
# Describir los pasos en la función:

root <- function(n, inf, sup, lambda){
  
  u <- runif(n) # Genera los nros aleatorios de la distribución uniforme
  times <- rep(NA, n) 
  
  for(i in 1:n){
    result <- uniroot(f, interval=c(min(inf), max(sup)),
                      u=u[i], inf=inf, sup=sup, lambda=lambda) # encuentre la raiz de la funcion "f"
    times[i] <- result$root
  }
  return(times)
}

# Ahora generamos 10.000 tiempos de espera a la muerte y los guardamos
# en t
t <- root(10^4, inf, sup, lambda) #


# Para asegurarnos de que los resultados obtenidos en los pasos anteriores 
# son correctos, vamos a calcular una función de supervivencia a partir de los
# datos simulados y la vamos a comparar con la curva "teórica"

# Para esto vamos a usar el paquete "survival" que tiene una función "survfit" que
# nos va a calcular la función de supervivencia utilizando el estimador no paramétrico
# Kaplan-Meier de la función de Supervivencia

# Cargamos el paquete 
library(survival)

# survfit() necesita un objeto de tipo survival que se crea con la función
# Surv(). Esta función toma los tiempos y un indicador que indica cuando 
# hay evento = 1, o cuando no se observa el evento = 0
# En nuestro caso estamos asumiendo todos las observaciones experimentaron el evento
# es decir, no hay casos truncados.
eventos <- t < Inf

# Calculamos S
survival_fit <- survfit(Surv(t, eventos)~1)
# Extraemos los objetos necesarios para simular
sim_survival <- with(survival_fit, data.frame(time, surv))

# Graficamos
plot(sim_survival, xlab="t", ylab="S(t)", xlim = c(0,101))

# Comparamos con la función de supervivencia teórica
lines(x, exp(-H), lwd=3, col=2, lty=2)

# Que se observa en el gráfico?



# Ejercicio:

# 2) Crear una función "ste" con argumentos (n, edades, lambda) para simular los
#    tiempos de espera a un evento a partir de una función de riesgo.
#    Agregar un argumento Haz que cuando T devuelva la función de riesgo acumulado H 
#    además de las t.


# Creamos una función
ste <- function(n, edades, lambda, Haz = F){
  
  inf <- seq(0, length(edades)-1,1)
  sup <- seq(1, length(edades))

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
  
  f <- function(t, inf, sup, lambda, u){
    res <- H.pw(t, inf, sup, lambda) + log(u)
    return(res)
  }
  
  root <- function(n, inf, sup, lambda){
    u <- runif(n) 
    times <- rep(NA, n) 
    
    for(i in 1:n){
      result <- uniroot(f, interval=c(0, length(lambda)),
                        u=u[i], inf=inf, sup=sup, lambda=lambda) 
      times[i] <- result$root
    }
    return(times)
  }

  t <- root(n, inf, sup, lambda)
  

  if(Haz){ return(list(t, H))}else{ return(t) }
  
}


####################
#    VALIDACIÓN    #
####################
n <- 10000
mort <- read.csv(file.path("datos","mx.csv"))
te <- ste(n, edades = mort$edad, lambda = mort$h, Haz = T)
eventos <- te[[1]] < Inf 
H <- te[[2]] 
plot(survfit(Surv(te[[1]], eventos)~1), xlab="t", ylab="S(t)")
lines(mort$edad, exp(-H), lwd=3, col=2, lty=2)

fert <- read.csv(file.path("datos","fx.csv"))
te <- ste(n, edades = fert$edad, lambda = fert$h, Haz = T)
eventos <- te[[1]] < Inf 
H <- te[[2]]
plot(survfit(Surv(te[[1]], eventos)~1), xlab="t", ylab="S(t)")
lines(fert$edad-15, exp(-H), lwd=3, col=2, lty=2)



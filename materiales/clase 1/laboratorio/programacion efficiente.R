##############################################################################
# Microsimulación - 2025                                                     #
# Daniel Ciganda                                                             #
# LABORATORIO: PROGRAMACION EFICIENTE EN R                                   #
##############################################################################

# Objetivo: Este laboratorio introduce técnicas fundamentales para escribir
# código eficiente en R. Cubriremos vectorización, programación en paralelo y
# perfilamiento de código (profiling). El objetivo es poder identificar cuellos
# de botella en el código y aplicar estrategias para que nuestras simulaciones
# se ejecuten más rápido.

# -------------------------------------------------------------------------- #
#             1. VECTORIZACIÓN: EVITANDO LOOPS INEFICIENTES                  #
# -------------------------------------------------------------------------- #

# Una de las claves para la programación eficiente en R es usar operaciones
# vectorizadas en lugar de bucles. R es un lenguaje interpretado (es decir,
# el código se traduce a instrucciones de máquina línea por línea al
# ejecutarse), y los bucles (como for) suelen ser lentos porque cada
# iteración se procesa por separado. Las operaciones vectorizadas, en cambio,
# reemplazan el bucle en R por un bucle mucho más eficiente que se ejecuta a
# bajo nivel en código precompilado y altamente optimizado (a menudo escrito
# en C o Fortran).

# --- Ejemplo 1: Suma de dos vectores ---

# Este ejemplo muestra el concepto de vectorización en su forma más pura.
# Sumaremos dos vectores elemento por elemento. El bucle `for` nos muestra
# lo que ocurre conceptualmente, paso a paso. La versión vectorizada hace
# exactamente lo mismo, pero la iteración ocurre a bajo nivel en código C
# precompilado, lo que es órdenes de magnitud más rápido.

# 1. Crear dos vectores numéricos grandes.
set.seed(101)
vector_a <- rnorm(10^7)
vector_b <- rnorm(10^7)

# 2. Definir una función ineficiente usando un bucle `for`.
# Esta función recorre cada índice `i`, toma el elemento `i` de cada vector,
# los suma, y asigna el resultado al elemento `i` del vector de salida.
# Esto es lo que significa "operación elemento a elemento".
sumar_vectores_loop <- function(v1, v2) {
  resultado <- numeric(length(v1)) # Pre-alocar el vector de salida
  for (i in 1:length(v1)) {
    resultado[i] <- v1[i] + v2[i]
  }
  return(resultado)
}

# 3. Medir el tiempo de ejecución de la función con el bucle.
print("Sumando vectores con un bucle for:")
system.time(resultado_loop <- sumar_vectores_loop(vector_a, vector_b))

# 4. Usar una operación vectorizada.
# Al escribir `vector_a + vector_b`, R entiende que debe realizar la misma
# operación "elemento a elemento" que vimos en el bucle. La diferencia es que
# esta simple línea ejecuta un bucle altamente optimizado en C.
print("Sumando vectores de forma vectorizada:")
system.time(resultado_vec <- vector_a + vector_b)

# Comprobamos que los resultados son idénticos.
print(paste("¿Son los resultados idénticos? ",
            all.equal(resultado_loop, resultado_vec)))


# --- Ejemplo 2: Cálculo de un puntaje promedio en una encuesta ---

# Un caso de uso muy común es calcular un puntaje o índice a partir de varias
# columnas en un set de datos. Imaginemos que tenemos datos de una encuesta
# y queremos calcular el puntaje promedio para cada encuestado.

# Este ejercicio compara el cálculo del promedio por fila usando un bucle `for`
# vs. la función vectorizada `rowMeans()`.

# Crear un data frame simulando respuestas a una encuesta.
# Cada fila es un encuestado, cada columna es un ítem de la encuesta (con
# puntajes 1-5)
set.seed(123)
n_encuestados <- 50000
n_items <- 10
datos_encuesta <- as.data.frame(
  matrix(sample(1:5, n_encuestados * n_items, replace = TRUE),
         nrow = n_encuestados)
)
colnames(datos_encuesta) <- paste0("item_", 1:n_items)

# Definir una función ineficiente usando un bucle `for`.
# Esta función itera a través de cada fila, calcula la media y la guarda.
calcular_puntaje_loop <- function(df) {
  puntajes <- numeric(nrow(df)) # Pre-alocar 
  for (i in 1:nrow(df)) {
    puntajes[i] <- mean(as.numeric(df[i, ]))
  }
  return(puntajes)
}

# Medir el tiempo de ejecución de la función con el bucle.
# system.time() mide cuánto tiempo tarda en ejecutarse una expresión.
print("Calculando puntajes con un bucle for:")
system.time(puntajes_loop <- calcular_puntaje_loop(datos_encuesta))

# Usar una función vectorizada (optimizada para matrices y data frames).
# `rowMeans()` está diseñada específicamente para esta tarea y es muy rápida.
print("Calculando puntajes con rowMeans():")
system.time(puntajes_vec <- rowMeans(datos_encuesta))

# Comprobamos que los resultados son idénticos.
# all.equal() es más seguro que `==` para comparar números de punto flotante.
print(paste("¿Son los resultados idénticos? ",
            all.equal(puntajes_loop, puntajes_vec)))


# -------------------------------------------------------------------------- #
#           2. PROGRAMACIÓN EN PARALELO: USANDO MÚLTIPLES NÚCLEOS            #
# -------------------------------------------------------------------------- #

# La mayoría de las computadoras modernas tienen múltiples núcleos (cores) en
# su CPU. Por defecto, R solo usa uno. La programación en paralelo te permite
# dividir una tarea entre múltiples núcleos para obtener resultados más
# rápido. Esto es especialmente útil para simulaciones donde se ejecuta el mismo
# proceso muchas veces.

# Usaremos el paquete **`parallel`**, que es parte de la distribución base de R.

# --- Ejemplo: Un `lapply` en paralelo ---

# Este script demuestra cómo ejecutar una tarea en paralelo para acelerarla.
# Compararemos un `lapply` secuencial con un `parLapply` en paralelo.

# 1. Cargar el paquete `parallel`.
library(parallel)

# 2. Detectar el número de núcleos de CPU disponibles en tu máquina.
n_cores <- detectCores()
print(paste("Esta máquina tiene", n_cores, "núcleos."))

# 3. Definir una función que tarda un tiempo en ejecutarse.
# Usamos Sys.sleep() para simular un cálculo largo.
funcion_compleja <- function(x) {
  Sys.sleep(0.1)
  return(x^2)
}

# 4. Crear una lista de entradas para nuestra función.
entradas <- 1:100

# 5. Ejecutar la función de forma secuencial con `lapply()`.
# Registramos el tiempo de inicio y fin para medir la duración.
print("Ejecutando secuencialmente con lapply...")
tiempo_inicio_seq <- Sys.time()
resultados_seq <- lapply(entradas, funcion_compleja)
tiempo_fin_seq <- Sys.time()
duracion_seq <- tiempo_fin_seq - tiempo_inicio_seq
print(paste("Tiempo secuencial:", round(duracion_seq, 2), "segundos"))

# 6. Ejecutar la función en paralelo usando `parLapply()`.
# Para esto, primero debemos crear un "clúster" de procesos.
# Es buena práctica dejar un núcleo libre para el sistema operativo.
print("Ejecutando en paralelo con parLapply...")

# Paso 6.1: Crear el clúster.
cl <- makeCluster(n_cores - 1)

# Paso 6.2: Ejecutar parLapply en el clúster.
tiempo_inicio_par <- Sys.time()
resultados_par <- parLapply(cl, entradas, funcion_compleja)
tiempo_fin_par <- Sys.time()

# Paso 6.3: ¡Muy importante! Detener el clúster cuando termines.
# Deno hacerlo, los procesos seguirán corriendo en segundo plano.
stopCluster(cl)

duracion_par <- tiempo_fin_par - tiempo_inicio_par
print(paste("Tiempo en paralelo:", round(duracion_par, 2), "segundos"))

# La ganancia de tiempo ideal sería igual al número de núcleos que usamos.
# En la práctica, es un poco menor debido al costo de coordinar los procesos (overhead).
print(paste("Reducción de tiempo:", round(as.numeric(duracion_seq) / as.numeric(duracion_par), 1),
            "veces. Idealmente, se acercaría a", n_cores - 1, "veces."))

# ¿Por qué la ganancia no es perfecta?
# Existe un costo computacional (conocido como "overhead") asociado a la
# creación de los procesos paralelos, la división de la tarea entre ellos y
# la recolección de los resultados. Este costo fijo hace que la ganancia
# real sea ligeramente menor que el ideal teórico.


# -------------------------------------------------------------------------- #
#             3. PERFILAMIENTO: ENCONTRANDO CUELLOS DE BOTELLA               #
# -------------------------------------------------------------------------- #

# Antes de poder optimizar tu código, necesitas saber qué partes son lentas.
# Para esto sirve el **perfilamiento** (profiling). R tiene un perfilador
# incorporado, `Rprof()`, que te ayuda a identificar estos "cuellos de botella".

# La función `Rprof()` inicia el perfilador, que registra la pila de llamadas
# a funciones a intervalos regulares. `Rprof(NULL)` lo detiene. La salida se
# analiza con `summaryRprof()`.

# --- Ejemplo: Perfilando una simulación de caminata aleatoria ---

# Un error común en R es "hacer crecer" un objeto (como un vector o data frame)
# dentro de un bucle. Esto es muy ineficiente porque R tiene que reasignar
# memoria en cada iteración. Escribamos una función para una caminata
# aleatoria que comete este error y usemos el perfilador para detectarlo.

# 1. Definir una función ineficiente para una simulación de caminata aleatoria.
# Esta función hace crecer el vector 'path' dentro del bucle `for` usando `c()`,
# que es un cuello de botella de rendimiento clásico en R.
caminata_aleatoria_ineficiente <- function(n_pasos) {
  path <- 0 # Empezar en la posición 0
  for (i in 1:n_pasos) {
    paso <- sample(c(-1, 1), 1)
    # Ineficiente: hacer crecer el vector en cada paso
    path <- c(path, path[i] + paso)
  }
  return(path)
}

# 2. Iniciar el perfilador y ejecutar la función ineficiente.
# Rprof() escribe los datos de perfilamiento en el archivo especificado.
Rprof("perfil_caminata.out")
datos_caminata <- caminata_aleatoria_ineficiente(20000)
# Detener el perfilador.
Rprof(NULL)

# 3. Analizar la salida del perfilador.
summaryRprof("perfil_caminata.out")

# --- ¿Cómo leer la salida de summaryRprof()? ---
# La salida tiene dos secciones principales, `$by.self` y `$by.total`,
# y dos líneas informativas al final.

# $by.self:
#   - Muestra el tiempo consumido DENTRO de cada función, sin contar el
#     tiempo de otras funciones que estas hayan llamado.
#   - Es útil para encontrar funciones que son lentas por sí mismas.
#   - Columnas:
#     - self.time: Segundos totales que el código pasó dentro de esa función.
#     - self.pct: Porcentaje del tiempo total que representa `self.time`.

# $by.total:
#   - Muestra el tiempo consumido por una función Y TODAS LAS FUNCIONES
#     que fueron llamadas por ella.
#   - Es útil para encontrar los "culpables" de alto nivel. Una función
#     puede no ser lenta por sí misma (`self.time` bajo), pero sí llamar a
#     otras funciones que lo son (`total.time` alto).
#   - Columnas:
#     - total.time: Segundos totales desde que se entró a la función hasta
#       que se salió.
#     - total.pct: Porcentaje del tiempo total que representa `total.time`.
#     - self.time y self.pct: Se repiten para facilitar la comparación.


# En nuestro ejemplo, veremos que la función `c` tiene un `self.time` muy
# alto. Esto nos dice que la operación de concatenar (`c()`) es el cuello de
# botella principal, ya que es lenta por sí misma.

# $sample.interval:
#   - El intervalo en segundos con el que el perfilador tomó "fotos" de la
#     pila de llamadas (por defecto 0.02 segundos).

# $sampling.time:
#   - El tiempo total en segundos que el perfilador estuvo activo.


# -------------------------------------------------------------------------- #
#                           EJERCICIOS PRÁCTICOS                             #
# -------------------------------------------------------------------------- #

# --- Ejercicio 1: Paralelización ---
# Imagina que tienes que correr una simulación simple muchas veces para
# estimar una probabilidad. La siguiente función simula el lanzamiento de
# 10 dados y devuelve TRUE si la suma es mayor a 40, y FALSE en caso contrario.
# Tarea: Usa `parLapply` para correr esta simulación 10,000 veces en paralelo
# y luego calcula la probabilidad estimada (el promedio de los resultados).

simular_dados <- function(id_simulacion) {
  # El argumento es solo para que lapply/parLapply pueda iterar. No se usa.
  dados <- sample(1:6, 10, replace = TRUE)
  return(sum(dados) > 40)
}

# Corre la simulación 10,000 veces de forma secuencial
print("Ejercicio 3: Corriendo simulación secuencialmente")
system.time(resultados_seq_ej3 <- lapply(1:10000, simular_dados))
prob_seq <- mean(unlist(resultados_seq_ej3))
print(paste("Probabilidad estimada (secuencial):", prob_seq))

# Ahora, haz lo mismo en paralelo


# Ejercicio 2: —-- Caminata aleatoria: perfilar, optimizar y vectorizar ---
#
# Tomar las dos implementaciones de la cainata aleatoria que disctimos en clase.
# PERFILARLAS para identificar los cuello de botella.
# Proponer y probar una versión VECTORIZADA equivalente.




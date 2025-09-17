schelling <- function(alikePref, density, years = 100, citySize = 51, parameters = F){
  
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
  unHappyMonitor <- c()
  similarMonitor <- c()
  
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
    unHappyMonitor <- c(unHappyMonitor,
                        length(whoUnhappy)/(length(which(!unhappy)) + length(whoUnhappy)))
    similarMonitor <- c(similarMonitor, mean(similar, na.rm = T))
    
    # Moviendo Agentes Insatisfechos  
    randUnhappy <- whoUnhappy[sample.int(length(whoUnhappy))]
    empty <- which(city == 0)
    
    for (i in randUnhappy) {
      dest <- sample.int(length(empty), 1)
      city[empty[dest]] <- city[i]
      city[i] <- 0
      empty[dest] <- i
    }
    
    #Sys.sleep(0.1)
    # par(mfrow = c(1,3))
    # side <- dim(city)[1]
    # x <- rep(1:side, side)
    # y <- rep(1:side, each = side)
    # plot(x , y, axes = F, xlab = "", ylab = "",col = city, main = paste("Year", t),
    #      pch = 19, cex = 40 / side)
    # 
    # plot(runif(years,0,1), ylab = "Proportion Unhappy",
    #      xlab = "Years", col = "white", ylim = c(0,1))
    # lines(unHappyMonitor, oma = c(0, 0, 2, 0), col = "red")
    # 
    # plot(runif(years,0,1), ylab = "Proportion Similar",
    #      xlab = "Years", col = "white", ylim = c(0,1))
    # lines(similarMonitor, oma = c(0, 0, 2, 0), col = "red")
    

    if (identical(lastCity, city)) { 
      cat(paste("\n Everybody Happy after", t, "years"))
      break 
      }
    
    if (t == years){
      cat("\n End Iterations")
    }
    
  }
  
  cat(paste("\n Proportion Similar",similarMonitor[length(similarMonitor)]))
  if(parameters){
    cat(paste("\n Preferences:",alikePref, "\n Density:", density))
    
  }
  return(invisible(similarMonitor[length(similarMonitor)]))
  
  
}

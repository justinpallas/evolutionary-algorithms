# Definieren der Zielfunktionen
g <- function(x) x
h <- function(x) sin(x)
i <- function(x) x * sin(x)
j <- function(x) 2 + cos(x) + sin(2*x)

gradient_rise <- function(f) {
  # Festlegen der Schrittweiten eta
  etas <- c(0.01, 0.1, 0.25)
  
  # Festlegen der Anzahl von Schritten
  num_steps <- 100
  
  # Festlegen verschiedener Startpunkte
  starts <- c(-10, 0, 10)
  
  # Iterieren über alle Schrittweiten
  for (eta in etas) {
    
    # Iterieren über alle Startpunkte
    for (start in starts) {
      xk <- start
      
      # Speichern der Fitnesswerte in einem Vektor
      fitness_values <- rep(NA, num_steps)
      
      # Anwenden des Gradientenaufstiegsverfahrens
      for (i in 1:num_steps) {
        # Berechnen der Ableitung der Zielfunktion an der Stelle xk
        df <- (f(xk + eta) - f(xk - eta)) / (2 * eta)
        # Berechnen des neuen Punkts mit dem Gradientenaufstieg
        xk <- xk + eta * df
        
        # Speichern des aktuellen Fitnesswerts
        fitness_values[i] <- f(xk)
      }
      # Plotten der Fitnesswerte
      plot(fitness_values, type = "l", main = paste("Fitnessverlauf für\n",deparse(substitute(f)),"(x) =", deparse(body(f)), "\nmit eta =", eta,"\nund dem Startpunkt", start),
           xlab = "Schritte", ylab = "Fitnesswert", cex.main= 0.9)
  }
  }
}

pdf(file="plots_g(x).pdf")
par(mfrow = c(3,3))
gradient_rise(g)
dev.off()

pdf(file="plots_h(x).pdf")
par(mfrow = c(3,3))
gradient_rise(h)
dev.off()

pdf(file="plots_i(x).pdf")
par(mfrow = c(3,3))
gradient_rise(i)
dev.off()

pdf(file="plots_j(x).pdf")
par(mfrow = c(3,3), oma=c(2,1,5,1))
gradient_rise(j)
dev.off()

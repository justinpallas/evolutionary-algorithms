pdf(file = "test.pdf")

par(mfrow = c(3,3))

# Definieren der Zielfunktionen
g <- function(x) x
h <- function(x) sin(x)
i <- function(x) x * sin(x)
j <- function(x) 2 + cos(x) + sin(2*x)

# Festlegen der Schrittweiten eta
etas <- c(0.01, 0.1, 0.25)

# Festlegen der Anzahl von Schritten
num_steps <- 100

# Iterieren über alle Funktionen und Schrittweiten
for (f in c(g, h, i, j)) {
  for (eta in etas) {
    # Festlegen des Startpunkts
    xk <- runif(1, -10, 10)
    
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
    plot(fitness_values, type = "l", main = paste("Fitnessverlauf für f(x) =", deparse(substitute(f)), "und eta =", eta),
         xlab = "Schritte", ylab = "Fitnesswert")
  }
}


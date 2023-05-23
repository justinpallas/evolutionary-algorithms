# Definieren der Fitnessfunktionen
f1 <- function(x) x
f2 <- function(x) sin(x)
f3 <- function(x) x * sin(x)
f4 <- function(x) 2 + cos(x) + sin(2*x)

# Definieren der EA-Parameter
pop_size <- 10
num_parents <- 10
num_klones <- 10
mutation_rate <- 0.5
delta <- 0.1
num_generations <- 100

# Definieren der Mutationsfunktionen
fixed_delta_mutation <- function(x, delta) {
  if(runif(1) < 0.5) {
    x + delta
  } else {
    x - delta
  }
}

gaussian_mutation <- function(x, sigma) {
  rnorm(1, x, sigma)
}

# Definieren der EA-Funktion
evolutionary_algorithm <- function(fitness_function, mutation_function, mutation_param) {
  
  b_fitnesses <- rep(NA, num_generations)
  b_individuals <- rep(NA, num_generations)

  # Generieren der ersten Population
  population <- runif(pop_size, 0, 10)
  
  # Iterieren über alle Generationen
  for (gen in 1:num_generations) {
    # Erzeugen von Klonen durch Mutation der Eltern
    klones <- sapply(population, function(x) mutation_function(x, mutation_param))
    
    # Verbinden von Eltern und Klonen
    all_individuals <- c(population, klones)
    
    # Berechnen der Fitnesswerte
    fitness_values <- sapply(all_individuals, fitness_function)
    
    # Sortieren nach Fitnesswert
    sorted_indices <- order(fitness_values, decreasing = TRUE)
    
    # Selektion der fittesten Individuen
    selected_indices <- sorted_indices[1:num_parents]
    population <- all_individuals[selected_indices]
    
    # Ausgabe des fittesten Individuums
    best_fitness <- max(fitness_values)
    b_fitnesses[gen] <- best_fitness
    best_individual <- all_individuals[which.max(fitness_values)]
    b_individuals[gen] <- best_individual
    print(paste("Generation:", gen, "Bestes Individuum:", best_individual, "Fitness:", best_fitness))
  }
  if (type == "delta") {
  plot(b_fitnesses, type = "l", main = paste("Fitnessverlauf für\ny =", deparse(body(fitness_function)), "\nmit fixem delta =", mutation_param),
       xlab = "Generationen", ylab = "Fitnesswert", cex.main= 0.9)
  } else if (type == "gaussian") {
    plot(b_fitnesses, type = "l", main = paste("Fitnessverlauf für\ny =", deparse(body(fitness_function)), "\nmit Gauß-Mutation und sigma =", mutation_param),
         xlab = "Generationen", ylab = "Fitnesswert", cex.main= 0.9)
  }
}

# Testen der Fitnessfunktionen mit fester Delta-Mutation
type ="delta"
pdf(file="fixed_delta_mutation.pdf")
par(mfrow=c(4,3), mar= c(2, 2, 4, 2), oma=c(1,1,1,1))
for (f in list(f1, f2, f3, f4)) {
  for (delta in c(0.01, 0.1, 0.25)) {
    cat("Fitnessfunktion: y =", deparse(body(f)), "und Delta:", delta, "\n")
    evolutionary_algorithm(f, fixed_delta_mutation, delta)
  }
}
dev.off()

# Testen der Fitnessfunktionen mit Gauss-Mutation
type = "gaussian"
pdf(file="gaussian_mutation.pdf")
par(mfrow=c(4,3), mar= c(2, 2, 4, 2), oma=c(1,1,1,1))
for (f in list(f1, f2, f3, f4)) {
  for (sigma in c(0.01, 0.1, 0.25)) {
    cat("Fitnessfunktion: y =", deparse(body(f)), "und Sigma:", sigma, "\n")
    evolutionary_algorithm(f, gaussian_mutation, sigma)
  }
}
dev.off()

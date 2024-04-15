# The function of this script file is to create a function that standardized
# any numeric vector.

std <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  # Standardizing x
  x <- (x - mean(x)) / sd(x)

  # Return standardized value
  return(x)
}
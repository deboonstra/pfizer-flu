# The function of this script is to extract the trend, seasonal, and anomaly and
# error components based on the observed series and state-space modeling.

# Creating extractation function ####
signal_extract <- function(data, object) {

  # Parameter checking ####
  if (class(data) != "ts") {
    stop("data must be a ts object.")
  }

  if (class(object) != "ss") {
    stop("object must be a ss object produced by ss.")
  }

  # Pulling latent processes ####

  ## Trend
  tsm <- stats::ts(
    object$ks$Xs[1, , ], start = stats::tsp(data)[1], freq = stats::tsp(data)[3]
  )
  ## Seasonal
  ssm <- stats::ts(
    object$ks$Xs[2, , ], start = stats::tsp(data)[1], freq = stats::tsp(data)[3]
  )
  ## Anomaly and error
  asm <- stats::ts(
    data - tsm - ssm, start = stats::tsp(data)[1], freq = stats::tsp(data)[3]
  )

  # Returning object ####
  return(
    structure(
      .Data = list(obs = data, tsm = tsm, ssm = ssm, asm = asm),
      class = "ss.extract"
    )
  )
}

## Creating a plotting function ####
signal_extract_plot <- function(
  data, ylim = c(-3, 3), ylab = "Incidence per 100,000", xlab = "Time",
  main = "", oma = c(2, 2, 0, 0), ...
) {

  # Parameter checking ####
  if (class(data) != "ss.extract") {
    stop("data must be a ss.extract object produced by signal_extract")
  }

  # Plotting ####
  graphics::par(mfrow = c(4, 1), oma = oma)
  ## Observations
  astsa::tsplot(
    x = data$obs, main = "Observed series", ylab = "", xlab = "",
    ylim = ylim, ...
  )
  ## Trend
  astsa::tsplot(
    x = data$tsm, main = "Trend component", ylab = "", xlab = "",
    ylim = ylim, ...
  )
  ## Seasonal
  astsa::tsplot(
    x = data$ssm, main = "Seasonal component", ylab = "", xlab = "",
    ylim = ylim, ...
  )
  ## Anomaly and error
  astsa::tsplot(
    x = data$asm, main = "Local component", ylab = "", xlab = "", ...
  )
  ## Adding axis labels
  graphics::mtext(text = ylab, side = 2, outer = TRUE, cex = 1, las = 0)
  graphics::mtext(text = xlab, side = 1, outer = TRUE, cex = 1)
  if (oma[3] > 0) {
    graphics::mtext(text = main, side = 3, outer = TRUE, cex = 1)
  }
  par(mfrow = c(1, 1))
}
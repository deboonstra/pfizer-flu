# The function of this script file is to create a function that plots the
# illness rates throughout the time series and monthly averages.
plot_rates <- function(
  data, ccs_code, log = FALSE, center = FALSE, type = "o", title = TRUE
) {
  # checking parameter values ####
  if (!("monthly.rates" %in% class(data))) {
    stop("data must be a monthly.rates data object produced by read_all.")
  }

  if (!is.numeric(ccs_code)) {
    stop("ccs_code must be numeric.")
  }

  if (!is.logical(log)) {
    stop("log must be a logial denoting the log(rate) should be used.")
  }

  if (!is.logical(center)) {
    stop("center must be a logical denoting if the rates should be centered.")
  }

  if (!is.logical(title)) {
    stop(
      paste(
        "title must be a logical denoting if an automatic title should",
        "be created."
      )
    )
  }
  # Pulling CCS data of interest ####
  rates <- data[data$ccs_code == ccs_code, ]
  rates <- rates[order(rates$year, rates$month), ]

  # Determining response by log parameter
  if (log) {
    y <- log(rates$rate_avg_enroll)
    ylab <- "log(Rate)"
  } else {
    y <- rates$rate_avg_enroll
    ylab <- "Rate"
  }

  # Centering
  if (center) {
    y <- y - mean(y)
  } else {
    y <- y
  }

  # Plotting

  ## Basic plot ####
  if (title) {
    mtitle <- paste0("CCS code: ", ccs_code, ", ", rates$ccs_category[1])
  } else {
    mtitle <- ""
  }

  graphics::plot(
    x = seq_along(y),
    y = y,
    type = type,
    xaxt = "n",
    xlab = "Year", ylab = ylab,
    bty = "n",
    main = mtitle
  )
  graphics::axis(
    side = 1,
    at = seq(
      from = 1,
      to = nrow(rates),
      by = 12,
    ),
    labels = seq(
      from = min(rates$year),
      to = max(rates$year),
      by = 1
    ),
    las = 2
  )

  ## Adding zero line if center
  if (center) {
    abline(h = 0, lty = 3, col = "red")
  }
}
# The function in this script file is calculate the change in incidence for
# disease y given a percent (pct) reduction in disease x based on the months
# of interest for disease x. A relative incidence change is returned if
# relative is TRUE.
change_incidence <- function(
  x, y, object, months, pct, type = "raw", relative = FALSE
) {
  # Checking parameter values ####
  if (class(x) != "ts") {
    stop("x must be a ts object.")
  }

  if (class(y) != "ts") {
    stop("y must be a ts object.")
  }

  if (class(object) != "lm") {
    stop("object must be a lm object.")
  }

  if (!is.numeric(months) || min(months) < 1L || max(months) > 12L) {
    stop("months must be a vector of integers for each month of interest.")
  }

  if (!is.numeric(pct) || pct < 0 || pct > 1) {
    stop("pct must be a numeric between 0 and 1 denoting reduction percentage.")
  }

  if (!(type %in% c("raw", "log", "std", "std-log"))) {
    stop("type must be one of the following: raw, log, std, std-log.")
  }

  if (!is.logical(relative)) {
    stop("relative must be a logical denoting if relative pct change returned.")
  }

  # 1. Finding mean incidence for each months ####
  mm <- stats::cycle(x)
  mean_incidence <- rep(NA, length = length(months))
  for (l in seq_along(months)) {
    w <- which(mm == months[l])
    raw_incidence <- x[w]
    mean_incidence[l] <- mean(raw_incidence)
  }

  # 2. Maximum mean incidence ####
  f_max <- max(mean_incidence)

  # 3. Calculating target incidence in disease x ####
  f_target <- f_max * (1 - pct)

  # 4. Transforming raw incidence to type scale ####
  if (type == "std-log") {
    x_max <- (log(f_max) - mean(log(x))) / stats::sd(log(x))
    x_target <- (log(f_target) - mean(log(x))) / stats::sd(log(x))
  } else if (type == "raw") {
    x_max <- f_max
    x_target <- f_target
  } else if (type == "log") {
    x_max <- log(f_max)
    x_target <- log(f_target)
  } else if (type == "std") {
    x_max <- (f_max - mean(x)) / stats::sd(x)
    x_target <- (f_target - mean(x)) / stats::sd(x)
  }

  # 5. Predicting incidence of disease y on std-log scale ####
  y_target <- c(object$coefficients %*% c(1, x_target))
  y_max <- c(object$coefficients %*% c(1, x_max))

  # 6. Transforming predicted values to original incidence scale ####
  if (type == "std-log") {
    d_target <- exp((y_target * stats::sd(log(y))) + mean(log(y)))
    d_max <- exp((y_max * stats::sd(log(y))) + mean(log(y)))
  } else if (type == "raw") {
    d_target <- y_target
    d_max <- y_max
  } else if (type == "log") {
    d_target <- exp(y_target)
    d_max <- exp(y_target)
  } else if (type == "std") {
    d_target <- (y_target * stats::sd(y)) + mean(y)
    d_max <- (y_max * stats::sd(y)) + mean(y)
  }

  # 7. Returning change in incidence ####
  if (relative) {
    1 - (d_target / d_max)
  } else {
    d_max - d_target
  }
}
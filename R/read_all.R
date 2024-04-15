# The function of this script file is create a function that reads in all of
# the data.

read_all <- function(cd = getwd()) {
  # check parameter values
  if (!is.character(cd)) {
    stop("cd must be character value denoting the current directory.")
  }

  # Importing data ####

  ## CCS labels ####
  ccs_labels <- utils::read.csv(
    file = "./data/ccs-series/ccs_labels.csv",
    header = TRUE,
    colClasses = c("numeric", "character"),
    stringsAsFactors = FALSE
  )

  ## Monthly CCS counts ####
  ccs_monthly <- utils::read.csv(
    file = "./data/ccs-series/monthly_ccae_mdcr.csv",
    header = TRUE,
    colClasses = rep("numeric", 4),
    stringsAsFactors = FALSE
  )

  ## Daily enrollment ####
  enroll_daily <- utils::read.csv(
    file = "./data/enrollment-series/daily_enroll_all.csv",
    header = TRUE,
    colClasses = c("character", "numeric", "character"),
    stringsAsFactors = FALSE
  )

  ### Converting date character variable to R date variable
  enroll_daily$date <- as.Date(enroll_daily$date, "%Y-%m-%d")
  enroll_daily$year <- lubridate::year(enroll_daily$date)
  enroll_daily$month <- lubridate::month(enroll_daily$date)
  enroll_daily$day <- lubridate::day(enroll_daily$date)

  ### Removing medicad claims
  enroll_daily_medicaid <- subset(
    x = enroll_daily,
    subset = source == "medicaid"
  )
  enroll_daily <- subset(
    x = enroll_daily,
    subset = source != "medicaid"
  )

  ## Monthly enrollment ####
  enroll_monthly <- utils::read.csv(
    file = "./data/enrollment-series/monthly_enroll_ccae_mdcr.csv",
    header = TRUE,
    colClasses = rep("numeric", 3),
    stringsAsFactors = FALSE
  )

  ### Adding number of enrollments

  #### Getting monthly enrollment counts
  count_enroll <- stats::aggregate(
    x = total_enroll ~ month + year,
    data = enroll_daily,
    FUN = sum
  )

  #### Merging counts with monthly enrollment data
  enroll_monthly <- dplyr::arrange(.data = enroll_monthly, year, month)
  count_enroll <- dplyr::arrange(.data = count_enroll, year, month)
  enroll_monthly$count_enroll <- count_enroll$total_enroll

  ## Monthly rates ####
  ## Creating rate of illness data set
  ccs_monthly_rates <- merge(
    x = ccs_monthly, y = enroll_monthly,
    by = c("year", "month"),
    all.x = TRUE
  )

  ccs_monthly_rates <- merge(
    x = ccs_monthly_rates, y = ccs_labels,
    by = "ccs_code",
    all.x = TRUE
  )

  ## Adding rate variables
  ccs_monthly_rates <- dplyr::mutate(
    .data = ccs_monthly_rates,
    rate_avg_enroll = n / average_enroll,
    rate_count_enroll = n / count_enroll
  )

  ## Adding attribute ####
  class(ccs_monthly_rates) <- c("data.frame", "monthly.rates")

  # Output ####
  output <- list(
    ccs_labels = ccs_labels,
    ccs_monthly = ccs_monthly,
    enroll_daily = enroll_daily,
    enroll_daily_medicaid = enroll_daily_medicaid,
    enroll_monthly = enroll_monthly,
    ccs_monthly_rates = ccs_monthly_rates
  )
  return(output)
}
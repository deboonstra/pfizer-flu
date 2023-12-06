# The function of this script file is perform some basic splitting of the time
# series for specific illness possibility related to the flu.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating sub-directory to output figures ####
if (!dir.exists("./outputs/basic-time-series-split")) {
  dir.create("./outputs/basic-time-series-split")
}

# Importing data ####
data_all <- read_all()

## Creating specific data sets
ccs_monthly_rates <- data_all$ccs_monthly_rates
temp_monthly_rates <- ccs_monthly_rates

# Adjusting response for log base-e rates ####
temp_monthly_rates$log_rates <- log(temp_monthly_rates$rate_avg_enroll)

# Centering observations ####
mean_rates <- stats::aggregate(
  x = log_rates ~ ccs_code,
  data = temp_monthly_rates,
  FUN = mean
)
colnames(mean_rates) <- c("ccs_code", "mean_log_rates")

## Merging mean rates with original observations
temp_monthly_rates <- merge(
  x = temp_monthly_rates, y = mean_rates,
  by = "ccs_code",
  all.x = TRUE
)

## Centering rates
temp_monthly_rates <- dplyr::mutate(
  .data = temp_monthly_rates,
  center_log_rates = log_rates - mean_log_rates
)

# Splitting time series ####

## Flu ####

### Getting data ####
flu <- subset(x = temp_monthly_rates, subset = ccs_code == 123)
flu <- flu[order(flu$year, flu$month), ]

### Creating time-series object with time and seasonal functions
flu_ts <- stats::ts(
  data = flu$center_log_rates,
  start = c(min(flu$year), 1),
  frequency = 12
)
flu_time <- stats::time(flu_ts)
flu_month_ <- TSA::season(flu_ts)

### Global trend ####

#### Fitting model
flu_trend <- stats::lm(formula = flu_ts ~ flu_time)

### Seasonal trend ####

#### Fitting model
flu_seasonal <- stats::lm(flu_trend$residuals ~ -1 + flu_month_)

## AMI ####
ami <- subset(x = temp_monthly_rates, subset = ccs_code == 100)

### Creating time-series object with time and seasonal functions
ami_ts <- ts(
  data = ami$center_log_rates,
  start = c(min(ami$year), 1),
  frequency = 12
)
ami_time <- stats::time(ami_ts)
ami_month_ <- TSA::season(ami_ts)

### Global trend

#### Fitting model
ami_trend <- stats::lm(
  formula = ami_ts ~ splines::bs(
    x = ami_time,
    knots = c(2009, 2016, 2019),
    intercept = FALSE
  )
)

### Seasonal trend

#### Fitting model
ami_seasonal <- stats::lm(ami_trend$residuals ~ -1 + ami_month_)

## Pneumonia ####

### Getting data ####
pneumonia <- subset(x = temp_monthly_rates, subset = ccs_code == 122)
pneumonia <- pneumonia[order(pneumonia$year, pneumonia$month), ]

### Creating time-series object with time and seasonal functions
pneumonia_ts <- stats::ts(
  data = pneumonia$center_log_rates,
  start = c(min(pneumonia$year), 1),
  frequency = 12
)
pneumonia_time <- stats::time(pneumonia_ts)
pneumonia_month_ <- TSA::season(pneumonia_ts)

### Global trend ####

#### Fitting model
pneumonia_trend <- stats::lm(formula = pneumonia_ts ~ pneumonia_time)

### Seasonal trend ####

#### Fitting model
pneumonia_seasonal <- stats::lm(
  formula = pneumonia_trend$residuals ~ -1 + pneumonia_month_
)

# Exporting results ####

## Flu
flu_res <- save(
  flu_trend, flu_seasonal,
  file = "./outputs/basic-time-series-split/flu_res.rda"
)

## AMI
ami_res <- save(
  ami_trend, ami_seasonal,
  file = "./outputs/basic-time-series-split/ami_res.rda"
)

## Pneumonia
pneumonia_res <- save(
  pneumonia_trend, pneumonia_seasonal,
  file = "./outputs/basic-time-series-split/pneumonia_res.rda"
)

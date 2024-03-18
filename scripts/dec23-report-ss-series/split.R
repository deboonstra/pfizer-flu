# The function of this script file is to implement to signal extract technique
# devised by Shumway and Stoffer. Here we are looking to pull out the trend and
# seasonal components leaving the noise and anomaly combined for all diseases
# based on CCS codes. We will only be using the inpatient and outpatient
# combined ICD-9-CM data.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating output directory ####
if (!dir.exists("./outputs/dec23-report-ss-series/")) {
  dir.create("./outputs/dec23-report-ss-series/")
}

# Importing data ####
load("./data/incidence-series/monthly_incidence_cohorts.RData")

# Preparing data for analysis ####

## Pulling unweighted series from ICD-9 cohort ####
icd9 <- subset(
  x = monthly_incidence_cohorts,
  subset = population == "icd9" &
    series == "unweighted" &
    setting == "inpatient + outpatient"
)

### Transforming each series into ts objects ####
### based on setting and disease type
icd9_ts <- split(
  x = icd9,
  f = factor(icd9$ccs_code)
)
icd9_ts <- lapply(
  X = icd9_ts,
  FUN = function(x) {
    x <- x[order(x$year, x$month), ]
    x <- subset(x = x, select = incidence)
    x <- stats::ts(
      data = x,
      start = 2003, end = 2013, frequency = 12
    )
    return(x)
  }
)
names(icd9_ts) <- paste0("ccs", names(icd9_ts))

# Signal extract ####

## Defining observation equation design matrix ####
a <- cbind(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

## Defining the design matrix for the state equation
phi <- diag(0, length(a))
phi[1, 1] <- 1
phi[2, ] <- c(0, rep(-1, 11))
w <- which(a == 0)[1]
for (k in seq(w, nrow(phi))) {
  phi[k, (k - 1)] <- 1
}

## Automatically extracting components from series ####
all_models <- vector(mode = "list", length = length(icd9_ts))
names(all_models) <- names(icd9_ts)
pb <- utils::txtProgressBar(min = 0, max = length(all_models), style = 3)
for (j in seq_along(all_models)) {
  ### Fitting models
  all_models[[j]] <- ss(
    y = std(log(icd9_ts[[j]])),
    a = a,
    phi = phi,
    theta0 = c(1, 1),
    mu0 = rep(0, 12),
    sigma0 = diag(1, 12),
    control = list(),
    auto = TRUE
  )

  ### Updating progress bar
  utils::setTxtProgressBar(pb, j)
  if (j == length(all_models)) {
    close(pb)
  }
}

## Examining which series did NOT converge ####
convergence <- rep(NA, length(all_models))
names(convergence) <- names(all_models)
convergence <- sapply(X = all_models, FUN = function(x) x$convergence)
cat(sum(convergence), "did not converge\n")

# Exporting results ####

## Series ####
saveRDS(
  object = icd9_ts,
  file = "./outputs/dec23-report-ss-series/all_series.rds"
)

## Models ####
saveRDS(
  object = all_models,
  file = "./outputs/dec23-report-ss-series/all_models.rds"
)
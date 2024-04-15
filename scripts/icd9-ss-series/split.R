# The function of this script file is to implement to signal extract technique
# devised by Shumway and Stoffer. Here we are looking to pull out the trend and
# seasonal components leaving the noise and anomaly combined for all diseases
# based on CCS codes. We will only be using the inpatient and outpatient
# combined ICD-9-CM data.

# The difference between this analysis and the all-ss-series analysis is the
# inclusion of additional years of data. We now have data until Sept. 2015
# when ICD method changed from ICD-9 to ICD-10.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating output directory ####
if (!dir.exists("./outputs/icd9-ss-series/")) {
  dir.create("./outputs/icd9-ss-series/")
}

# Importing data ####
load("./data/final-incidence-series/final_cohort_incidence.RData")

# Preparing data for analysis ####

## Pulling unweighted series from ICD-9 cohort ####
icd <- subset(
  x = final_cohort_incidence,
  subset = cohort == "icd9"
)

### Transforming each series into ts objects ####
### based on setting and disease type
icd_ts <- split(
  x = icd,
  f = factor(icd$ccs_code)
)
icd_ts <- lapply(
  X = icd_ts,
  FUN = function(x) {
    x <- temp <- x[order(x$year, x$month), ]
    x <- subset(x = x, select = incidence)
    x <- stats::ts(
      data = x,
      start = c(2003, 1), frequency = 12
    )
    return(x)
  }
)
names(icd_ts) <- paste0("ccs", names(icd_ts))
original_len <- length(icd_ts)

### Eliminating disease with missing data ####
### There are certain diseases that do NOT have data for every month in the
### series. This is mostly likely due to some months having a zero incidence
### during that month. Therefore, our analyatical method is a complete case
### analysis.
verify_len <- length(rep(x = 2003:2015, times = c(rep(12, 12), 9)))
w_diseases <- which(x = sapply(icd_ts, length) == verify_len)
valid_diseases <- names(w_diseases)
if (length(valid_diseases) < original_len) {
  bad_diseases <- names(icd_ts)[-w_diseases]
}
icd_ts <- icd_ts[valid_diseases]

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
all_models <- vector(mode = "list", length = length(icd_ts))
names(all_models) <- names(icd_ts)
pb <- utils::txtProgressBar(min = 0, max = length(all_models), style = 3)
for (j in seq_along(all_models)) {
  ### Fitting models
  all_models[[j]] <- tryCatch(
    expr = {
      res <- ss(
        y = std(log(icd_ts[[j]])),
        a = a,
        phi = phi,
        theta0 = c(1, 1),
        mu0 = rep(0, 12),
        sigma0 = diag(1, 12),
        control = list(),
        auto = TRUE
      )
    }, warning = function(w) {
      paste(w)
    }, error = function(e) {
      paste(e)
    }, finally = {
      res
    }
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
convergence <- sapply(
  X = all_models,
  FUN = function(x) {
    if ("ss" %in% class(x)) {
      x$convergence
    } else {
      1
    }
  }
)
if (sum(convergence) == 0) {
  cat(sum(convergence), "did not converge\n")
} else {
  cat(sum(convergence), "did not converge\n")
  ww <- which(convergence == 1)
  data.frame(
    ccs = names(all_models)[ww],
    issue = all_models[[ww]]
  )
}

## Excluding problematic series ####
if (sum(convergence) > 0) {
  icd_ts_issue <- icd_ts[ww]
  all_models_issue <- all_models[ww]

  icd_ts <- icd_ts[-ww]
  all_models <- all_models[-ww]
}

# Exporting results ####

## Series ####
saveRDS(
  object = icd_ts,
  file = "./outputs/icd9-ss-series/all_series.rds"
)
if (sum(convergence) > 0) {
  saveRDS(
    object = icd_ts_issue,
    file = "./outputs/icd9-ss-series/all_series_issue.rds"
  )
}

## Models ####
saveRDS(
  object = all_models,
  file = "./outputs/icd9-ss-series/all_models.rds"
)
if (sum(convergence) > 0) {
  saveRDS(
    object = all_models_issue,
    file = "./outputs/icd9-ss-series/all_models_issue.rds"
  )
}

## Saving names of diseases with missing data ####
if (length(valid_diseases) < original_len) {
  saveRDS(
    object = bad_diseases,
    file = "./outputs/icd9-ss-series/bad_diseases.rds"
  )
}
# The function of this script file is to implement to signal extract technique
# devised by Shumway and Stoffer. Here we are looking to pull out the trend and
# seasonal components leaving the noise and anomaly combined for all diseases
# based on CCS codes. We will only be using the inpatient and outpatient
# combined ICD-9-CM data.

# The difference between this analysis and the all-ss-series analysis is the
# inclusion of additional years of data. We now have data until 2015.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating output directory ####
if (!dir.exists("./outputs/final-all-ss-series/")) {
  dir.create("./outputs/final-all-ss-series/")
}

# Importing data ####
load("./data/final-incidence-series/final_cohort_incidence.RData")

# Preparing data for analysis ####

## Pulling unweighted series from ICD-9 cohort ####
icd9 <- subset(
  x = final_cohort_incidence,
  subset = cohort == "icd9"
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
      start = 2003, end = 2015, frequency = 12
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
  all_models[[j]] <- tryCatch(
    expr = {
      res <- ss(
        y = std(log(icd9_ts[[j]])),
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
icd9_ts_issue <- icd9_ts[ww]
all_models_issue <- all_models[ww]

icd9_ts <- icd9_ts[-ww]
all_models <- all_models[-ww]

# Exporting results ####

## Series ####
saveRDS(
  object = icd9_ts,
  file = "./outputs/final-all-ss-series/final_all_series.rds"
)
if (length(icd9_ts_issue) > 0) {
  saveRDS(
    object = icd9_ts_issue,
    file = "./outputs/final-all-ss-series/final_all_series_issue.rds"
  )
}

## Models ####
saveRDS(
  object = all_models,
  file = "./outputs/final-all-ss-series/final_all_models.rds"
)
if (length(all_models_issue) > 0) {
  saveRDS(
    object = all_models,
    file = "./outputs/final-all-ss-series/final_all_models_issue.rds"
  )
}
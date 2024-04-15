# The function of this script file is to generate two CSV files that contain the
# estimated reduction in attributable risk for all age groups given a 20% and
# 60% reduction in influenza incidence during the peak month.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating output sub-directory ####
if (!dir.exists("./outputs/age-incidence/")) {
  dir.create("./outputs/age-incidence/")
}

# Importing attributable risk results ####
age_incidence_rds <- paste0(
  "./outputs/age-",
  c(
    "less2", "2to4", "5to11",
    "12to17", "18to34", "35to44",
    "45to54", "55to64", "greater64"
  ),
  "-ss-series/incidence_change.rds"
)
incidence_change <- lapply(X = age_incidence_rds, FUN = readRDS)

## Adding age cohort variable ####
age_cohort <- c(
  "Age: <2", "Age: 2-4", "Age: 5-11",
  "Age: 12-17", "Age: 18-34", "Age: 35-44",
  "Age: 45-54", "Age: 55-64", "Age: >64"
)
for (j in seq_along(incidence_change)) {
  incidence_change[[j]]$age_cohort <- age_cohort[j]
  incidence_change[[j]] <- subset(
    x = incidence_change[[j]],
    select = c(age_cohort, ccs_code, disease, pct, raw_change, relative_change)
  )
}

# Cleaning up imported data ####

## Creating one complete data set ####
incidence_change <- dplyr::bind_rows(incidence_change)

## Excluding attributable risk estimates not related to 20% and 60%
incidence_change <- subset(
  x = incidence_change,
  subset = pct %in% c(0.2, 0.6),
  select = -c(raw_change)
)
row.names(incidence_change) <- NULL

## Splititng risk estimates into two separate data sets ####
## based on reduction influenza
incidence_change <- split(
  x = incidence_change,
  f = factor(incidence_change$pct)
)
names(incidence_change) <- paste0(
  "pct",
  sapply(
    X = incidence_change,
    FUN = function(x) {
      unique(x$pct) * 100
    }
  )
)

# Exporting data ####

## 20% reduction in influenza ####
utils::write.csv(
  x = incidence_change$pct20,
  file = "./outputs/age-incidence/age_incidence_pct20.csv",
  row.names = FALSE
)

## 60% reduction in influenza ####
utils::write.csv(
  x = incidence_change$pct60,
  file = "./outputs/age-incidence/age_incidence_pct60.csv",
  row.names = FALSE
)
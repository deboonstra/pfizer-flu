# The function of this script file is to calculate the incidence reduction of a
# disease based on a reduction in the flu incidence. We are only modeling the
# inpatient and outpatient combined data as it covers diseases that only
# received inpatient or outpatient only cases.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing data and results ####
all_series <- readRDS(
  file = "./outputs/age-less2-ss-series/all_series.rds"
)
cause_mods <- readRDS(
  file = "./outputs/age-less2-ss-series/causation_mods.rds"
)
load("./outputs/age-less2-ss-series/clustering.RData")

# Subsetting the series object ####
# Pulling out the diseases that are globally and locally clustered with the flu
cause_series <- all_series[names(cause_mods)]

# Calculating change in incidence ####

## Creating incidence object ####
pct <- c(0.2, 0.3, 0.4, 0.5, 0.6)
change <- data.frame(
  ccs_code = rep(corr_asm$ccs_code, times = length(pct)),
  disease = rep(corr_asm$disease, times = length(pct)),
  pct = rep(pct, each = nrow(corr_asm)),
  raw_change = NA,
  relative_change = NA
)
change <- split(x = change, f = factor(change$pct))
for (p in seq_along(change)) {
  for (s in seq_along(cause_series)) {
    change[[p]]$raw_change[s] <- change_incidence(
      x = all_series$ccs123, y = cause_series[[s]], object = cause_mods[[s]],
      months = c(12, 1, 2, 3), pct = pct[p], type = "std-log", relative = FALSE
    )
    change[[p]]$relative_change[s] <- change_incidence(
      x = all_series$ccs123, y = cause_series[[s]], object = cause_mods[[s]],
      months = c(12, 1, 2, 3), pct = pct[p], type = "std-log", relative = TRUE
    )
  }
}
change <- dplyr::bind_rows(change)

# Exporting ####
saveRDS(
  object = change,
  file = "./outputs/age-less2-ss-series/incidence_change.rds"
)
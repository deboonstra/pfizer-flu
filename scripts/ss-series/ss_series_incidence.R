# The function of this script file is to calculate the incidence reduction of a
# disease based on a reduction in the flu incidence. We are only modeling the
# inpatient and outpatient combined data as it covers diseases that only
# received inpatient or outpatient only cases.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing data and results ####
load("./outputs/ss-series/inoutpatient_series.RData")
load("./outputs/ss-series/causation_mods.RData")
load("./outputs/ss-series/clustering.RData")

# Examining clustering ####
print(corr_inout_asm)

# Creating data objects to automatically calculate incidence change ####

## Matrix of ts objects ####
ts_inout <- data.frame(
  flu = flu_inout,
  bronchitis = bronchitis_inout,
  up_resp = up_resp_inout,
  pneumonia = pneumonia_inout,
  viral = viral_inout,
  otitis = otitis_inout,
  tonsillitis = tonsillitis_inout
)

## List of causation models ####
mods_inout <- list(
  bronchitis = bronchitis_mod_inout,
  up_resp = up_resp_mod_inout,
  pneumonia = pneumonia_mod_inout,
  viral = viral_mod_inout,
  otitis = otitis_mod_inout,
  tonsillitis = tonsillitis_mod_inout
)

# Calculating change in incidence ####
change <- data.frame(
  disease = c(
    "Bronchitis", "Upper respiratory infection", "Pneumonia", "Viral infection",
    "Otitis media", "Tonsillitis"
  ),
  raw_change = NA,
  relative_change = NA
)
for (j in seq_along(mods_inout)) {
  change[j, 2] <- change_incidence(
    x = ts_inout[, 1], y = ts_inout[, j + 1], object = mods_inout[[j]],
    months = c(12, 1, 2, 3), pct = 0.20, type = "std-log", relative = FALSE
  )
  change[j, 3] <- change_incidence(
    x = ts_inout[, 1], y = ts_inout[, j + 1], object = mods_inout[[j]],
    months = c(12, 1, 2, 3), pct = 0.20, type = "std-log", relative = TRUE
  )
}

# Exporting ####
saveRDS(object = change, file = "./outputs/ss-series/incidence_change.rds")
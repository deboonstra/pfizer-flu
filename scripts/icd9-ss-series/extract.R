# The function of this script file is extract the latent processes from each
# state-space modeling object. These results are dependent on
# final_all_ss_series_split.R script file.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing results ####

## Series ####
icd9_ts <- readRDS(file = "./outputs/icd9-ss-series/all_series.rds")

## Models ###
all_models <- readRDS(
  file = "./outputs/icd9-ss-series/all_models.rds"
)

# Extraction ####
all_extract <- lapply(
  X = seq_along(all_models),
  FUN = function(j) {
    signal_extract(
      data = std(log(icd9_ts[[j]])),
      object = all_models[[j]]
    )
  }
)
names(all_extract) <- names(all_models)

# Exporting results ####
saveRDS(
  object = all_extract,
  file = "./outputs/icd9-ss-series/all_extract.rds"
)
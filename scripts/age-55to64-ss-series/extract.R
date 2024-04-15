# The function of this script file is extract the latent processes from each
# state-space modeling object. These results are dependent on
# split.R script file.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing results ####

## Series ####
all_series <- readRDS(
  file = "./outputs/age-55to64-ss-series/all_series.rds"
)

## Models ###
all_models <- readRDS(
  file = "./outputs/age-55to64-ss-series/all_models.rds"
)

# Extraction ####
all_extract <- lapply(
  X = seq_along(all_models),
  FUN = function(j) {
    signal_extract(
      data = std(log(all_series[[j]])),
      object = all_models[[j]]
    )
  }
)
names(all_extract) <- names(all_models)

# Exporting results ####
saveRDS(
  object = all_extract,
  file = "./outputs/age-55to64-ss-series/all_extract.rds"
)
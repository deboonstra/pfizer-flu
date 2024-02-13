# The function of this script file is perform global and local clustering. The
# threshold for global clustering will be set to +0.6 and threshold for local
# clustering will be set at +0.2. The series that achieve both of this
# thresholds will move onto the examination of causation. This clustering
# algorithm will only be applied to the inpatient and outpatient combined data
# at the current moment (2023-11-28).

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing series abstraction ####
all_extract <- readRDS(
  file = "./outputs/age-5to11-ss-series/all_extract.rds"
)
all_series <- readRDS(
  file = "./outputs/age-5to11-ss-series/all_series.rds"
)

## Importing CCS names ####
ccs_labels <- utils::read.csv(file = "./data/ccs-series/ccs_labels.csv")

### Reordering CCS names ####
### to match the order of the all_extract object
ccs_no <- sapply(
  X = names(all_extract),
  FUN = function(x) {
    as.integer(substr(x = x, start = 4, stop = nchar(x)))
  }
)
ccs_labels <- subset(
  x = ccs_labels,
  subset = ccs_code %in% ccs_no
)
ccs_labels <- ccs_labels[order(ccs_labels$ccs_code), ]
row.names(ccs_labels) <- NULL

# Clustering ####

## Setting thresholds ####
ssm_threshold <- 0.6 # global
asm_threshold <- 0.2 # local

## Creating matrix of correlation to evaluate ####

### Global correlation ####
rho_ssm <- sapply(
  X = seq_along(all_extract),
  FUN = function(j) {
    stats::cor(c(all_extract$ccs123$ssm), c(all_extract[[j]]$ssm))
  }
)

### Local correlation ####
rho_asm <- sapply(
  X = seq_along(all_extract),
  FUN = function(j) {
    stats::cor(c(all_extract$ccs123$asm), c(all_extract[[j]]$asm))
  }
)

### Creating matrix of correlation values ####
corr_all <- data.frame(
  ccs_code = ccs_labels$ccs_code,
  disease = ccs_labels$ccs_category,
  rho_ssm = rho_ssm,
  rho_asm = rho_asm
)

### Excluding the influenza observation ####
corr_all <- subset(x = corr_all, subset = ccs_code != 123L)

## Global (seasonal) clustering ####
corr_ssm <- subset(
  x = corr_all,
  subset = rho_ssm >= ssm_threshold
)

## Local (anomaly and error) clustering ####
corr_asm <- subset(
  x = corr_ssm,
  subset = rho_asm >= asm_threshold
)

# Exporting ####
save(
  corr_all, corr_ssm, corr_asm, ssm_threshold, asm_threshold,
  file = "./outputs/age-5to11-ss-series/clustering.RData"
)
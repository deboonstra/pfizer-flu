# The function of this script file is to devise models that have the possbility
# of asserting causality between the SSM + ASM components of each disease and
# the flu. We are only modeling the inpatient and outpatient combined data as it
# covers diseases that only received inpatient or outpatient only cases.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing data and correlation matrices ####
all_extract <- readRDS(
  file = "./outputs/dec23-report-ss-series/all_extract.rds"
)
load("./outputs/dec23-report-ss-series/clustering.RData")


# Subsetting the extracted series object ####
# Pulling out the diseases that are globally and locally clustered with the flu
ccs_no <- paste0("ccs", corr_asm$ccs_code)
cause_extract <- all_extract[ccs_no]

# Modeling ####
cause_mods <- vector(mode = "list", length = length(cause_extract))
names(cause_mods) <- names(cause_extract)
x <- c(all_extract$ccs123$ssm + all_extract$ccs123$asm)
for (j in seq_along(cause_extract)) {
  y <- c(cause_extract[[j]]$ssm + cause_extract[[j]]$asm)
  cause_mods[[j]] <- stats::lm(y ~ x)
}

# Exporting models ####
saveRDS(
  object = cause_mods,
  file = "./outputs/dec23-report-ss-series/causation_mods.rds"
)
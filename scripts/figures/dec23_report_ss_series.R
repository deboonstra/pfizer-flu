# The function of this script file is to generate all figures for the diseases
# that globally or locally correlated with influenza.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating output directory ####
if (!dir.exists("./figures/dec23-report-ss-series/")) {
  dir.create("./figures/dec23-report-ss-series/")
}

# Importing data and results ####
all_extract <- readRDS(
  file = "./outputs/dec23-report-ss-series/all_extract.rds"
)
load("./outputs/dec23-report-ss-series/clustering.RData")

# Plotting ####

## Influenza ####
svglite::svglite(
  filename = "./figures/dec23-report-ss-series/ccs123.svg"
)
signal_extract_plot(
  data = all_extract$ccs123,
  ylab = "Standardized log(Incidence per 100,000)",
  main = "CCS: 123, Influenza", oma = c(2, 2, 2, 0)
)
grDevices::dev.off()

## Other diseases (Globally and locally correlated)
for (j in seq_len(nrow(corr_ssm))) {
  fname <- paste0("ccs", corr_ssm$ccs_code[j])
  svglite::svglite(
    filename = paste0("./figures/dec23-report-ss-series/", fname, ".svg")
  )
  signal_extract_plot(
    data = all_extract[[fname]],
    ylab = "Standardized log(Incidence per 100,000)",
    main = paste0("CCS: ", corr_ssm$ccs_code[j], ", ", corr_ssm$disease[j]),
    oma = c(2, 2, 2, 0)
  )
  grDevices::dev.off()
}
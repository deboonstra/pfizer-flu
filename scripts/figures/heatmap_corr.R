# The function of this script file is to create two heat maps based on the
# seasonal and local correlations. The diseases included will be any disease
# that was ever locally correlated with influenza

# Loading libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating heatmap sub-directory ####
if (!dir.exists("./figures/heatmap/")) {
  dir.create("./figures/heatmap/")
}

# Importing data ####

## Age group: less than 2 years old (secondary cohort) ####
load("./outputs/age-less2-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_less2 <- corr_all
corr_less2$type <- "Age group: < 2"
### Pulling CCS codes for locally correlated diseases
ccs_less2 <- corr_asm$ccs_code

## Age group: 2 to 4 years old (secondary cohort) ####
load("./outputs/age-2to4-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_2to4 <- corr_all
corr_2to4$type <- "Age group: 2-4"
### Pulling CCS codes for locally correlated diseases
ccs_2to4 <- corr_asm$ccs_code

## Age group: 5 to 11 years old (secondary cohort) ####
load("./outputs/age-5to11-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_5to11 <- corr_all
corr_5to11$type <- "Age group: 5-11"
### Pulling CCS codes for locally correlated diseases
ccs_5to11 <- corr_asm$ccs_code

## Age group: 12 to 17 years old (secondary cohort) ####
load("./outputs/age-12to17-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_12to17 <- corr_all
corr_12to17$type <- "Age group: 12-17"
### Pulling CCS codes for locally correlated diseases
ccs_12to17 <- corr_asm$ccs_code

## Age group: 18 to 34 years old (secondary cohort) ####
load("./outputs/age-18to34-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_18to34 <- corr_all
corr_18to34$type <- "Age group: 18-34"
### Pulling CCS codes for locally correlated diseases
ccs_18to34 <- corr_asm$ccs_code

## Age group: 35 to 44 years old (secondary cohort) ####
load("./outputs/age-35to44-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_35to44 <- corr_all
corr_35to44$type <- "Age group: 35-44"
### Pulling CCS codes for locally correlated diseases
ccs_35to44 <- corr_asm$ccs_code

## Age group: 12 to 17 years old (secondary cohort) ####
load("./outputs/age-45to54-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_45to54 <- corr_all
corr_45to54$type <- "Age group: 45-54"
### Pulling CCS codes for locally correlated diseases
ccs_45to54 <- corr_asm$ccs_code

## Age group: 55 to 64 years old (secondary cohort) ####
load("./outputs/age-55to64-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_55to64 <- corr_all
corr_55to64$type <- "Age group: 55-64"
### Pulling CCS codes for locally correlated diseases
ccs_55to64 <- corr_asm$ccs_code

## Age group: greater than 64 years old (secondary cohort) ####
load("./outputs/age-greater64-ss-series/clustering.RData")
### Renaming all correlation data ####
corr_greater64 <- corr_all
corr_greater64$type <- "Age group: > 64"
### Pulling CCS codes for locally correlated diseases
ccs_greater64 <- corr_asm$ccs_code

# Finding all the unique locally correlated diseases
ccs <- c(
  ccs_less2, ccs_2to4, ccs_5to11, ccs_12to17, ccs_12to17, ccs_18to34,
  ccs_35to44, ccs_45to54, ccs_55to64, ccs_greater64
)
ccs <- unique(ccs)
ccs <- ccs[order(ccs)]

# Creating seasonal and local correlation objects for local diseases ####
# Local diseases is defined to be the diseases that are at any point locally
# correlated with influenza.

## Combining all the correlation data.frames ####
corr <- dplyr::bind_rows(
  corr_less2, corr_2to4, corr_5to11, corr_12to17, corr_18to34, corr_35to44,
  corr_45to54, corr_55to64, corr_greater64
)

## Subsetting overall correlation object to only contain local diseases ####
corr <- subset(corr, subset = ccs_code %in% ccs)

## Turning type variable into a factor ####
## This is needed to force a discrete mapping.
corr$type <- factor(
  x = corr$type,
  levels = unique(corr$type),
  ordered = TRUE
)

## Turning ccs_code variable into a factor
corr <- corr[order(corr$ccs_code), ]
corr$ccs_code <- factor(
  x = corr$ccs_code,
  levels = unique(corr$ccs_code),
  ordered = TRUE
)

## Creating seasonal correlation matrix ####
ssm <- subset(corr, select = c(ccs_code, disease, rho_ssm, type))

### Inputting NA values ####
### For values less than threshold
ssm$rho_ssm <- ifelse(ssm$rho_ssm < ssm_threshold, NA, ssm$rho_ssm)

## Creating local correlation matrix ####
asm <- subset(corr, select = c(ccs_code, disease, rho_asm, type))

### Inputting NA values ####
### For values less than threshold
asm$rho_asm <- ifelse(asm$rho_asm < asm_threshold, NA, asm$rho_asm)

# Plotting heat maps ####

## Seasonal ####
svglite::svglite(filename = "./figures/heatmap/corr_ssm.svg")
ggplot2::ggplot(
  data = ssm,
  mapping = ggplot2::aes(x = ccs_code, y = type, fill = rho_ssm)
) +
ggplot2::geom_tile() +
ggplot2::scale_fill_gradient(
  low = "#999999",
  high = "#000000",
  na.value = "#b25f5f",
  limit = c(ssm_threshold, 1)
) +
ggplot2::labs(
  x = "CCS",
  y = "Age group",
  fill = paste0("Seasonal", "\n", "correlation")
) +
ggplot2::theme(
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank()
)
grDevices::dev.off()

## Local ####
svglite::svglite(filename = "./figures/heatmap/corr_asm.svg")
ggplot2::ggplot(
  data = asm,
  mapping = ggplot2::aes(x = ccs_code, y = type, fill = rho_asm)
) +
ggplot2::geom_tile() +
ggplot2::scale_fill_gradient(
  low = "#999999",
  high = "#000000",
  na.value = "#b25f5f",
  limit = c(asm_threshold, 1)
) +
ggplot2::labs(
  x = "CCS",
  y = "Age group",
  fill = paste0("Local", "\n", "correlation")
) +
ggplot2::theme(
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank()
)
grDevices::dev.off()
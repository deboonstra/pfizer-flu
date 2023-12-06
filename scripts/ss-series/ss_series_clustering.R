# The function of this script file is perform global and local clustering. The
# threshold for global clustering will be set to +0.7 and threshold for local
# clustering will be set at +0.2. The series that achieve both of this
# thresholds will move onto the examination of causation. This clustering
# algorithm will only be applied to the inpatient and outpatient combined data
# at the current moment (2023-11-28).

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing series abstraction ####
load("./outputs/ss-series/inoutpatient_extract.RData")

# Clustering ####

## Setting thresholds ####
ssm_threshold <- 0.7 # global
asm_threshold <- 0.2 # local

## Creating matrix of correlation to evaluate ####
corr_inout <- data.frame(
  disease = c(
    "Bronchitis", "Upper respiratory infection", "Pneumonia", "Gout",
    "Live born", "Breast cancer", "Meningitis", "Viral infections",
    "Other infections", "Encephalitis", "Inflammation; infection of eye",
    "Otitis media", "Other ear and sense organ disorders", "AMI",
    "Nonspecific chest pain", "Varicoses veins of lower extremity",
    "Other diseases of veins and lymphatics", "Tonsillitis"
  ),
  rho_ssm = c(
    stats::cor(c(flu_inout_extract$ssm), c(bronchitis_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(up_resp_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(pneumonia_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(gout_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(liveborn_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(cancerbreast_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(meningitis_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(viral_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(other_infct_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(encephalitis_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(eye_infct_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(otitis_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(other_ear_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(ami_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(chest_pain_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(varicoses_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(other_veins_inout_extract$ssm)),
    stats::cor(c(flu_inout_extract$ssm), c(tonsillitis_inout_extract$ssm))
  ),
  rho_asm = c(
    stats::cor(c(flu_inout_extract$asm), c(bronchitis_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(up_resp_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(pneumonia_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(gout_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(liveborn_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(cancerbreast_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(meningitis_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(viral_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(other_infct_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(encephalitis_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(eye_infct_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(otitis_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(other_ear_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(ami_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(chest_pain_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(varicoses_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(other_veins_inout_extract$asm)),
    stats::cor(c(flu_inout_extract$asm), c(tonsillitis_inout_extract$asm))
  )
)

## Global (seasonal) ####

### View ####
print(corr_inout[order(corr_inout$rho_ssm, decreasing = TRUE), ])

### Correlation filtering ####
corr_inout_ssm <- subset(
  x = corr_inout,
  subset = rho_ssm >= ssm_threshold
)

## Local clustering (anomaly and error) ####

### View ####
print(corr_inout_ssm[order(corr_inout_ssm$rho_asm, decreasing = TRUE), ])

### Correlation filtering ####
corr_inout_asm <- subset(
  x = corr_inout_ssm,
  subset = rho_asm >= asm_threshold
)

# Exporting ####
save(
  corr_inout, corr_inout_ssm, corr_inout_asm,
  file = "./outputs/ss-series/clustering.RData"
)
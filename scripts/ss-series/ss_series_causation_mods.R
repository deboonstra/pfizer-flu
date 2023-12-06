# The function of this script file is devise models that have the possbility of
# asserting causality between the SSM + ASM components of each disease and the
# flu. We are only modeling the inpatient and outpatient combined data as it
# covers diseases that only received inpatient or outpatient only cases.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing data and correlation matrices ####
load("./outputs/ss-series/inoutpatient_extract.RData")
load("./outputs/ss-series/clustering.RData")

# Examining clustering ####
print(corr_inout_asm)

# Modeling ####

# Combining the SSM and ASM components of the flu ####
flu_x_inout <- flu_inout_extract$ssm + flu_inout_extract$asm

## Bronchitis ####

### Combining the SSM and ASM components ####
bronchitis_y_inout <- bronchitis_inout_extract$ssm + bronchitis_inout_extract$asm #nolint

### Modeling #####
bronchitis_mod_inout <- stats::lm(bronchitis_y_inout ~ flu_x_inout)
summary(bronchitis_mod_inout)

## Pneumonia ####

### Combining the SSM and ASM components ####
pneumonia_y_inout <- pneumonia_inout_extract$ssm + pneumonia_inout_extract$asm #nolint

### Modeling #####
pneumonia_mod_inout <- stats::lm(pneumonia_y_inout ~ flu_x_inout)
summary(pneumonia_mod_inout)

## Upper respiratory infections ####

### Combining the SSM and ASM components ####
up_resp_y_inout <- up_resp_inout_extract$ssm + up_resp_inout_extract$asm #nolint

### Modeling #####
up_resp_mod_inout <- stats::lm(up_resp_y_inout ~ flu_x_inout)
summary(up_resp_mod_inout)

## Viral infections ####

### Combining the SSM and ASM components ####
viral_y_inout <- viral_inout_extract$ssm + viral_inout_extract$asm #nolint

### Modeling #####
viral_mod_inout <- stats::lm(viral_y_inout ~ flu_x_inout)
summary(viral_mod_inout)

## Otitis media ####

### Combining the SSM and ASM components ####
otitis_y_inout <- otitis_inout_extract$ssm + otitis_inout_extract$asm

### Modeling ####
otitis_mod_inout <- stats::lm(otitis_y_inout ~ flu_x_inout)
summary(otitis_mod_inout)

## Tonsillitis ####

### Combining the SSM and ASM components ####
tonsillitis_y_inout <- tonsillitis_inout_extract$ssm + tonsillitis_inout_extract$asm #nolint

### Modeling ####
tonsillitis_mod_inout <- stats::lm(tonsillitis_y_inout ~ flu_x_inout)
summary(tonsillitis_mod_inout)

# Exporting ####
save(
  bronchitis_mod_inout, pneumonia_mod_inout, up_resp_mod_inout, viral_mod_inout,
  otitis_mod_inout, tonsillitis_mod_inout,
  file = "./outputs/ss-series/causation_mods.RData"
)
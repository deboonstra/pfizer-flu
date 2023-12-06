# The function of this script file is extract the latent processes from each
# state-space modeling object. These results are dependent on ss_series_split.R
# script file.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Importing results ####

## Series ####
load("./outputs/ss-series/inpatient_series.RData")
load("./outputs/ss-series/inoutpatient_series.RData")

## Models ###
load("./outputs/ss-series/inpatient_models.RData")
load("./outputs/ss-series/inoutpatient_models.RData")

# Extraction ####

## Flu ####
flu_in_extract <- signal_extract(data = std(log(flu_in)), object = ss_flu_in)
flu_inout_extract <- signal_extract(
  data = std(log(flu_inout)),
  object = ss_flu_inout
)

## Bronchitis ####
bronchitis_in_extract <- signal_extract(
  data = std(log(bronchitis_in)),
  object = ss_bronchitis_in
)
bronchitis_inout_extract <- signal_extract(
  data = std(log(bronchitis_inout)),
  object = ss_bronchitis_inout
)

## Upper respiratory infections ####
up_resp_in_extract <- signal_extract(
  data = std(log(up_resp_in)),
  object = ss_up_resp_in
)
up_resp_inout_extract <- signal_extract(
  data = std(log(up_resp_inout)),
  object = ss_up_resp_inout
)

## Pneumonia ####
pneumonia_in_extract <- signal_extract(
  data = std(log(pneumonia_in)),
  object = ss_pneumonia_in
)
pneumonia_inout_extract <- signal_extract(
  data = std(log(pneumonia_inout)),
  object = ss_pneumonia_inout
)

## Gout ####
gout_in_extract <- signal_extract(data = std(log(gout_in)), object = ss_gout_in)
gout_inout_extract <- signal_extract(
  data = std(log(gout_inout)),
  object = ss_gout_inout
)

## Liveborn ####
liveborn_in_extract <- signal_extract(
  data = std(log(liveborn_in)),
  object = ss_liveborn_in
)
liveborn_inout_extract <- signal_extract(
  data = std(log(liveborn_inout)),
  object = ss_liveborn_inout
)

## Cancer of breast ####
cancerbreast_in_extract <- signal_extract(
  data = std(log(cancerbreast_in)),
  object = ss_cancerbreast_in
)
cancerbreast_inout_extract <- signal_extract(
  data = std(log(cancerbreast_inout)),
  object = ss_cancerbreast_inout
)

## Meningitis ####
meningitis_in_extract <- signal_extract(
  data = std(log(meningitis_in)),
  object = ss_meningitis_in
)
meningitis_inout_extract <- signal_extract(
  data = std(log(meningitis_inout)),
  object = ss_meningitis_inout
)

## Viral infections ####
viral_in_extract <- signal_extract(
  data = std(log(viral_in)),
  object = ss_viral_in
)
viral_inout_extract <- signal_extract(
  data = std(log(viral_inout)),
  object = ss_viral_inout
)

## Other infections ####
other_infct_in_extract <- signal_extract(
  data = std(log(other_infct_in)),
  object = ss_other_infct_in
)
other_infct_inout_extract <- signal_extract(
  data = std(log(other_infct_inout)),
  object = ss_other_infct_inout
)

## Encephalitis ####
encephalitis_in_extract <- signal_extract(
  data = std(log(encephalitis_in)),
  object = ss_encephalitis_in
)
encephalitis_inout_extract <- signal_extract(
  data = std(log(encephalitis_inout)),
  object = ss_encephalitis_inout
)

## Inflammation; infection of eye ####
eye_infct_in_extract <- signal_extract(
  data = std(log(eye_infct_in)),
  object = ss_eye_infct_in
)
eye_infct_inout_extract <- signal_extract(
  data = std(log(eye_infct_inout)),
  object = ss_eye_infct_inout
)

## Otitis media ####
otitis_in_extract <- signal_extract(
  data = std(log(otitis_in)),
  object = ss_otitis_in
)
otitis_inout_extract <- signal_extract(
  data = std(log(otitis_inout)),
  object = ss_otitis_inout
)

## Other ear and sense organ disorders ####
other_ear_in_extract <- signal_extract(
  data = std(log(other_ear_in)),
  object = ss_other_ear_in
)
other_ear_inout_extract <- signal_extract(
  data = std(log(other_ear_inout)),
  object = ss_other_ear_inout
)

## Acute myocardial infraction ####
ami_in_extract <- signal_extract(
  data = std(log(ami_in)),
  object = ss_ami_in
)
ami_inout_extract <- signal_extract(
  data = std(log(ami_inout)),
  object = ss_ami_inout
)

## Nonspecific chest pain ####
chest_pain_in_extract <- signal_extract(
  data = std(log(chest_pain_in)),
  object = ss_chest_pain_in
)
chest_pain_inout_extract <- signal_extract(
  data = std(log(chest_pain_inout)),
  object = ss_chest_pain_inout
)

## Varicoses veins of lower extremity ####
varicoses_in_extract <- signal_extract(
  data = std(log(varicoses_in)),
  object = ss_varicoses_in
)
varicoses_inout_extract <- signal_extract(
  data = std(log(varicoses_inout)),
  object = ss_varicoses_inout
)

## Other diseases of veins and lymphatics ####
other_veins_in_extract <- signal_extract(
  data = std(log(other_veins_in)),
  object = ss_other_veins_in
)
other_veins_inout_extract <- signal_extract(
  data = std(log(other_veins_inout)),
  object = ss_other_veins_inout
)

## Acute and chronic tonsillitis ####
tonsillitis_in_extract <- signal_extract(
  data = std(log(tonsillitis_in)),
  object = ss_tonsillitis_in
)
tonsillitis_inout_extract <- signal_extract(
  data = std(log(tonsillitis_inout)),
  object = ss_tonsillitis_inout
)

# Exporting results ####
save(
  bronchitis_in_extract, cancerbreast_in_extract, flu_in_extract,
  gout_in_extract, liveborn_in_extract, pneumonia_in_extract,
  up_resp_in_extract, meningitis_in_extract, viral_in_extract,
  other_infct_in_extract, encephalitis_in_extract, eye_infct_in_extract,
  otitis_in_extract, other_ear_in_extract, ami_in_extract,
  chest_pain_in_extract, varicoses_in_extract, other_veins_in_extract,
  tonsillitis_in_extract,
  file = "./outputs/ss-series/inpatient_extract.RData"
)
save(
  bronchitis_inout_extract, cancerbreast_inout_extract, flu_inout_extract,
  gout_inout_extract, liveborn_inout_extract, pneumonia_inout_extract,
  up_resp_inout_extract, meningitis_inout_extract, viral_inout_extract,
  other_infct_inout_extract, encephalitis_inout_extract,
  eye_infct_inout_extract, otitis_inout_extract,other_ear_inout_extract,
  ami_inout_extract, chest_pain_inout_extract, varicoses_inout_extract,
  other_veins_inout_extract, tonsillitis_inout_extract,
  file = "./outputs/ss-series/inoutpatient_extract.RData"
)
# The function of this script file is to implement to signal extract technique
# devised by Shumway and Stoffer. Here we are looking to pull out the trend and
# seasonal components leaving the noise and anomaly combined.

# Importing libraries and functions ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating output directory ####
if (!dir.exists("./outputs/ss-series/")) {
  dir.create("./outputs/ss-series/")
}

# Importing data ####
load("./data/incidence-series/monthly_incidence_cohorts.RData")

# Preparing data for analysis ####

## Pulling unweighted series from ICD-9 cohort ####
icd9 <- subset(
  x = monthly_incidence_cohorts,
  subset = population == "icd9" & series == "unweighted"
)

### Transforming each series into ts objects ####
### based on setting and disease type

#### Inpatient ####
icd9_in <- subset(x = icd9, subset = setting == "inpatient")
icd9_in <- split(
  x = icd9_in,
  f = factor(icd9_in$ccs_code)
)
icd9_in <- lapply(
  X = icd9_in,
  FUN = function(x) {
    x <- x[order(x$year, x$month), ]
    x <- subset(x = x, select = incidence)
    x <- stats::ts(
      data = x,
      start = 2003, end = 2013, frequency = 12
    )
    return(x)
  }
)

#### Inpatient and outpatient combined ####
icd9_inout <- subset(
  x = icd9,
  subset = setting == "inpatient + outpatient"
)
icd9_inout <- split(
  x = icd9_inout,
  f = factor(icd9_inout$ccs_code)
)
icd9_inout <- lapply(
  X = icd9_inout,
  FUN = function(x) {
    x <- x[order(x$year, x$month), ]
    x <- subset(x = x, select = incidence)
    x <- stats::ts(
      data = x,
      start = 2003, end = 2013, frequency = 12
    )
    return(x)
  }
)

# Signal extract ####

## Defining observation equation design matrix ####
a <- cbind(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

## Defining the design matrix for the state equation
phi <- diag(0, length(a))
phi[1, 1] <- 1
phi[2, ] <- c(0, rep(-1, 11))
w <- which(a == 0)[1]
for (k in seq(w, nrow(phi))) {
  phi[k, (k - 1)] <- 1
}

## Flu ####
flu_in <- icd9_in$`123`
flu_inout <- icd9_inout$`123`

### Inpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(flu_in), main = "Observed series", ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_flu_in <- ss(
  y = std(log(flu_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(flu_inout), main = "Observed series", ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_flu_inout <- ss(
  y = std(log(flu_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Bronchitis ####
bronchitis_in <- icd9_in$`125`
bronchitis_inout <- icd9_inout$`125`

### Inpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(bronchitis_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_bronchitis_in <- ss(
  y = std(log(bronchitis_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(bronchitis_inout), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_bronchitis_inout <- ss(
  y = std(log(bronchitis_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Upper respiratory infections
up_resp_in <- icd9_in$`126`
up_resp_inout <- icd9_inout$`126`

### Inpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(up_resp_in), main = "Observed series", ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_up_resp_in <- ss(
  y = std(log(up_resp_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(up_resp_inout), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_up_resp_inout <- ss(
  y = std(log(up_resp_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Pneumonia ####
pneumonia_in <- icd9_in$`122`
pneumonia_inout <- icd9_inout$`122`

### Inpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(pneumonia_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_pneumonia_in <- ss(
  y = std(log(pneumonia_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(pneumonia_inout), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_pneumonia_inout <- ss(
  y = std(log(pneumonia_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Gout ####
gout_in <- icd9_in$`54`
gout_inout <- icd9_inout$`54`

### Inpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(gout_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_gout_in <- ss(
  y = std(log(gout_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
#### Non-stationary and log-transformation does not fix it.
astsa::tsplot(
  log(gout_inout), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_gout_inout <- ss(
  y = std(log(gout_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Liveborn ####
liveborn_in <- icd9_in$`218`
liveborn_inout <- icd9_inout$`218`

### Inpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(liveborn_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_liveborn_in <- ss(
  y = std(log(liveborn_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(liveborn_inout), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_liveborn_inout <- ss(
  y = std(log(liveborn_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Cancer of breast ####
cancerbreast_in <- icd9_in$`24`
cancerbreast_inout <- icd9_inout$`24`

### Inpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(cancerbreast_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_cancerbreast_in <- ss(
  y = std(log(cancerbreast_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(cancerbreast_inout), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_cancerbreast_inout <- ss(
  y = std(log(cancerbreast_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Meningitis ####
meningitis_in <- icd9_in$`76`
meningitis_inout <- icd9_inout$`76`

### Inpatient ####

#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(meningitis_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_meningitis_in <- ss(
  y = std(log(meningitis_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
#### We have it with a log-transformation
astsa::tsplot(
  log(meningitis_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_meningitis_inout <- ss(
  y = std(log(meningitis_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Viral infections ####
viral_in <- icd9_in$`7`
viral_inout <- icd9_inout$`7`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(viral_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_viral_in <- ss(
  y = std(log(viral_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(viral_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_viral_inout <- ss(
  y = std(log(viral_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Other infections ####
other_infct_in <- icd9_in$`8`
other_infct_inout <- icd9_inout$`8`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(other_infct_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_other_infct_in <- ss(
  y = std(log(other_infct_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(other_infct_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_other_infct_inout <- ss(
  y = std(log(other_infct_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Encephalitis ####
encephalitis_in <- icd9_in$`77`
encephalitis_inout <- icd9_inout$`77`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(encephalitis_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_encephalitis_in <- ss(
  y = std(log(encephalitis_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(encephalitis_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_encephalitis_inout <- ss(
  y = std(log(encephalitis_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Inflammation; infection of eye ####
eye_infct_in <- icd9_in$`90`
eye_infct_inout <- icd9_inout$`90`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(eye_infct_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_eye_infct_in <- ss(
  y = std(log(eye_infct_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(eye_infct_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_eye_infct_inout <- ss(
  y = std(log(eye_infct_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Otitis media ####
otitis_in <- icd9_in$`92`
otitis_inout <- icd9_inout$`92`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(otitis_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_otitis_in <- ss(
  y = std(log(otitis_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(otitis_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_otitis_inout <- ss(
  y = std(log(otitis_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Other ear and sense organ disorders ####
other_ear_in <- icd9_in$`94`
other_ear_inout <- icd9_inout$`94`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(other_ear_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_other_ear_in <- ss(
  y = std(log(other_ear_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(other_ear_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_other_ear_inout <- ss(
  y = std(log(other_ear_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Acute myocardial infraction ####
ami_in <- icd9_in$`100`
ami_inout <- icd9_inout$`100`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(ami_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_ami_in <- ss(
  y = std(log(ami_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(ami_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_ami_inout <- ss(
  y = std(log(ami_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Nonspecific chest pain ####
chest_pain_in <- icd9_in$`102`
chest_pain_inout <- icd9_inout$`102`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(chest_pain_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_chest_pain_in <- ss(
  y = std(log(chest_pain_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(chest_pain_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_chest_pain_inout <- ss(
  y = std(log(chest_pain_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Varicoses veins of lower extremity ####
varicoses_in <- icd9_in$`119`
varicoses_inout <- icd9_inout$`119`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(varicoses_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_varicoses_in <- ss(
  y = std(log(varicoses_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(varicoses_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_varicoses_inout <- ss(
  y = std(log(varicoses_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Other diseases of veins and lymphatics ####
other_veins_in <- icd9_in$`121`
other_veins_inout <- icd9_inout$`121`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(other_veins_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_other_veins_in <- ss(
  y = std(log(other_veins_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(other_veins_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_other_veins_inout <- ss(
  y = std(log(other_veins_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

## Acute and chronic tonsillitis ####
tonsillitis_in <- icd9_in$`124`
tonsillitis_inout <- icd9_inout$`124`

### Inpatient ####

#### Checking for stationarity ####
astsa::tsplot(
  log(tonsillitis_in), main = "Observed series",
  ylab = "log(Incidence per 100,000)"
)

#### Modeling the series ####
ss_tonsillitis_in <- ss(
  y = std(log(tonsillitis_in)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

### Inpatient and outpatient ####
#### Checking for stationarity ####
astsa::tsplot(
  log(tonsillitis_inout), main = "Observed series",
  ylab = "Incidence per 100,000"
)

#### Modeling the series ####
ss_tonsillitis_inout <- ss(
  y = std(log(tonsillitis_inout)),
  a = a,
  phi = phi,
  theta0 = c(1, 1),
  mu0 = rep(0, 12),
  sigma0 = diag(1, 12)
)

# Exporting results ####

## Series ####
save(
  bronchitis_in, cancerbreast_in, flu_in, gout_in, liveborn_in, pneumonia_in,
  up_resp_in, meningitis_in, viral_in, other_infct_in, encephalitis_in,
  eye_infct_in, otitis_in, other_ear_in, ami_in, chest_pain_in, varicoses_in,
  other_veins_in, tonsillitis_in,
  file = "./outputs/ss-series/inpatient_series.RData"
)
save(
  bronchitis_inout, cancerbreast_inout, flu_inout, gout_inout, liveborn_inout,
  pneumonia_inout, up_resp_inout, meningitis_inout, viral_inout,
  other_infct_inout, encephalitis_inout, eye_infct_inout, otitis_inout,
  other_ear_inout, ami_inout, chest_pain_inout, varicoses_inout,
  other_veins_inout, tonsillitis_inout,
  file = "./outputs/ss-series/inoutpatient_series.RData"
)

## Models ####
save(
  ss_bronchitis_in, ss_cancerbreast_in, ss_flu_in, ss_gout_in, ss_liveborn_in,
  ss_pneumonia_in, ss_up_resp_in, ss_meningitis_in, ss_viral_in,
  ss_other_infct_in, ss_encephalitis_in, ss_eye_infct_in, ss_otitis_in,
  ss_other_ear_in, ss_ami_in, ss_chest_pain_in, ss_varicoses_in,
  ss_other_veins_in, ss_tonsillitis_in,
  file = "./outputs/ss-series/inpatient_models.RData"
)
save(
  ss_bronchitis_inout, ss_cancerbreast_inout, ss_flu_inout, ss_gout_inout,
  ss_liveborn_inout, ss_pneumonia_inout, ss_up_resp_inout, ss_meningitis_inout,
  ss_viral_inout, ss_other_infct_inout, ss_encephalitis_inout,
  ss_eye_infct_inout, ss_otitis_inout, ss_other_ear_inout, ss_ami_inout,
  ss_chest_pain_inout, ss_varicoses_inout, ss_other_veins_inout,
  ss_tonsillitis_inout,
  file = "./outputs/ss-series/inoutpatient_models.RData"
)
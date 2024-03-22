# The function of this script file is to generate the figures for my version of
# Pfizer report named influenza_burden_report_21Feb_eb.docx.

# Loading functions and packages ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating tables directory ####
if (!dir.exists("./figures/influenza-burden-report-21Feb-eb/")) {
  dir.create("./figures/influenza-burden-report-21Feb-eb/")
}

# Creating figures ####

## Figure 1 ####
## Caption: Standardized log-transformed influenza series for the age groups
## throughout the study period (2001 – 2019). The incidence rate has a natural
## logarithm transformation to produce a stationary time series.

### Importing data ####
age_groups <- c(
  "less2", "2to4", "5to11",
  "12to17", "18to34", "35to44",
  "45to54", "55to64", "greater64"
)
flu_rds <- paste0("./outputs/age-", age_groups, "-ss-series/all_extract.rds")

#### Influenza extracted series ####
flu_extract <- lapply(
  X = flu_rds,
  FUN = function(x) {
    dat <- readRDS(file = x)
    flu <- dat$ccs123
    return(flu)
  }
)
names(flu_extract) <- paste0("age", age_groups)

### Plotting observed series ####

#### Getting observed series ####
flu_obs <- lapply(
  X = flu_extract,
  FUN = function(x) {
    flu <- x$obs
    return(flu)
  }
)
names(flu_obs) <- paste0("age", age_groups)

#### Plotting influenza series ####

##### Age group titles ####
age_group_titles <- c(
  "Age: <2", "Age: 2-4", "Age: 5-11",
  "Age: 12-17", "Age: 18-34", "Age: 35-44",
  "Age: 45-54", "Age: 55-64", "Age: >64"
)

#### Plotting ####

### SVG ####
svglite::svglite(
  filename = "./figures/influenza-burden-report-21Feb-eb/fig1.svg",
  width = 12, height = 12
)
graphics::par(mfrow = c(3, 3), oma = c(2, 2, 0, 0))
for (j in seq_along(flu_obs)) {
  astsa::tsplot(
    x = flu_obs[[j]], main = age_group_titles[j],
    ylab = "", xlab = "", ylim = c(-3, 3), lwd = 2
  )
}
graphics::mtext(
  text = "Standardize log(Incidence per 100,000)",
  side = 2, outer = TRUE, cex = 1, las = 0
)
graphics::mtext(text = "Time", side = 1, outer = TRUE, cex = 1)
grDevices::dev.off()

### JPEG ####
grDevices::jpeg(
  filename = "./figures/influenza-burden-report-21Feb-eb/fig1.jpeg",
  width = 12, height = 12, units = "in", quality = 100, res = 1080
)
graphics::par(mfrow = c(3, 3), oma = c(2, 2, 0, 0))
for (j in seq_along(flu_obs)) {
  astsa::tsplot(
    x = flu_obs[[j]], main = age_group_titles[j],
    ylab = "", xlab = "", ylim = c(-3, 3), lwd = 2
  )
}
graphics::mtext(
  text = "Standardize log(Incidence per 100,000)",
  side = 2, outer = TRUE, cex = 1, las = 0
)
graphics::mtext(text = "Time", side = 1, outer = TRUE, cex = 1)
grDevices::dev.off()

## Figure 2 ####
## Caption: Standardized incidence per 100,000 cases of influenza for 18 to 34
## age group decomposed into the global trend, seasonal, and anomaly and error
## components. The incidence rate has a natural logarithm transformation to
## produce a stationary time series.

### Getting influenza decomposition ####
flu_18to34 <- flu_extract$age18to34

### Plotting decomposition ####

#### SVG ####
svglite::svglite(
  filename = "./figures/influenza-burden-report-21Feb-eb/fig2.svg",
  width = 12, height = 12
)
signal_extract_plot(
  data = flu_18to34, ylab = "Standardized log(Incidence per 100,000)", lwd = 2
)
grDevices::dev.off()

#### JPEG ####
grDevices::jpeg(
  filename = "./figures/influenza-burden-report-21Feb-eb/fig2.jpeg",
  width = 12, height = 12, units = "in", quality = 100, res = 1080
)
signal_extract_plot(
  data = flu_18to34, ylab = "Standardized log(Incidence per 100,000)", lwd = 2
)
grDevices::dev.off()

## Figure 3 ####
## Caption: Standardized incidence per 100,000 cases of pneumonia for 18 to 34
## age group decomposed into the global trend, seasonal, and anomaly and error
## components. The incidence rate has a natural logarithm transformation to
## produce a stationary time series.

### Importing 18-34 series ####
age18to34 <- readRDS(
  file = "./outputs/age-18to34-ss-series/all_extract.rds"
)

#### Pulling out pneumonia data ####
age18to34_ccs122 <- age18to34$ccs122

### Plotting pneumonia data ####

#### SVG ####
svglite::svglite(
  filename = "./figures/influenza-burden-report-21Feb-eb/fig3.svg",
  width = 12, height = 12
)
signal_extract_plot(
  data = age18to34_ccs122, ylab = "Standardized log(Incidence per 100,000)",
  lwd = 2
)
grDevices::dev.off()

#### JPEG ####
grDevices::jpeg(
  filename = "./figures/influenza-burden-report-21Feb-eb/fig3.jpeg",
  width = 12, height = 12, units = "in", quality = 100, res = 1080
)
signal_extract_plot(
  data = age18to34_ccs122, ylab = "Standardized log(Incidence per 100,000)",
  lwd = 2
)
grDevices::dev.off()

## Figure 4 ####
## Caption: Standardized incidence per 100,000 cases of otitis media and related
## conditions for the 55 to 64 group decomposed into the global trend, seasonal,
## and anomaly and error components. The incidence rate has a natural logarithm
## transformation to produce a stationary time series.

## Importing 55-64series ####
age55to64 <- readRDS(
  file = "./outputs/age-55to64-ss-series/all_extract.rds"
)

### Pulling out pneumonia data ####
age55to64_ccs92 <- age55to64$ccs92

## Plotting pneumonia data ####

### SVG ####
svglite::svglite(
  filename = "./figures/influenza-burden-report-21Feb-eb/fig4.svg",
  width = 12, height = 12
)
signal_extract_plot(
  data = age55to64_ccs92, ylab = "Standardized log(Incidence per 100,000)",
  lwd = 2
)
grDevices::dev.off()

### JPEG ####
grDevices::jpeg(
  filename = "./figures/influenza-burden-report-21Feb-eb/fig4.jpeg",
  width = 12, height = 12, units = "in", quality = 100, res = 1080
)
signal_extract_plot(
  data = age55to64_ccs92, ylab = "Standardized log(Incidence per 100,000)",
  lwd = 2
)
grDevices::dev.off()
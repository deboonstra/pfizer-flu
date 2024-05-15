# The function of this script file is to generate the figures for my version of
# Pfizer report named influenza_burden_report_4_8_24_eb.docx.

# Loading functions and packages ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating tables directory ####
if (!dir.exists("./figures/influenza-burden-report-4-8-24-eb/")) {
  dir.create("./figures/influenza-burden-report-4-8-24-eb/")
}

# Creating figures ####

## Figure 1 ####
## Caption: Standardized log-transformed influenza series for the age groups
## throughout the study period (2001 – 2019). The incidence rate has a natural
## logarithm transformation to produce a stationary time series.

### Importing data ####
age_groups <- c(
  "less2", "2to4", "5to11",
  "12to17", "18to64", "greater64"
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
  "Age: 12-17", "Age: 18-64", "Age: >64"
)

#### Plotting ####

### SVG ####
svglite::svglite(
  filename = "./figures/influenza-burden-report-4-8-24-eb/fig1.svg",
  width = 12, height = 12
)
graphics::par(mfrow = c(6, 1), oma = c(2, 2, 0, 0))
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
  filename = "./figures/influenza-burden-report-4-8-24-eb/fig1.jpeg",
  width = 12, height = 12, units = "in", quality = 100, res = 1080
)
graphics::par(mfrow = c(6, 1), oma = c(2, 2, 0, 0))
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
## Caption: Standardized incidence per 100,000 cases of influenza for 18 to 64
## age group decomposed into the global trend, seasonal, and anomaly and error
## components. The incidence rate has a natural logarithm transformation to
## produce a stationary time series.

### Getting influenza decomposition ####
flu_18to64 <- flu_extract$age18to64

### Plotting decomposition ####

#### SVG ####
svglite::svglite(
  filename = "./figures/influenza-burden-report-4-8-24-eb/fig2.svg",
  width = 12, height = 12
)
signal_extract_plot(
  data = flu_18to64, ylab = "Standardized log(Incidence per 100,000)", lwd = 2
)
grDevices::dev.off()

#### JPEG ####
grDevices::jpeg(
  filename = "./figures/influenza-burden-report-4-8-24-eb/fig2.jpeg",
  width = 12, height = 12, units = "in", quality = 100, res = 1080
)
signal_extract_plot(
  data = flu_18to64, ylab = "Standardized log(Incidence per 100,000)", lwd = 2
)
grDevices::dev.off()

## Figure 3 ####
## Caption: Standardized incidence per 100,000 cases of pneumonia for 18 to 64
## age group decomposed into the global trend, seasonal, and anomaly and error
## components. The incidence rate has a natural logarithm transformation to
## produce a stationary time series.

### Importing 18-64 series ####
age18to64 <- readRDS(
  file = "./outputs/age-18to64-ss-series/all_extract.rds"
)

#### Pulling out pneumonia data ####
age18to64_ccs122 <- age18to64$ccs122

### Plotting pneumonia data ####

#### SVG ####
svglite::svglite(
  filename = "./figures/influenza-burden-report-4-8-24-eb/fig3.svg",
  width = 12, height = 12
)
signal_extract_plot(
  data = age18to64_ccs122, ylab = "Standardized log(Incidence per 100,000)",
  lwd = 2
)
grDevices::dev.off()

#### JPEG ####
grDevices::jpeg(
  filename = "./figures/influenza-burden-report-4-8-24-eb/fig3.jpeg",
  width = 12, height = 12, units = "in", quality = 100, res = 1080
)
signal_extract_plot(
  data = age18to64_ccs122, ylab = "Standardized log(Incidence per 100,000)",
  lwd = 2
)
grDevices::dev.off()

## Figure 4 ####
## Caption: Standardized incidence per 100,000 cases of noninfectious
## gastroenteritis for the 18 to 64 group decomposed into the global trend,
## seasonal, and anomaly and error components. The incidence rate has a
## natural logarithm transformation to produce a stationary time series.
## This secondary disease is an example of disease that is always globally
## correlated with influenza; however, is never locally correlated with
## influenza.

## Importing 18-64 series ####
age18to64 <- readRDS(
  file = "./outputs/age-18to64-ss-series/all_extract.rds"
)

### Pulling out noninfectious gastroenteritis data ####
age18to64_ccs154 <- age18to64$ccs154

## Plotting noninfectious gastroenteritis data ####

### SVG ####
svglite::svglite(
  filename = "./figures/influenza-burden-report-4-8-24-eb/fig4.svg",
  width = 12, height = 12
)
signal_extract_plot(
  data = age18to64_ccs154, ylab = "Standardized log(Incidence per 100,000)",
  lwd = 2
)
grDevices::dev.off()

### JPEG ####
grDevices::jpeg(
  filename = "./figures/influenza-burden-report-4-8-24-eb/fig4.jpeg",
  width = 12, height = 12, units = "in", quality = 100, res = 1080
)
signal_extract_plot(
  data = age18to64_ccs154, ylab = "Standardized log(Incidence per 100,000)",
  lwd = 2
)
grDevices::dev.off()

## Figure 5 ####

### Scatter plots
graphics::par(mfrow = c(4, 1), oma = c(2, 2, 0, 0))
plot(
  x = flu_18to64$ssm, y = age18to64_ccs122$ssm, type = "p", pch = 16,
  xlab = "Influenza", ylab = "Pneumonia", bty = "n", xaxt = "n", yaxt = "n"
)
graphics::axis(side = 1, at = seq(from = -1.45, to = 1.55, by = 0.5))
graphics::axis(side = 2, at = seq(from = -1.5, to = 1.5, by = 0.5))
graphics::mtext(
  text = "Comparison of seasonal components between Influenza and Pneumonia",
  side = 3, outer = FALSE, cex = 1, las = 0, font = 2
)
plot(
  x = flu_18to64$asm, y = age18to64_ccs122$asm, type = "p", pch = 16,
  xlab = "Influenza", ylab = "Pneumonia", bty = "n", xaxt = "n", yaxt = "n"
)
graphics::axis(side = 1, at = seq(from = -0.7, to = 0.8, by = 0.25))
graphics::axis(side = 2, at = seq(from = -0.75, to = 0.75, by = 0.25))
graphics::mtext(
  text = "Comparison of local components between Influenza and Pneumonia",
  side = 3, outer = FALSE, cex = 1, las = 0, font = 2
)
plot(
  x = flu_18to64$ssm, y = age18to64_ccs154$ssm, type = "p", pch = 16,
  xlab = "Influenza", ylab = "Noninfectious Gastroenteritis", bty = "n",
  xaxt = "n", yaxt = "n"
)
graphics::axis(side = 1, at = seq(from = -1.45, to = 1.55, by = 0.5))
graphics::axis(side = 2, at = seq(from = -1, to = 1.5, by = 0.5))
graphics::mtext(
  text = paste0(
    "Comparison of seasonal components between",
    " Influenza and Noninfectious Gastroenteritis"
  ),
  side = 3, outer = FALSE, cex = 1, las = 0, font = 2
)
plot(
  x = flu_18to64$asm, y = age18to64_ccs154$asm, type = "p", pch = 16,
  xlab = "Influenza", ylab = "Noninfectious Gastroenteritis", bty = "n",
  xaxt = "n", yaxt = "n"
)
graphics::axis(side = 1, at = seq(from = -0.7, to = 0.8, by = 0.25))
graphics::axis(side = 2, at = seq(from = -1, to = 2, by = 0.5))
graphics::mtext(
  text = paste0(
    "Comparison of local components between",
    " Influenza and Noninfectious Gastroenteritis"
  ),
  side = 3, outer = FALSE, cex = 1, las = 0, font = 2
)
grDevices::dev.off()

## Time series plots
graphics::par(mfrow = c(4, 1), oma = c(2, 2, 0, 8))
astsa::tsplot(
  x = age18to64_ccs122$ssm,
  ylim = c(-3, 3), lwd = 2, lty = 1,
  ylab = "", xlab = "", main = ""
)
lines(x = flu_18to64$ssm, lwd = 2, lty = 2)
graphics::mtext(
  text = "Comparison of seasonal components between Influenza and Pneumonia",
  side = 3, outer = FALSE, cex = 1, las = 0, font = 2
)
boonstra::legend_right(
  legend = c("Secondary\ndisease", "Influenza"),
  lty = c(1, 2), lwd = 2
)
astsa::tsplot(
  x = age18to64_ccs122$asm,
  lwd = 2, lty = 1,
  ylab = "", xlab = "", main = ""
)
lines(x = flu_18to64$asm, lwd = 2, lty = 2)
graphics::mtext(
  text = "Comparison of local components between Influenza and Pneumonia",
  side = 3, outer = FALSE, cex = 1, las = 0, font = 2
)
astsa::tsplot(
  x = age18to64_ccs154$ssm,
  ylim = c(-3, 3), lwd = 2, lty = 1,
  ylab = "", xlab = "", main = ""
)
lines(x = flu_18to64$ssm, lwd = 2, lty = 2)
graphics::mtext(
  text = paste0(
    "Comparison of seasonal components between",
    " Influenza and Noninfectious Gastroenteritis"
  ),
  side = 3, outer = FALSE, cex = 1, las = 0, font = 2
)
astsa::tsplot(
  x = age18to64_ccs154$asm,
  lwd = 2, lty = 1,
  ylab = "", xlab = "", main = ""
)
lines(x = flu_18to64$asm, lwd = 2, lty = 2)
graphics::mtext(
  text = paste0(
    "Comparison of local components between",
    " Influenza and Noninfectious Gastroenteritis"
  ),
  side = 3, outer = FALSE, cex = 1, las = 0, font = 2
)
graphics::mtext(
  text = "Standardize log(Incidence per 100,000)",
  side = 2, outer = TRUE, cex = 1, las = 0
)
graphics::mtext(text = "Time", side = 1, outer = TRUE, cex = 1)
grDevices::dev.off()
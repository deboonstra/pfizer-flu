# The function of this script file is to generate the tables for my version of
# Pfizer report named influenza_burden_report_4_8_24_eb.docx.

# Loading functions and packages ####
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Creating tables directory ####
if (!dir.exists("./tables/influenza-burden-report-4-8-24-eb/")) {
  dir.create("./tables/influenza-burden-report-4-8-24-eb/")
}

# Importing CCS labels
ccs <- utils::read.csv(file = "./data/ccs-series/ccs_labels.csv")

# Creating tables ####

## Table 2 ####
## Caption: The global correlation values for all the disease that were ever
## locally correlated with influenza based on age group. Diseases with
## missing correlation values are an indication that for the age group of
## interest the disease was not globally and locally correlated with influenza.

### Creating vector of file names to import results ####
tab2_rds <- paste0(
  "./outputs/age-",
  c(
    "less2", "2to4", "5to11",
    "12to17", "18to64", "greater64"
  ),
  "-ss-series/clustering.RData"
)

### Importing data and pulling the diseases of interest ####
tab2 <- lapply(
  X = tab2_rds,
  FUN = function(x) {
    load(file = x)
    ccs <- unique(corr_asm$ccs_code)
    ccs <- ccs[order(ccs)]
    corr_ssm <- subset(
      x = corr_ssm,
      subset = ccs_code %in% ccs,
      select = c(ccs_code, rho_ssm)
    )
    corr_ssm <- corr_ssm[order(ccs), ]
    rho_ssm <- round(corr_ssm$rho_ssm, digits = 3)
    names(rho_ssm) <- paste0("ccs", ccs)
    return(rho_ssm)
  }
)

### Creating table ####
tab2 <- data.frame(dplyr::bind_rows(tab2))
tab2 <- tab2[, order(as.integer(substr(names(tab2), 4, nchar(names(tab2)))))]
row.names(tab2) <- c(
  "Age: <2", "Age: 2-4", "Age: 5-11",
  "Age: 12-17", "Age: 18-64", "Age: >64"
)
tab2 <- t(tab2)
tab2 <- cbind(tab2, substr(row.names(tab2), 4, nchar(row.names(tab2))))
tab2 <- tab2[, c(dim(tab2)[2], seq_len(dim(tab2)[2])[-dim(tab2)[2]])]
colnames(tab2)[1] <- "CCS"
tab2 <- cbind(
  data.frame(CCS = tab2[, 1]),
  subset(x = ccs, subset = ccs$ccs_code %in% tab2[, 1], select = ccs_category),
  tab2[, 2:dim(tab2)[2]]
)
colnames(tab2)[2] <- "Disease"
row.names(tab2) <- NULL

## Table 3
## Caption: A selection correlation values for diseases that are classified as
## peaking in the summer. A disease is classified as a summer disease by having
## a global correlation of -0.6 (i.e., -1 * global correlation threshold) with
## the seasonal component of influenza. The diseases presented here have a
## correlation value of -0.9 or higher.

### Creating vector of file names to import results ####
tab3_rds <- paste0(
  "./outputs/age-",
  c(
    "less2", "2to4", "5to11",
    "12to17", "18to64", "greater64"
  ),
  "-ss-series/clustering.RData"
)

### Importing data and pulling the diseases of interest ####
tab3 <- lapply(
  X = tab3_rds,
  FUN = function(x) {
    load(file = x)
    corr_sum <- subset(
      x = corr_all,
      subset = rho_ssm <= (-1 * 0.9)
    )
    ccs <- unique(corr_sum$ccs_code)
    ccs <- ccs[order(ccs)]
    corr_sum <- subset(
      x = corr_sum,
      select = c(ccs_code, rho_ssm)
    )
    corr_sum <- corr_sum[order(ccs), ]
    rho_sum <- round(corr_sum$rho_ssm, digits = 3)
    names(rho_sum) <- paste0("ccs", ccs)
    return(rho_sum)
  }
)

### Creating table ####
tab3 <- data.frame(dplyr::bind_rows(tab3))
tab3 <- tab3[, order(as.integer(substr(names(tab3), 4, nchar(names(tab3)))))]
row.names(tab3) <- c(
  "Age: <2", "Age: 2-4", "Age: 5-11",
  "Age: 12-17", "Age: 18-64", "Age: >64"
)
tab3 <- t(tab3)
tab3 <- cbind(tab3, substr(row.names(tab3), 4, nchar(row.names(tab3))))
tab3 <- tab3[, c(dim(tab3)[2], seq_len(dim(tab3)[2])[-dim(tab3)[2]])]
colnames(tab3)[1] <- "CCS"
tab3 <- cbind(
  data.frame(CCS = tab3[, 1]),
  subset(x = ccs, subset = ccs$ccs_code %in% tab3[, 1], select = ccs_category),
  tab3[, 2:dim(tab3)[2]]
)
colnames(tab3)[2] <- "Disease"
row.names(tab3) <- NULL


## Table 4 ####
## Caption: The local correlation values for all the disease that were ever
## locally correlated with influenza given based on age group. Diseases with
## missing correlation values are an indication that for the age group of
## interest the disease was not globally and locally correlated with influenza.

### Creating vector of file names to import results ####
tab4_rds <- paste0(
  "./outputs/age-",
  c(
    "less2", "2to4", "5to11",
    "12to17", "18to64", "greater64"
  ),
  "-ss-series/clustering.RData"
)

### Importing data and pulling the diseases of interest ####
tab4 <- lapply(
  X = tab4_rds,
  FUN = function(x) {
    load(file = x)
    ccs <- unique(corr_asm$ccs_code)
    ccs <- ccs[order(ccs)]
    corr_asm <- subset(
      x = corr_asm,
      select = c(ccs_code, rho_asm)
    )
    corr_asm <- corr_asm[order(ccs), ]
    rho_asm <- round(corr_asm$rho_asm, digits = 3)
    names(rho_asm) <- paste0("ccs", ccs)
    return(rho_asm)
  }
)

### Creating table ####
tab4 <- data.frame(dplyr::bind_rows(tab4))
tab4 <- tab4[, order(as.integer(substr(names(tab4), 4, nchar(names(tab4)))))]
row.names(tab4) <- c(
  "Age: <2", "Age: 2-4", "Age: 5-11",
  "Age: 12-17", "Age: 18-64", "Age: >64"
)
tab4 <- t(tab4)
tab4 <- cbind(tab4, substr(row.names(tab4), 4, nchar(row.names(tab4))))
tab4 <- tab4[, c(dim(tab4)[2], seq_len(dim(tab4)[2])[-dim(tab4)[2]])]
colnames(tab4)[1] <- "CCS"
tab4 <- cbind(
  data.frame(CCS = tab4[, 1]),
  subset(x = ccs, subset = ccs$ccs_code %in% tab4[, 1], select = ccs_category),
  tab4[, 2:dim(tab4)[2]]
)
colnames(tab4)[2] <- "Disease"
row.names(tab4) <- NULL

## Table 5 ####
## Caption: The percent reduction in incidence rate for the diseases that are
## globally and locally clustered with influenza given a reduction in the
## influenza incidence rate for primary and seconday disease cohorts,
## where L%, R% refers to the percent reduction in incidence rate for a disease
## assuming the influenza incidence rate decreased by 20%, 60%.
## Diseases with no reduction in incidence rate reported indicate the
## disease was not globally and locally assoicated with influenza, which
## means any attributable risk estimate would not be statistically significant.

### Creating vector of file names to import results ####
tab5_rds <- paste0(
  "./outputs/age-",
  c(
    "less2", "2to4", "5to11",
    "12to17", "18to64", "greater64"
  ),
  "-ss-series/incidence_change.rds"
)

### Importing data and pulling the diseases of interest ####
tab5 <- lapply(
  X = tab5_rds,
  FUN = function(x) {
    dat <- readRDS(file = x)
    ccs <- unique(dat$ccs_code)
    ccs <- ccs[order(ccs)]
    dat20 <- subset(
      x = dat,
      subset = pct == 0.2,
      select = c(ccs_code, relative_change)
    )
    dat20 <- dat20[order(dat20$ccs_code), ]
    dat20 <- format(100 * round(dat20$relative_change, 5), nsmall = 3)
    dat60 <- subset(
      x = dat,
      subset = pct == 0.6,
      select = c(ccs_code, relative_change)
    )
    dat60 <- dat60[order(dat60$ccs_code), ]
    dat60 <- format(100 * round(dat60$relative_change, 5), nsmall = 3)
    change <- paste0(dat20, "%, ", dat60, "%")
    names(change) <- paste0("ccs", ccs)
    return(change)
  }
)

### Creating table ####
tab5 <- data.frame(dplyr::bind_rows(tab5))
tab5 <- tab5[, order(as.integer(substr(names(tab5), 4, nchar(names(tab5)))))]
row.names(tab5) <- c(
  "Age: <2", "Age: 2-4", "Age: 5-11",
  "Age: 12-17", "Age: 18-64", "Age: >64"
)
tab5 <- t(tab5)
tab5 <- cbind(tab5, substr(row.names(tab5), 4, nchar(row.names(tab5))))
tab5 <- tab5[, c(dim(tab5)[2], seq_len(dim(tab5)[2])[-dim(tab5)[2]])]
colnames(tab5)[1] <- "CCS"
tab5 <- cbind(
  data.frame(CCS = tab5[, 1]),
  subset(x = ccs, subset = ccs$ccs_code %in% tab5[, 1], select = ccs_category),
  tab5[, 2:dim(tab5)[2]]
)
colnames(tab5)[2] <- "Disease"
row.names(tab5) <- NULL


# Exporting tables ####
utils::write.csv(
  x = tab2,
  file = "./tables/influenza-burden-report-4-8-24-eb/tab2.csv",
  row.names = FALSE, na = ""
)
utils::write.csv(
  x = tab3,
  file = "./tables/influenza-burden-report-4-8-24-eb/tab3.csv",
  row.names = FALSE, na = ""
)
utils::write.csv(
  x = tab4,
  file = "./tables/influenza-burden-report-4-8-24-eb/tab4.csv",
  row.names = FALSE, na = ""
)
utils::write.csv(
  x = tab5,
  file = "./tables/influenza-burden-report-4-8-24-eb/tab5.csv",
  row.names = FALSE, na = ""
)
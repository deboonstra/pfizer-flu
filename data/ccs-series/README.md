The following summarizes the contents of the ccs_series folder.


`ccs_labels.csv`

This file contains the labels for the various CCS categories. The dataset contains the following variables:
  - ccs_code - CCS code
  - ccs_category - Label/description for the CCS category

`monthly_ccae_mdcr.csv`

This dataset contains monthly time series of CCS counts for all patients in the CCAE and MDCR databases. This dataset was created by collecting all diagnoses in the inpatient, outpatient services, facility headers and inpatient services table. The diagnosis codes were crosswalked with corresponding CCS categories, then the distinct enrollids, month, and year were counted for each CCS code. Note: the script for generating these series can be found in the repo aarmiller/pfizer_flu/extract_dx/extract_ccs_monthly.R

This dataset contains the following variables:
- ccs_code - CCS code
- year - Year
- month - Month
- n - Number of enrollees with a diagnosis with the corresponding CCS code in the given year and month.
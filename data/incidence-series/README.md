The following summarizes the contents of the incidence_series folder.

`monthly_incidence_all.RData`

This file contains monthly incidence for all patients in the MDCR and CCAE population. The following variables are contained in this file:

- setting - the type of visit setting included. Currently there are two options: (1) inpatient + outpatient and (2) inpatient only
- population - the study population included. Currently there is only one study population (ccae + mdcr). Note, later we will include the medicaid population
- series - the type of incidence series calculated. There are currently two options: (1) unweighted and (2) census weighted. The census weighted is weighted according to the age and sex groups in the 2020 census.
- ccs_code - code for ccs category
- year - year
- month - month
- incidence - incidence in cases per 100,000 enrollees
- month_start - date of the start of month
- month_end - date of the end of month
- month_mid - middle date of the month

`monthly_incidence_cohorts.RData`

This file contains monthly incidence for all for the three different study cohorts who can be continuously followed across time. Currently there are three different study cohorts: (1) ICD-9 cohort, (2) ICD-10 cohort, (3) ICD crossover-cohort. See below for more details on the three different cohorts. The following variables are contained in this file:
- setting - the type of visit setting included. Currently there are two options: (1) inpatient + outpatient and (2) inpatient only
- population - the study population included. Currently there are the three different cohorts described below (icd9, icd10, and icd_crossover).
- series - the type of incidence series calculated. There are currently two options: (1) unweighted and (2) census weighted. The census weighted is weighted according to the age and sex groups in the 2020 census.
- ccs_code - code for ccs category
- year - year
- month - month
- incidence - incidence in cases per 100,000 enrollees
- month_start - date of the start of month
- month_end - date of the end of month
- month_mid - middle date of the month


Summary of three different study cohorts.

We have identified the following 3 study cohorts who are continuously enrolled across a given span of time.

1) ICD-9 Cohort - contains individuals who can be continuosly followed from 01-01-2003 through 9-30-2013. There are a total of 1,185,114 individuals included in this cohort.
2) ICD-10 Cohort - contains individuals who can be continuosly followed from 01-01-2015 through 12-31-2019. There are a total of 7,698,657 individuals included in this cohort.
3) ICD-Crossover Cohort - contains individuals who can be continuosly followed from 01-01-2011 through 12-31-2019. There are a total of 3,592,988 individuals included in this cohort.
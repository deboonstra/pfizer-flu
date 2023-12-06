The following summarizes the contents of the enrollment_series folder.

`daily_enroll_all.csv`

This file contains daily enrollment counts for all three sources (ccae, mdcr, and medicaid). This file contains the following variables:
- date - Date
- total_enroll - Total enrollment on the given date and source
- source - Data source (ccae, mdcr, or medicaid)

`monthly_enroll_ccae_mdcr.csv`

This file contains average monthly enrollment counts for the ccae and mdcr populations combined. This dataset was constucted by averaging the daily enrollment summed across the ccae and mdcr populations each day in a given month. This dataset contains the following variables:
  - year - Year
  - month - Month
  - average_enroll - average daily enrollment in the given month
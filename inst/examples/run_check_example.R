# Check social contact datasets with contactdataformat: example script for RStudio
#
# Open this file in RStudio, set `data_dir` below and run it (Cmd/Ctrl + Shift +
# Enter, or line by line). The data are only read; the reports are written to
# `output_dir`, never into a dataset folder.
#
# `data_dir` is either
#   - one dataset folder (the folder holds NAMETAG_*.csv files), or
#   - a folder with one sub-folder per dataset (one NAMETAG per sub-folder).
#
# To find this script again: system.file("examples", "run_check_example.R",
#                                         package = "contactdataformat")

## ---- 1. Package: install if needed, then load --------------------------- ##

if (!requireNamespace("contactdataformat", quietly = TRUE)) {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  pak::pak("lwillem/contactdataformat")
}
# To update to the latest version, run once:
# pak::pak("lwillem/contactdataformat", upgrade = TRUE)

library(contactdataformat)

## ---- 2. Settings -------------------------------------------------------- ##

data_dir   <- "path/to/your/datasets"   # <- change this
output_dir <- NULL                      # NULL: a folder 'check_reports' next to / inside data_dir
smoke_test <- FALSE                     # TRUE: also load each dataset with socialmixr (slower)

## ---- 3. Run the checks, write the reports, print the summary ------------ ##

run <- run_check(data_dir, output_dir = output_dir, smoke_test = smoke_test)

## ---- 4. Look at the results --------------------------------------------- ##

run$summary                      # one row per dataset: status, errors, warnings, ...
run$output_dir                   # where the reports (.csv and .md) were written

# The full report of one dataset (replace the name by one in run$summary$dataset)
# run$reports[["NAME_2025_COUNTRY"]]
# subset(as.data.frame(run$reports[["NAME_2025_COUNTRY"]]), severity == "error")

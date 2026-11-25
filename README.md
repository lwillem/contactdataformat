# contactdataformat

Check whether a social contact survey dataset follows the
[SOCRATES data-formatting guidelines](https://socialcontactdata.org), the file
layout read by the [socialmixr](https://github.com/epiforecasts/socialmixr)
package and the SOCRATES tools.

> **Status: first working version for guideline 1.0 (draft).** The schema
> follows the recommended choices of the guideline review (e.g. `cnt_id` with
> legacy alias `cont_id`, `cnt_otherplace`, `part_age_exact`); open choices are listed by
> `open_decisions()` and can be changed in the schema. The package is
> developed standalone and may later move into `contactsurveys` or `socialmixr`.

## Installation

```r
# install.packages("pak")
pak::pak("lwillem/contactdataformat")
```

## Usage

```r
library(contactdataformat)

run <- run_check("path/to/folder")   # one dataset folder, or a folder with one sub-folder per dataset:
                             # checks, writes a report per dataset + check_summary.csv, prints the summary
# example script for RStudio: system.file("examples", "run_check_example.R", package = "contactdataformat")

report <- check_contact_data("path/to/NAMETAG_dataset_folder")   # one dataset, nothing written
report                       # status PASS / PASS WITH WARNINGS / MANDATORY UPDATES NEEDED (errors must be fixed; a record with only warnings can be submitted)
summary(report)
as.data.frame(report)        # findings: rule_id, severity, file, column, ...
report_write(report, "report.md")   # or .csv / .json

list_rules()                 # rule catalogue (classes A, X, H, M)
open_decisions("open")       # guideline choices that are still open
check_contact_data(path, waive = c(R21 = "group contacts are documented"))   # waive a rule, with a reason
check_contact_data(path, smoke_test = TRUE)   # also load with socialmixr
```

`check_tables()` runs the same rules on data frames in memory;
`use_template()` writes an empty submission skeleton; a record carries no
study metadata file (the study information is in the Zenodo record and the
dictionary) and at most one `.json`, because socialmixr reads the first
`.json` file of a record as its citation; `example_survey()` and
`write_survey()` create a synthetic valid dataset.

## What is checked

| Class | Meaning | Examples |
|---|---|---|
| A | automatic, schema driven | file names, columns, types (booleans are `TRUE`/`FALSE` only, `0/1` is an error), allowed values (`country` is required and never `NA`; text for several countries or a region is a warning), age encoding, location logic, survey-day dates, key uniqueness, privacy scan, socialmixr smoke test and merge simulation |
| X | automatic, cross-table | extra files vs common files, foreign keys, dictionary coverage, household size, panel ids (one individual per `panel_id`: same gender, plausible exact age), record hygiene (no CSV that socialmixr would merge), identifier types |
| H | heuristic, mostly warnings | contact counts, share of missing ages and of missing survey dates, implausible household size (above 20), columns that are entirely `NA`, personal-identifier column names (direct identifiers such as e-mail or phone are errors, the word `name` alone a warning) |
| M | manual checklist | NAMETAG content, documented conventions, Zenodo record content |

The checker also reads a dictionary with a deviating file name (for example
`NAMETAG_data_dictionary.xlsx`), reports the name as an error and still checks
its content, and names near-miss column names (`Cnt_Gender`) in its messages.

Rules R28-R33 (content of a restricted geography file) and R37 (covered by R05) are in the catalogue but not yet implemented. Rule R36 (Zenodo metadata) gives no finding for a local folder; it checks the `NAMETAG_reference.json` of a folder downloaded with contactsurveys, and `check_zenodo_record()` (optional) checks a published record by DOI. **Rule R27 flags ZIP codes, addresses and coordinates in a
public record**, including a restricted geography file placed in the same
folder, because `socialmixr` merges every CSV that shares a column name with the
main tables.

## Privacy

The package ships no real survey data. Tests use synthetic fixtures. To check a
local export of real data, point to it with an environment variable (the
relevant tests are skipped when it is unset):

```sh
CONTACTDATAFORMAT_FLANDERS_DIR=/path/to/export Rscript -e 'devtools::test()'
```

## Versioning

The package `MAJOR.MINOR` equals the guideline version it implements
(`1.0.z` implements guideline `1.0`); the third number counts checker fixes.
Older schemas stay bundled (`guideline_versions()`), and every report records
the package version, guideline version and schema hash.

## Licence and contributing

MIT. See [CONTRIBUTING.md](CONTRIBUTING.md).

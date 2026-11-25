# contactdataformat (development version)

## Guideline version 1.0 (draft)

* First working version. The package version follows the guideline version it
  implements: `MAJOR.MINOR` equals the bundled schema version, the third number
  counts checker fixes (`1.0.z`).
* Adds `check_contact_data()` and `check_tables()` returning a
  `socrates_report`, with `print()`, `summary()` and `as.data.frame()` methods
  and `report_write()` to Markdown, CSV and JSON.
* Adds the bundled schema `socrates-guidelines-1.0.yaml` as the single source
  of truth, and `list_rules()`, `guideline_versions()`, `open_decisions()`.
* Implements automatic (class A), cross-table (class X) and heuristic (class H)
  rules, a manual checklist (class M), `waive` with reasons, and a privacy rule
  (R27) that keeps restricted geography out of the public record.
* Aligned with sections 6A.7/6A.8 of the guideline review: optional columns
  may be absent, column order is not enforced by default, booleans are
  `TRUE`/`FALSE` (legacy `0/1` gives a warning, see below), study metadata is
  a YAML file (socialmixr reads the first `.json` of a record as its
  citation), and record-hygiene rules R34 (CSV files that
  socialmixr would merge), R35 (merge simulation, with `smoke_test = TRUE`),
  R38 (identifier types) and R39 (`diary_weight`) are implemented.
* Study metadata file dropped (25 Nov 2026): no `NAMETAG_metadata.yaml` is
  required, rule R24 and the `metadata` argument of `check_tables()` are
  removed, and R27 no longer has a declared geography level (municipality,
  city and NIS columns are always flagged). The manual item for the Zenodo
  record content is now R25g.
* NAMETAG form is a recommendation (25 Nov 2026): R02 is a note for a name
  that is not `NAME_YEAR_COUNTRY[_SUFFIX]` (including the year-first legacy
  prefix) and an error only if the name cannot be a file name prefix. The same
  prefix on every file stays a hard rule (R01): files with different prefixes, and a CSV outside the file set whose name does not start with the NAMETAG, are errors; other CSV files that start with the NAMETAG (e.g. `NAMETAG_timeuse.csv`) stay a warning.
* Dictionary format enforced (25 Nov 2026): the dictionary of the published record must be `.xlsx` or `.xls`; a `.csv` dictionary is still read, so the dictionary checks run, but is an R01 error. Country codes stay ISO 3166-1
  alpha-2 only (R09).
* Date format (25 Nov 2026): `sday_id` is `YYYYMMDD` (eight digits, as in
  POLYMOD), not ISO `YYYY-MM-DD`; ISO and dotted dates are errors.
* Contact ID: `cnt_id` is canonical and `cont_id` is an accepted legacy alias
  (also `part_age` for `part_age_exact`); the schema `aliases` field lists
  accepted spellings, `common_mistakes` only gives hints. New rule R40: a
  record must use one spelling of the contact ID in `contact_common` and
  `contact_extra`.
* Booleans: `TRUE`/`FALSE` is the only canonical form for new datasets.
  Integer or quoted `0`/`1` (legacy: POLYMOD, CoMix, Zenodo v1.2; also `1`/`2`
  for `phys_contact`) is reported as a warning by R08 (variant
  `legacy_boolean`): socialmixr still reads it, so published records stay
  checkable. Text codes (`Y`/`N`, `T`/`F`, `yes`/`no`) remain errors. The
  severity is a schema setting.
* Optional `holiday` column in the survey-day file (`sday`): TRUE if the diary
  day falls in a school holiday that applies to the participant (regional,
  not a public-holiday flag), FALSE otherwise, `NA` if the date is unknown.
  It follows the boolean rule (R08: `TRUE`/`FALSE`, `0`/`1` accepted, `Y`/`N`
  is an error); quoted `"0"`/`"1"` strings as in Zenodo v1.2 give the legacy
  warning described below. When the column is present, a manual checklist item
  (R25) asks that its definition is documented in the dictionary.
  `example_survey(with_holiday = TRUE)` adds it to the synthetic example.
* Dictionary: `.xls` is accepted like `.xlsx` (and `.csv`); the reader also
  understands the header names of the Zenodo dictionary template ("Data
  repository name", "Variable label", "Variable Type", "Value labels"),
  folds value labels spread over several rows into the variable above, and
  reports an unreadable spreadsheet as a note instead of failing.
* Additions from the comparison with `other_code/validate_socrates.R` and the
  three test datasets (25 Nov 2026):
  * R42 (warning): `hh_size` above `options$max_plausible_hh_size` (20).
  * R07: missing-value tokens are matched case-insensitively and trimmed;
    `unknown`, `missing`, `n.a.` added to the token list.
  * R01/read: a dictionary that starts with the NAMETAG but is named otherwise
    (`NAMETAG_data_dictionary.xlsx`, `NAMETAG_DICTIONARY.xlsx`) is reported
    (error) and still read, so R14/R15 run; it no longer produces a second
    NAMETAG candidate.
  * R05: the "missing column" message names a near-miss column (case, spaces,
    punctuation, one character off), not only the listed common mistakes.
  * R20 (variant `summary`, note): number of panel individuals and records per
    individual. `panel_id` is also read from `sday` when a submission put it
    there.
  * R43 (note; warning when all location columns are `NA`): columns that are
    entirely `NA`.
  * R44 (warning): share of participants without `sday_id` above
    `options$max_share_missing_sday` (5%), with the number that reported
    contacts.
  * R45 (warning): one `panel_id` keeps its `part_gender`, and its exact age
    follows the survey dates (no decrease, no larger jump than the time
    elapsed).
  * R10 (variant `empty_interval`, warning): equal lower and upper bound; the
    upper bound is exclusive, so the interval is empty.
* Catalogued but not yet implemented: R28-R33 (content of a restricted
  geography file), R36 (Zenodo record metadata, needs network) and R37
  (silent-failure columns, currently covered by R05).

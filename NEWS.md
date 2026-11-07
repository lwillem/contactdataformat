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
* Catalogued but not yet implemented: R28-R33 (content of a restricted
  geography file), R36 (Zenodo record metadata, needs network) and R37
  (silent-failure columns, currently covered by R05).

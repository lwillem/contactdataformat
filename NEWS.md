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
  may be absent, column order is not enforced by default, `0/1` booleans are
  accepted, study metadata is a YAML file (socialmixr reads the first `.json`
  of a record as its citation), and record-hygiene rules R34 (CSV files that
  socialmixr would merge), R35 (merge simulation, with `smoke_test = TRUE`),
  R38 (identifier types) and R39 (`diary_weight`) are implemented.
* Contact ID: `cnt_id` is canonical and `cont_id` is an accepted legacy alias
  (also `part_age` for `part_age_exact`); the schema `aliases` field lists
  accepted spellings, `common_mistakes` only gives hints. New rule R40: a
  record must use one spelling of the contact ID in `contact_common` and
  `contact_extra`.
* Catalogued but not yet implemented: R28-R33 (content of a restricted
  geography file), R36 (Zenodo record metadata, needs network) and R37
  (silent-failure columns, currently covered by R05).

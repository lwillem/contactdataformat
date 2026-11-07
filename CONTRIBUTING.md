# Contributing to contactdataformat

Thank you for helping. A few conventions keep the package consistent with
`socialmixr` and `contactsurveys`.

* **The schema is the specification.** File names, columns, types, allowed
  values, keys, thresholds and rule severities live in
  `inst/schema/socrates-guidelines-<version>.yaml`. Change the schema first;
  only write R code for logic that YAML cannot express. Rule logic lives in
  `R/rules_*.R` as `rule_Rxx()` functions that receive the checking context
  and return findings.
* **Versioning.** `MAJOR.MINOR` of the package equals the guideline version
  that the newest schema implements. A guideline change that breaks existing
  datasets bumps MAJOR; an additive change bumps MINOR; checker fixes bump the
  third number. Old schemas stay bundled.
* **Tests.** testthat (3rd edition). Every rule needs a test with a valid
  fixture and a deliberately faulty one. Fixtures are built in code from
  `example_survey()`.
* **Privacy.** Never commit real survey data or restricted geography files
  (ZIP codes, coordinates). Tests use synthetic data only. Local checks
  against real data read the folder from the environment variable
  `CONTACTDATAFORMAT_FLANDERS_DIR` and are skipped when it is unset.
* **Style.** Run `lintr::lint_package()` and keep code lintr-clean; use
  snake_case names (no dotted argument names), roxygen2 with markdown, US
  English, UTF-8.
* **Checks.** Run `devtools::document()`, `devtools::test()` and
  `R CMD check` (or `devtools::check()`) before opening a pull request, and
  add an entry to `NEWS.md`.

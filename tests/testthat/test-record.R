test_that("R34: CSV files that socialmixr would merge unintentionally", {
  tables <- valid_survey()

  dir <- survey_dir(tables)
  helper <- data.frame(part_id = tables$participant$part_id, score = 1)
  write_csv_utf8(helper, file.path(dir, "DOE_2012_BELGIUM_scores.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R34")
  expect_msg(report, "R34", "merges it")

  dir <- survey_dir(tables)
  lookup <- data.frame(code = 1:3, label = c("a", "b", "c"))
  write_csv_utf8(lookup, file.path(dir, "DOE_2012_BELGIUM_lookup.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_false("R34" %in% hits(report, "error"))
  expect_hit(report, "R01", "warning")

  dir <- survey_dir(tables)
  repeated <- data.frame(part_id = rep(tables$participant$part_id[1], 3),
                         wave = 1:3)
  write_csv_utf8(repeated, file.path(dir, "DOE_2012_BELGIUM_waves.csv"))
  expect_msg(check_contact_data(dir, quiet = TRUE), "R34", "replicate")

  dir <- survey_dir(tables)
  write_csv_utf8(lookup, file.path(dir, "DOE_2012_BELGIUM_lookup_a.csv"))
  write_csv_utf8(lookup, file.path(dir, "DOE_2012_BELGIUM_lookup_b.csv"))
  expect_msg(check_contact_data(dir, quiet = TRUE), "R34", "identical columns")

  dir <- survey_dir(tables)
  writeLines("{}", file.path(dir, "reference.json"))
  expect_false("R34" %in% hits(check_contact_data(dir, quiet = TRUE), "error"))
  writeLines("{}", file.path(dir, "other.json"))
  expect_msg(check_contact_data(dir, quiet = TRUE), "R34", "JSON")
})

test_that("R35: merge simulation reports unintended merges", {
  skip_if_not_installed("socialmixr", "0.7.0")
  tables <- valid_survey()
  dir <- survey_dir(tables)
  repeated <- data.frame(part_id = rep(tables$participant$part_id[1], 3),
                         wave = 1:3)
  write_csv_utf8(repeated, file.path(dir, "DOE_2012_BELGIUM_waves.csv"))
  report <- check_contact_data(dir, smoke_test = TRUE, quiet = TRUE)
  expect_hit(report, "R35")
})

test_that("R35: a CSV dictionary is noted, not an error", {
  skip_if_not_installed("socialmixr", "0.7.0")
  dir <- survey_dir()
  report <- check_contact_data(dir, smoke_test = TRUE, quiet = TRUE)
  expect_hit(report, "R35", "note")
  expect_equal(report$status, "PASS")
})

test_that("R38: identifier types agree across files", {
  tables <- valid_survey()
  mixed <- tables
  numeric_ids <- seq_len(nrow(mixed$participant))
  mixed$participant$part_id <- numeric_ids
  mixed$sday$part_id <- numeric_ids
  mixed$participant_extra$part_id <- numeric_ids
  # the contact file still holds the string ids
  report <- check_survey(mixed)
  expect_hit(report, "R38")
  expect_msg(report, "R38", "part_id")

  all_numeric <- mixed
  map <- stats::setNames(numeric_ids, tables$participant$part_id)
  all_numeric$contact$part_id <- unname(map[tables$contact$part_id])
  expect_false("R38" %in% hits(check_survey(all_numeric), "error"))
})

test_that("R39: diary_weight is numeric without missing values", {
  tables <- valid_survey()
  tables$participant_extra$diary_weight <- 1
  tables <- redocument(tables)
  expect_clean(check_survey(tables))

  missing <- tables
  missing$participant_extra$diary_weight[2] <- NA
  expect_hit(check_survey(missing), "R39")

  text <- tables
  text$participant_extra$diary_weight <- "high"
  expect_hit(check_survey(text), "R39")
})

test_that("integer 0/1 booleans are accepted and count as TRUE in R13", {
  tables <- valid_survey()
  loc <- c("cnt_home", "cnt_work", "cnt_school", "cnt_transport",
           "cnt_leisure", "cnt_otherplace")
  tables$contact[loc] <- lapply(tables$contact[loc], as.integer)
  expect_clean(check_survey(tables))
  tables$contact[1, loc] <- 0L
  expect_hit(check_survey(tables), "R13")
})

test_that("metadata is a YAML file, never JSON", {
  dir <- survey_dir()
  expect_true(file.exists(file.path(dir, "DOE_2012_BELGIUM_metadata.yaml")))
  expect_length(list.files(dir, pattern = "\\.json$"), 0)
})

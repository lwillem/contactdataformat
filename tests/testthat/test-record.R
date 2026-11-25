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

test_that("R35: a CSV dictionary is noted; R01 makes it an error", {
  skip_if_not_installed("socialmixr", "0.7.0")
  dir <- survey_dir(xlsx_dictionary = FALSE)
  report <- check_contact_data(dir, smoke_test = TRUE, quiet = TRUE)
  expect_hit(report, "R35", "note")
  expect_msg(report, "R01", "must be a spreadsheet")
  expect_equal(report$status, "FAIL")
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

test_that("legacy 0/1 booleans are an error, and still count as TRUE in R13", {
  tables <- valid_survey()
  loc <- c("cnt_home", "cnt_work", "cnt_school", "cnt_transport",
           "cnt_leisure", "cnt_otherplace")
  tables$contact[loc] <- lapply(tables$contact[loc], as.integer)
  report <- check_survey(tables)
  expect_hit(report, "R08", "error")
  expect_msg(report, "R08", "Legacy boolean")
  expect_false("R13" %in% hits(report))
  expect_equal(report$status, "FAIL")
  tables$contact[1, loc] <- 0L
  expect_hit(check_survey(tables), "R13")
})

test_that("legacy phys_contact 1/2, Y/N and T/F are errors", {
  tables <- valid_survey()
  legacy <- tables
  legacy$contact$phys_contact <- ifelse(legacy$contact$phys_contact, 1L, 2L)
  report <- check_survey(legacy)
  expect_hit(report, "R08", "error")
  expect_msg(report, "R08", "Legacy boolean")
  for (code in list(c("Y", "N"), c("T", "F"), c("yes", "no"))) {
    bad <- tables
    bad$contact$phys_contact <- ifelse(bad$contact$phys_contact, code[1], code[2])
    expect_hit(check_survey(bad), "R08")
  }
  # 2 is only a legacy value for phys_contact, not for other booleans
  other <- tables
  other$contact$cnt_home <- ifelse(other$contact$cnt_home, 1L, 2L)
  expect_hit(check_survey(other), "R08")
})

test_that("the severity of the legacy boolean error is a schema setting", {
  tables <- valid_survey()
  tables$contact$cnt_home <- as.integer(tables$contact$cnt_home)
  schema <- read_schema()
  idx <- which(purrr::map_chr(schema$rules, "id") == "R08")
  schema$rules[[idx]]$severities$legacy_boolean <- "warning"
  report <- check_survey(tables, schema = schema)
  expect_hit(report, "R08", "warning")
  expect_false("R08" %in% hits(report, "error"))
})

# Contact ID: cnt_id canonical, cont_id accepted legacy alias (R40) ------------

rename_contact_key <- function(tables, from, to, which = c("contact", "contact_extra")) {
  for (name in which) {
    names(tables[[name]])[names(tables[[name]]) == from] <- to
  }
  tables
}

test_that("a dataset with cnt_id passes", {
  tables <- valid_survey()
  expect_true("cnt_id" %in% names(tables$contact))
  expect_clean(check_survey(tables))
  expect_clean(check_contact_data(survey_dir(tables), quiet = TRUE))
})

test_that("a dataset with the legacy cont_id passes in memory and on disk", {
  legacy <- rename_contact_key(valid_survey(), "cnt_id", "cont_id")
  # the dictionary may document either spelling
  legacy$dictionary$variable[legacy$dictionary$variable == "cnt_id"] <- "cont_id"
  report <- check_survey(legacy)
  expect_clean(report)
  expect_hit(report, "R05", "note")
  expect_msg(report, "R05", "legacy spelling")
  expect_clean(check_contact_data(survey_dir(legacy), quiet = TRUE))
})

test_that("legacy cont_id: key rules still apply", {
  legacy <- rename_contact_key(valid_survey(), "cnt_id", "cont_id")
  legacy$contact$cont_id[3] <- legacy$contact$cont_id[2]
  report <- check_survey(legacy)
  expect_hit(report, "R16")
  expect_equal(only_rule(report, "R16")$column[1], "cnt_id")
})

test_that("R40: a mixed pair of spellings is an error", {
  mixed <- rename_contact_key(valid_survey(), "cnt_id", "cont_id", "contact")
  report <- check_survey(mixed)
  expect_hit(report, "R40")
  expect_msg(report, "R40", "cont_id")
  expect_false(report$passed)

  other_way <- rename_contact_key(valid_survey(), "cnt_id", "cont_id",
                                  "contact_extra")
  expect_hit(check_survey(other_way), "R40")

  # also when the files are read from disk
  expect_hit(check_contact_data(survey_dir(mixed), quiet = TRUE), "R40")

  # the same spelling twice is fine
  expect_false("R40" %in% hits(check_survey(valid_survey())))
})

test_that("both cnt_id and cont_id in one file is not an accepted alias use", {
  both <- valid_survey()
  both$contact$cont_id <- both$contact$cnt_id
  expect_hit(check_survey(both), "R05")
})

test_that("part_age is an accepted alias of part_age_exact", {
  tables <- valid_survey()
  names(tables$participant)[names(tables$participant) == "part_age_exact"] <- "part_age"
  report <- check_survey(tables)
  expect_false("R05" %in% hits(report, "error"))
  expect_msg(report, "R05", "legacy spelling")
})

test_that("cnt_otherplace stays the canonical name, cnt_others is an error", {
  tables <- valid_survey()
  names(tables$contact)[names(tables$contact) == "cnt_otherplace"] <- "cnt_others"
  report <- check_survey(tables)
  expect_hit(report, "R05")
  expect_msg(report, "R05", "cnt_others")
})

test_that("R40 works when only one of the files uses the legacy spelling", {
  # common uses the legacy cont_id, extra has no renamed column at all
  tables <- rename_contact_key(valid_survey(), "cnt_id", "cont_id", "contact")
  report <- check_survey(tables)
  expect_false(any(grepl("could not be evaluated", report$findings$message)))
  expect_hit(report, "R40")
  # no extra file at all
  tables$contact_extra <- NULL
  expect_false(any(grepl("could not be evaluated",
                         check_survey(tables)$findings$message)))
})

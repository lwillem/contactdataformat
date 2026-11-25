test_that("R01: required files, file names and unrecognised files", {
  tables <- valid_survey()

  dir <- survey_dir(tables)
  file.remove(file.path(dir, "DOE_2012_BELGIUM_hh_common.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R01")
  expect_msg(report, "R01", "hh_common")

  dir <- survey_dir(tables)
  file.remove(file.path(dir, "DOE_2012_BELGIUM_dictionary.csv"))
  expect_hit(check_contact_data(dir, quiet = TRUE), "R01")

  dir <- survey_dir(tables)
  file.copy(file.path(dir, "DOE_2012_BELGIUM_sday.csv"),
            file.path(dir, "OTHER_2012_BELGIUM_sday.csv"))
  report <- check_contact_data(dir, nametag = "DOE_2012_BELGIUM", quiet = TRUE)
  expect_hit(report, "R01")
  expect_msg(report, "R01", "does not start")

  dir <- survey_dir(tables)
  writeLines(c("a,b", "1,2"), file.path(dir, "DOE_2012_BELGIUM_timeuse.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R01", "warning")
  expect_true(report$passed)

  expect_hit(check_survey(modifyList(tables, list(hh = NULL))), "R01")
})

test_that("R02: NAMETAG syntax", {
  tables <- valid_survey()
  expect_clean(check_contact_data(survey_dir(tables, "DOE_2012_BELGIUM_PLATO"),
                                  quiet = TRUE))
  for (bad in c("doe_2012_belgium", "DOE_12_BELGIUM", "DOE_BELGIUM")) {
    report <- check_contact_data(survey_dir(tables, bad), quiet = TRUE)
    expect_hit(report, "R02")
  }
  # a documented legacy prefix (year first) is a warning, not an error
  legacy <- check_contact_data(survey_dir(tables, "2010_Willem_BELGIUM"),
                               quiet = TRUE)
  expect_msg(legacy, "R02", "older prefix")
  expect_false("R02" %in% hits(legacy, "error"))
})

test_that("R04: CSV format problems", {
  tables <- valid_survey()

  dir <- survey_dir(tables)
  file <- file.path(dir, "DOE_2012_BELGIUM_hh_common.csv")
  lines <- readLines(file)
  writeLines(gsub(",", ";", lines, fixed = TRUE), file)
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R04")
  expect_msg(report, "R04", "delimiter")

  dir <- survey_dir(tables)
  file <- file.path(dir, "DOE_2012_BELGIUM_sday.csv")
  utils::write.csv(tables$sday, file, row.names = TRUE)
  report <- check_contact_data(dir, quiet = TRUE)
  expect_msg(report, "R04", "row names")

  dir <- survey_dir(tables)
  file <- file.path(dir, "DOE_2012_BELGIUM_participant_common.csv")
  lines <- readLines(file)
  lines[3] <- sub("\"F\"|\"M\"", "\"\"", lines[3])
  lines[4] <- sub("\"F\"|\"M\"", "\" \"", lines[4])
  writeLines(lines, file)
  report <- check_contact_data(dir, quiet = TRUE)
  blank <- only_rule(report, "R04")
  blank <- blank[!is.na(blank$column), ]
  expect_equal(blank$column[1], "part_gender")
  expect_equal(blank$n_fail[1], 2)

  dir <- survey_dir(tables)
  file <- file.path(dir, "DOE_2012_BELGIUM_hh_common.csv")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), readBin(file, "raw", 1e5)), file)
  expect_msg(check_contact_data(dir, quiet = TRUE), "R04", "byte-order")

  # Latin-1 encoded text is not valid UTF-8 (convert the UTF-8 bytes c3 a9)
  tables_latin <- tables
  tables_latin$participant_extra$note <- "caf\u00e9"
  dir <- survey_dir(tables_latin)
  file <- file.path(dir, "DOE_2012_BELGIUM_participant_extra.csv")
  bytes <- readBin(file, "raw", 1e5)
  at <- which(bytes == as.raw(0xc3) & c(bytes[-1], as.raw(0)) == as.raw(0xa9))
  expect_length(at, 30)
  keep <- !(seq_along(bytes) %in% at)
  bytes[at + 1] <- as.raw(0xe9)
  writeBin(bytes[keep], file)
  report <- check_contact_data(dir, quiet = TRUE)
  expect_msg(report, "R04", "UTF-8")

  dir <- survey_dir(tables)
  file <- file.path(dir, "DOE_2012_BELGIUM_hh_common.csv")
  lines <- readLines(file)
  lines[1] <- "\"hh_id\",\"country\",\"country\""
  writeLines(lines, file)
  expect_msg(check_contact_data(dir, quiet = TRUE), "R04", "Duplicated column")
})

test_that("R05: common files have exactly the specified columns in order", {
  tables <- valid_survey()

  renamed <- tables
  names(renamed$contact)[names(renamed$contact) == "cnt_id"] <- "contact_id"
  report <- check_survey(renamed)
  expect_hit(report, "R05")
  expect_msg(report, "R05", "contact_id")

  dropped <- tables
  dropped$contact$phys_contact <- NULL
  expect_hit(check_survey(dropped), "R05")

  extra <- tables
  extra$hh$region <- "VLG"
  report <- check_survey(extra)
  expect_hit(report, "R05")
  expect_msg(report, "R05", "extra file")

  reordered <- tables
  reordered$hh <- reordered$hh[, c("country", "hh_id", "hh_size")]
  # order is not enforced by default (socialmixr joins by name) ...
  expect_clean(check_survey(reordered))
  # ... but can be made strict in the schema (Q14)
  schema <- read_schema()
  schema$options$column_order <- "strict"
  expect_msg(check_survey(reordered, schema = schema), "R05", "order")

  # the age-interval columns, cnt_age_multi and duration_exact are required
  # (A4); only the columns marked optional (holiday) may be absent
  required <- tables
  required$contact$duration_exact <- NULL
  required$participant$part_age_est_min <- NULL
  required <- redocument(required)
  expect_msg(check_survey(required), "R05", "required")

  optional <- tables
  optional$sday$holiday <- NULL
  optional <- redocument(optional)
  expect_clean(check_survey(optional))
})

test_that("R06: extra files match the common files one-to-one", {
  tables <- valid_survey()

  orphan <- tables
  orphan$participant_extra$part_id[1] <- "P999"
  report <- check_survey(orphan)
  expect_hit(report, "R06")

  short <- tables
  short$contact_extra <- short$contact_extra[-1, ]
  expect_hit(check_survey(short), "R06")

  duplicated <- tables
  duplicated$participant_extra$part_gender <- "F"
  report <- check_survey(duplicated)
  expect_hit(report, "R06")
  expect_msg(report, "R06", "duplicated")

  nokey <- tables
  nokey$hh_extra <- data.frame(household = "H001", size_note = "x")
  expect_hit(check_survey(nokey), "R06")

  shared <- tables
  shared$participant_extra$cnt_id <- "x"
  expect_hit(check_survey(shared), "R06", "warning")
})

test_that("a folder without recognisable file names gives one clear finding", {
  dir <- withr::local_tempdir()
  writeLines(c("local_id,age", "1,30"), file.path(dir, "individualsurvey_participants.txt"))
  writeLines(c("local_id,sex", "1,F"), file.path(dir, "individualsurvey_contacts.txt"))
  report <- check_contact_data(dir, quiet = TRUE)
  r01 <- only_rule(report, "R01")
  expect_equal(nrow(r01), 1)
  expect_match(r01$message, "NAMETAG")
  expect_match(r01$message, "individualsurvey_participants.txt")
  expect_match(r01$message, "not `.csv`")
  expect_false(any(grepl("NA_", report$findings$message)))
  expect_false("R24" %in% hits(report))
  expect_match(printed(report), "could not be determined")
  # with a NAMETAG the missing files are named
  named <- check_contact_data(dir, nametag = "DOE_2012_BELGIUM", quiet = TRUE)
  expect_msg(named, "R01", "DOE_2012_BELGIUM_hh_common.csv")
})

test_that("R01 mentions a dictionary file in an unsupported format", {
  dir <- survey_dir()
  file.remove(file.path(dir, "DOE_2012_BELGIUM_dictionary.csv"))
  writeLines("x", file.path(dir, "DOE_2012_BELGIUM_dictionary (1).xls"))
  expect_msg(check_contact_data(dir, quiet = TRUE), "R01", "ignored")
})

test_that("R06 reports shared columns of an extra file once", {
  tables <- valid_survey()
  tables$contact_extra$part_id <- tables$contact$part_id
  report <- check_survey(tables)
  shared <- only_rule(report, "R06")
  shared <- shared[shared$severity == "warning", ]
  expect_equal(nrow(shared), 1)
  expect_match(shared$message, "participant_common")
  expect_match(shared$message, "sday")
})

test_that("R01: required files, file names and unrecognised files", {
  tables <- valid_survey()

  dir <- survey_dir(tables)
  file.remove(file.path(dir, "DOE_2012_BELGIUM_hh_common.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R01")
  expect_msg(report, "R01", "hh_common")

  dir <- survey_dir(tables)
  file.remove(file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx"))
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
  expect_msg(report, "R01", "not part of the guideline file set")
  expect_true(report$passed)
  # other files with the NAMETAG are allowed, e.g. time use or locations
  dir <- survey_dir(tables)
  writeLines(c("a,b", "1,2"), file.path(dir, "DOE_2012_BELGIUM_participants_location.csv"))
  expect_true(check_contact_data(dir, quiet = TRUE)$passed)

  # a CSV with another prefix seems to belong to another record: error
  dir <- survey_dir(tables)
  writeLines(c("a,b", "1,2"), file.path(dir, "OTHER_2012_BELGIUM_timeuse.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R01")
  expect_msg(report, "R01", "another record")

  # core file with a different prefix (also a different year)
  dir <- survey_dir(tables)
  file.rename(file.path(dir, "DOE_2012_BELGIUM_sday.csv"),
              file.path(dir, "DOE_2013_BELGIUM_sday.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R01")
  expect_msg(report, "R01", "more than one NAMETAG")

  expect_hit(check_survey(modifyList(tables, list(hh = NULL))), "R01")
})

test_that("R02: NAMETAG form is a recommendation", {
  tables <- valid_survey()
  expect_clean(check_contact_data(survey_dir(tables, "DOE_2012_BELGIUM_PLATO"),
                                  quiet = TRUE))
  # NAME_YEAR without suffix is the recommended form too
  expect_clean(check_contact_data(survey_dir(tables, "DOE_2012"), quiet = TRUE))
  # deviating names are notes, not errors
  for (odd in c("doe_2012_belgium", "DOE_12_BELGIUM", "DOE_BELGIUM", "CoMix_BE")) {
    report <- check_contact_data(survey_dir(tables, odd), quiet = TRUE)
    expect_msg(report, "R02", "recommended form")
    expect_hit(report, "R02", "note")
    expect_false("R02" %in% hits(report, "error"))
    expect_true(report$passed)
  }
  # a year-first prefix gets its own note
  legacy <- check_contact_data(survey_dir(tables, "2010_Willem_BELGIUM"),
                               quiet = TRUE)
  expect_msg(legacy, "R02", "starts with the year")
  expect_hit(legacy, "R02", "note")
  expect_false("R02" %in% hits(legacy, "error"))
  # only a name that cannot serve as a file name prefix is an error
  bad <- check_contact_data(survey_dir(tables), nametag = "DOE 2012+X",
                            quiet = TRUE)
  expect_hit(bad, "R02")
  expect_msg(bad, "R02", "file name prefix")
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
  expect_msg(report, "R05", "hh_extra")

  reordered <- tables
  reordered$hh <- reordered$hh[, c("country", "hh_id", "hh_size")]
  # order is not enforced by default (socialmixr joins by name) ...
  expect_clean(check_survey(reordered))
  # ... but can be made strict in the schema (Q14)
  schema <- read_schema()
  schema$options$column_order <- "strict"
  expect_msg(check_survey(reordered, schema = schema), "R05", "order")

  # the age-interval columns and duration_exact are required
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
  expect_match(printed(report), "could not be determined")
  # with a NAMETAG the missing files are named
  named <- check_contact_data(dir, nametag = "DOE_2012_BELGIUM", quiet = TRUE)
  expect_msg(named, "R01", "DOE_2012_BELGIUM_hh_common.csv")
})

test_that("R01 mentions a dictionary file in an unsupported format", {
  dir <- survey_dir()
  file.remove(file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx"))
  writeLines("x", file.path(dir, "DOE_2012_BELGIUM_dictionary (1).ods"))
  expect_msg(check_contact_data(dir, quiet = TRUE), "R01", "ignored")
})

test_that("R01: a misnamed dictionary is reported and still checked", {
  for (name in c("DOE_2012_BELGIUM_data_dictionary.xlsx",
                 "DOE_2012_BELGIUM_DICTIONARY.xlsx")) {
    dir <- survey_dir()
    file.rename(file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx"),
                file.path(dir, name))
    report <- check_contact_data(dir, quiet = TRUE)
    expect_msg(report, "R01", "does not follow the naming")
    expect_false(any(grepl("more than one NAMETAG", only_rule(report, "R01")$message)))
    # content was read: a dictionary without a sheet for a file is reported
    expect_false(any(grepl("is missing", only_rule(report, "R01")$message)))
  }
})

test_that("R05 hints at a near-miss column name", {
  tables <- valid_survey()
  names(tables$contact)[names(tables$contact) == "cnt_gender"] <- "Cnt_Gender"
  names(tables$participant)[names(tables$participant) == "part_gender"] <- "part_gendr"
  report <- check_survey(tables)
  expect_msg(report, "R05", "found `Cnt_Gender`")
  expect_msg(report, "R05", "found `part_gendr`")
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

test_that("R01: only one participant and one contact common file", {
  tables <- valid_survey()
  for (extra in c("DOE_2012_BELGIUM_participant_common_v2.csv",
                  "DOE_2012_BELGIUM_participants_common.csv",
                  "OTHER_2013_BELGIUM_participant_common.csv")) {
    dir <- survey_dir(tables)
    file.copy(file.path(dir, "DOE_2012_BELGIUM_participant_common.csv"),
              file.path(dir, extra))
    report <- check_contact_data(dir, nametag = "DOE_2012_BELGIUM",
                                 quiet = TRUE)
    expect_hit(report, "R01")
    expect_msg(report, "R01", "More than one participant common file")
  }
  dir <- survey_dir(tables)
  file.copy(file.path(dir, "DOE_2012_BELGIUM_contact_common.csv"),
            file.path(dir, "DOE_2012_BELGIUM_contact_common_2.csv"))
  expect_msg(check_contact_data(dir, quiet = TRUE), "R01",
             "More than one contact common file")
  # hh_common is not stacked by socialmixr
  dir <- survey_dir(tables)
  file.copy(file.path(dir, "DOE_2012_BELGIUM_hh_common.csv"),
            file.path(dir, "DOE_2012_BELGIUM_hh_common_2.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_false(any(grepl("More than one", only_rule(report, "R01")$message)))
})

test_that("R05 says where an unexpected column belongs", {
  tables <- valid_survey()
  tables$contact$sday_id <- "20120101"
  tables$contact$cnt_others <- tables$contact$cnt_otherplace
  tables$contact$cnt_otherplace <- NULL
  tables$contact$cnt_age_multi <- NA_character_
  tables$contact$survey_note <- "x"
  report <- check_survey(tables)
  r05 <- only_rule(report, "R05")
  msg <- function(pattern) any(grepl(pattern, r05$message))
  expect_true(msg("`sday_id` belong\\(s\\) to the `sday` file"))
  expect_false(msg("sday_id.*extra file"))
  expect_true(msg("`cnt_others` is not a specified column; it looks like `cnt_otherplace`"))
  expect_true(msg("`cnt_age_multi` is no longer part of the format"))
  expect_true(msg("`survey_note`.*`contact_extra` file"))
})

test_that("the dictionary reader finds a header row below a title block", {
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("readxl")
  tables <- valid_survey()
  dir <- survey_dir(tables)
  file <- file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "INDEX")
  openxlsx::writeData(wb, "INDEX", data.frame(a = "One sheet per data file"))
  dict <- tables$dictionary
  for (id in unique(dict$file)) {
    sheet <- dict[dict$file == id, c("variable", "description", "type", "values")]
    names(sheet) <- c("Variable", "Description", "Type", "Allowed values")
    openxlsx::addWorksheet(wb, id)
    openxlsx::writeData(wb, id, paste0("DOE_2012_BELGIUM_", id, ".csv"),
                        startRow = 1, colNames = FALSE)
    openxlsx::writeData(wb, id, "PK: part_id", startRow = 2, colNames = FALSE)
    openxlsx::writeData(wb, id, sheet, startRow = 4)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  report <- check_contact_data(dir, quiet = TRUE)
  expect_false(any(grepl("no sheet/section", only_rule(report, "R15")$message)))
  expect_clean(report)
})

test_that("R15 notes a dictionary layout that differs from the guideline", {
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("readxl")
  tables <- valid_survey()
  write_dict <- function(dir, header, start_row = 1, type_word = NULL) {
    dict <- tables$dictionary
    if (!is.null(type_word)) dict$type[dict$type == "integer"] <- type_word
    wb <- openxlsx::createWorkbook()
    for (id in unique(dict$file)) {
      sheet <- dict[dict$file == id, c("variable", "description", "type", "values")]
      names(sheet) <- header
      openxlsx::addWorksheet(wb, id)
      if (start_row > 1) {
        openxlsx::writeData(wb, id, "Title", startRow = 1, colNames = FALSE)
      }
      openxlsx::writeData(wb, id, sheet, startRow = start_row)
    }
    openxlsx::saveWorkbook(wb, file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx"),
                           overwrite = TRUE)
  }
  notes <- function(dir) {
    r15 <- only_rule(check_contact_data(dir, quiet = TRUE), "R15")
    r15[r15$severity == "note", ]
  }
  dir <- survey_dir(tables)
  write_dict(dir, c("variable", "description", "type", "values"))
  expect_equal(nrow(notes(dir)), 0)

  # letter case alone is not a deviation
  write_dict(dir, c("Variable", "Description", "Type", "Values"))
  expect_equal(nrow(notes(dir)), 0)

  write_dict(dir, c("Variable", "Description", "Type", "Allowed values / coding"))
  expect_true(any(grepl("columns are called", notes(dir)$message)))

  write_dict(dir, c("variable", "description", "type", "values"), start_row = 3)
  expect_true(any(grepl("header is in row 3", notes(dir)$message)))

  write_dict(dir, c("variable", "description", "type", "values"),
             type_word = "float")
  expect_true(any(grepl("`float`", notes(dir)$message)))
})

test_that("R15 names the column a misspelled dictionary entry looks like", {
  tables <- valid_survey()
  tables$dictionary$variable[tables$dictionary$variable == "cnt_age_exact"] <-
    "cnt_ age_exact"
  report <- check_survey(tables)
  expect_msg(report, "R15", "looks like `cnt_age_exact`")
})

test_that("a valid synthetic dataset passes (in memory)", {
  report <- check_survey(valid_survey())
  expect_s3_class(report, "socrates_report")
  expect_equal(report$status, "PASS")
  expect_true(report$passed)
  expect_equal(sum(report$counts[c("error", "warning")]), 0)
  # zero-contact participants are noted, manual items are listed
  expect_true("R18" %in% hits(report, "note"))
  expect_true("R25" %in% hits(report, "manual"))
})

test_that("a valid synthetic dataset passes (folder, CSV dictionary)", {
  dir <- survey_dir()
  report <- check_contact_data(dir, quiet = TRUE)
  expect_equal(report$status, "PASS")
  expect_equal(report$meta$nametag, "DOE_2012_BELGIUM")
  expect_match(report$meta$schema_hash, "^[0-9a-f]{32}$")
  expect_match(report$meta$folder_checksum, "^[0-9a-f]{32}$")
})

test_that("optional files may be absent", {
  tables <- valid_survey()
  tables$participant_extra <- NULL
  tables$contact_extra <- NULL
  expect_clean(check_survey(tables))
})

test_that("an xlsx dictionary with one sheet per file is read", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  dir <- survey_dir(valid_survey())
  expect_true(file.exists(file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx")))
  expect_clean(check_contact_data(dir, quiet = TRUE))
})

test_that("an .xls dictionary is found and read like .xlsx", {
  skip_if_not_installed("readxl")
  dir <- survey_dir()
  file.remove(file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx"))
  # a real .xls file that ships with readxl (its content is not a dictionary)
  file.copy(readxl::readxl_example("datasets.xls"),
            file.path(dir, "DOE_2012_BELGIUM_dictionary.xls"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_false(any(grepl("dictionary file is missing", report$findings$message)))
  # the spreadsheet was parsed: it is judged on its content (R15), not skipped
  expect_hit(report, "R15")
  expect_msg(report, "R15", "no column for|no sheet")
})

test_that("an unreadable dictionary spreadsheet gives a note, not a crash", {
  skip_if_not_installed("readxl")
  dir <- survey_dir()
  file.remove(file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx"))
  writeLines("not a spreadsheet", file.path(dir, "DOE_2012_BELGIUM_dictionary.xls"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R15", "note")
  expect_msg(report, "R15", "could not be read")
})

test_that("check_contact_data can stop on errors", {
  tables <- valid_survey()
  tables$contact$cnt_id[2] <- tables$contact$cnt_id[1]
  dir <- survey_dir(tables)
  expect_error(check_contact_data(dir, quiet = TRUE, stop_on_error = TRUE),
               "error")
  expect_no_error(check_contact_data(dir, quiet = TRUE))
})

test_that("use_template writes a skeleton with the required headers", {
  dir <- withr::local_tempdir()
  files <- use_template(dir, "DOE_2012_BELGIUM")
  expect_true(all(file.exists(files)))
  header <- readLines(file.path(dir, "DOE_2012_BELGIUM_contact_common.csv"))
  expect_match(header, "^\"part_id\",\"cnt_id\"")
  report <- check_contact_data(dir, quiet = TRUE)
  # an empty skeleton is not a valid dataset: no data rows
  expect_hit(report, "R04")
})

test_that("a valid synthetic dataset passes (in memory)", {
  report <- check_survey(valid_survey())
  expect_s3_class(report, "socrates_report")
  expect_equal(report$status, "PASS")
  expect_true(report$passed)
  expect_equal(sum(report$counts[c("error", "warning")]), 0)
  # zero-contact participants are noted, manual items are listed
  expect_true("R18" %in% hits(report, "note"))
  expect_true(all(c("R03", "R25") %in% hits(report, "manual")))
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
  tables <- valid_survey()
  dir <- survey_dir(tables)
  file.remove(file.path(dir, "DOE_2012_BELGIUM_dictionary.csv"))
  wb <- openxlsx::createWorkbook()
  dict <- tables$dictionary
  for (id in unique(dict$file)) {
    sheet <- dict[dict$file == id, c("variable", "description", "type", "values")]
    names(sheet) <- c("Variable", "Description", "Data type", "Allowed values")
    openxlsx::addWorksheet(wb, id)
    openxlsx::writeData(wb, id, sheet)
  }
  openxlsx::saveWorkbook(wb, file.path(dir, "DOE_2012_BELGIUM_dictionary.xlsx"))
  expect_clean(check_contact_data(dir, quiet = TRUE))
})

test_that("check_contact_data can stop on errors", {
  tables <- valid_survey()
  tables$contact$cont_id[2] <- tables$contact$cont_id[1]
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
  expect_match(header, "^\"part_id\",\"cont_id\"")
  report <- check_contact_data(dir, quiet = TRUE)
  # an empty skeleton is not a valid dataset: no data rows
  expect_hit(report, "R04")
})

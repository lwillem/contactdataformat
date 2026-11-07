test_that("R22: smoke test with socialmixr loads a valid dataset", {
  skip_if_not_installed("socialmixr", "0.7.0")
  dir <- survey_dir()
  report <- check_contact_data(dir, smoke_test = TRUE, quiet = TRUE)
  expect_false("R22" %in% hits(report))
  expect_equal(report$status, "PASS")
})

test_that("R22: smoke test reports a dataset socialmixr cannot load", {
  skip_if_not_installed("socialmixr", "0.7.0")
  dir <- survey_dir()
  file.remove(file.path(dir, "DOE_2012_BELGIUM_participant_common.csv"))
  report <- check_contact_data(dir, smoke_test = TRUE, quiet = TRUE)
  expect_hit(report, "R22")
  expect_msg(report, "R22", "socialmixr")
})

test_that("R22 is skipped for in-memory tables", {
  report <- check_survey(valid_survey())
  expect_false("R22" %in% hits(report))
  tables <- valid_survey()
  report <- check_tables(tables$participant, tables$contact, tables$hh,
                         tables$sday, quiet = TRUE)
  expect_s3_class(report, "socrates_report")
})

test_that("the real Flanders 2010 export can be checked locally", {
  dir <- Sys.getenv("CONTACTDATAFORMAT_FLANDERS_DIR", unset = "")
  skip_if(dir == "", "CONTACTDATAFORMAT_FLANDERS_DIR is not set")
  skip_if_not(dir.exists(dir), "CONTACTDATAFORMAT_FLANDERS_DIR does not exist")
  # Only runs on the user's machine; the real data are never part of the
  # repository. The export is expected to need work until the layout is
  # converted, so we only assert that the checker runs and reports.
  report <- check_contact_data(dir, quiet = TRUE)
  expect_s3_class(report, "socrates_report")
  expect_true(nrow(as.data.frame(report)) > 0)
})

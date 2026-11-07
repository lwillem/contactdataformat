test_that("waive keeps findings in the report but they no longer fail it", {
  tables <- valid_survey()
  tables$sday$dayofweek[1] <- (tables$sday$dayofweek[1] + 1L) %% 7L
  tables$contact$cnt_id[2] <- tables$contact$cnt_id[1]
  report <- check_survey(tables)
  expect_equal(report$status, "FAIL")

  waived <- check_survey(tables, waive = c(
    R12 = "diary day is the assigned day, see dictionary",
    R16 = "duplicated on purpose in this test",
    R06 = "extra file follows the duplicated key"
  ))
  expect_equal(waived$status, "PASS")
  expect_true(waived$passed)
  f <- as.data.frame(waived)
  expect_setequal(f$rule_id[f$severity == "waived"], c("R12", "R16", "R06"))
  expect_match(f$waiver_reason[f$rule_id == "R12" & f$severity == "waived"],
               "assigned day")
  expect_equal(waived$counts[["waived"]], 3)
})

test_that("waive validates its input", {
  tables <- valid_survey()
  expect_error(check_survey(tables, waive = c(R12 = "")), "reason")
  expect_error(check_survey(tables, waive = c("no names")), "named")
  expect_error(check_survey(tables, waive = c(R99 = "x")), "Unknown rule")
  expect_error(check_survey(tables, skip = "R99"), "Unknown rule")
})

test_that("rules and skip select which rules run", {
  tables <- valid_survey()
  tables$contact$cnt_id[2] <- tables$contact$cnt_id[1]
  tables$hh$country[1] <- "XX"
  only <- check_survey(tables, rules = "R16")
  expect_equal(hits(only, "error"), "R16")
  skipped <- check_survey(tables, skip = "R16")
  expect_false("R16" %in% hits(skipped, "error"))
  expect_true("R16" %in% skipped$meta$rules_skipped)
  expect_true(all(sprintf("R%d", 28:33) %in% skipped$meta$rules_planned))
})

test_that("a crashing rule becomes an error finding instead of aborting", {
  local_mocked_bindings(rule_r16 = function(ctx) stop("boom"))
  report <- check_survey(valid_survey())
  expect_hit(report, "R16")
  expect_msg(report, "R16", "boom")
})

test_that("print, summary and as.data.frame work", {
  tables <- valid_survey()
  tables$contact$cnt_id[2] <- tables$contact$cnt_id[1]
  report <- check_survey(tables, waive = c(R06 = "reviewed"))
  out <- printed(report)
  expect_match(out, "FAIL")
  expect_match(out, "R16")
  expect_match(out, "Manual checklist")
  expect_match(out, "Waived")

  s <- summary(report)
  expect_s3_class(s, "summary.socrates_report")
  expect_equal(s$status, "FAIL")
  expect_output(print(s), "Findings per rule")

  df <- as.data.frame(report)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("rule_id", "severity", "file", "column", "n_fail",
                    "n_total", "example_rows", "message", "guideline_ref") %in%
                    names(df)))

  clean <- check_survey(valid_survey())
  expect_no_error(printed(clean))
  expect_match(printed(clean), "PASS")
  expect_no_error(capture.output(print(summary(clean))))
})

test_that("example rows are file line numbers", {
  tables <- valid_survey()
  tables$contact$cnt_gender[4] <- "N/A"
  report <- check_survey(tables)
  # data row 4 is line 5 of the CSV (header = line 1)
  expect_equal(only_rule(report, "R07")$example_rows, "5")
})

test_that("report_write writes Markdown, CSV and JSON", {
  tables <- valid_survey()
  tables$contact$cnt_id[2] <- tables$contact$cnt_id[1]
  report <- check_survey(tables)
  dir <- withr::local_tempdir()

  md <- report_write(report, file.path(dir, "r.md"))
  text <- readLines(md)
  expect_match(text[1], "FAIL")
  expect_true(any(grepl("R16", text)))
  expect_true(any(grepl("Manual checklist", text)))

  csv <- report_write(report, file.path(dir, "r.csv"))
  back <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_equal(nrow(back), nrow(as.data.frame(report)))

  json <- report_write(report, file.path(dir, "r.json"))
  parsed <- jsonlite::read_json(json, simplifyVector = TRUE)
  expect_equal(parsed$status, "FAIL")
  expect_false(parsed$passed)
  expect_equal(parsed$meta$guideline_version, "1.0")
  expect_true("R16" %in% parsed$findings$rule_id)

  expect_error(report_write(report, file.path(dir, "r.txt")), "should be one of")
  expect_error(report_write(list(), file.path(dir, "x.md")), "socrates_report")
})

test_that("read_socrates exposes the data as the checker sees them", {
  dir <- survey_dir()
  x <- read_socrates(dir)
  expect_s3_class(x, "socrates_data")
  expect_true(all(vapply(x$tables$contact_common, is.character, logical(1))))
  expect_equal(x$nametag, "DOE_2012_BELGIUM")
  expect_error(read_socrates(file.path(dir, "nope")), "existing folder")
})

test_that("braces in messages are printed literally", {
  tables <- valid_survey()
  report <- check_survey(tables, nametag = "bad_name")
  expect_match(printed(report), "[0-9]{4}", fixed = TRUE)
})

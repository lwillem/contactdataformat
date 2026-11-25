# Zenodo metadata (R36): never uses the network in tests

good_reference <- function() {
  list(title = list("Social contact survey in Belgium, 2010-2011"),
       author = list("Doe, Jane", "Smith, John"), year = list(2020),
       note = list("Version v1.0"), doi = list("10.5281/zenodo.4302055"),
       reference = list("Doe J (2012) A survey. Journal 1: 1-2"))
}

good_record <- function() {
  list(
    doi = "10.5281/zenodo.4302055",
    metadata = list(
      title = "Social contact survey in Belgium, 2010-2011",
      creators = list(list(name = "Doe, Jane", affiliation = "Univ", orcid = "0000-0000-0000-0000"),
                      list(name = "Smith, John", affiliation = "Univ")),
      publication_date = "2020-05-04", version = "v1.0",
      description = "Survey design, sample, period and scope.",
      resource_type = list(type = "dataset"),
      communities = list(list(id = "social_contact_data")),
      license = list(id = "cc-by-4.0"), access_right = "open",
      references = list("Doe J (2012) A survey. Journal 1: 1-2"),
      keywords = list("social contacts")
    ),
    files = list(list(key = "DOE_2012_BELGIUM_participant_common.csv"),
                 list(key = "DOE_2012_BELGIUM_contact_common.csv"))
  )
}

write_reference <- function(dir, content, name = "DOE_2012_BELGIUM_reference.json") {
  writeLines(jsonlite::toJSON(content, auto_unbox = FALSE), file.path(dir, name))
}

test_that("a local folder without reference file gets no R36 finding", {
  dir <- survey_dir()
  report <- check_contact_data(dir, quiet = TRUE)
  expect_false("R36" %in% as.data.frame(report)$rule_id)
  expect_clean(report)
})

test_that("R36 checks the reference file of a downloaded folder", {
  dir <- survey_dir()
  write_reference(dir, good_reference())
  report <- check_contact_data(dir, quiet = TRUE)
  expect_false(any(c("error", "warning") %in% only_rule(report, "R36")$severity))

  bad <- good_reference()
  bad$title <- list("DOE_2012_BELGIUM v1.0")
  bad$author <- list("Jane Doe", "John Smith")
  bad$note <- list("Version 1.2")
  bad$doi <- list()
  write_reference(dir, bad)
  report <- check_contact_data(dir, quiet = TRUE)
  r36 <- only_rule(report, "R36")
  expect_true(all(c("warning") %in% r36$severity))
  expect_true(any(grepl("version number or a NAMETAG", r36$message)))
  expect_true(any(grepl("Family name, Given names", r36$message)))
  expect_true(any(grepl("does not follow the scheme", r36$message)))
  expect_true(any(grepl("DOI is missing", r36$message)))
  # warnings do not block the record
  expect_true(report$passed)

  missing <- list(note = list("Version v1.0"))
  write_reference(dir, missing)
  expect_hit(check_contact_data(dir, quiet = TRUE), "R36")
})

test_that("check_zenodo_record checks a record and a reference file", {
  report <- check_zenodo_record(good_record(), quiet = TRUE)
  expect_s3_class(report, "socrates_report")
  expect_equal(report$status, "PASS")
  expect_equal(report$meta$rules_run, "R36")

  bad <- good_record()
  bad$metadata$resource_type$type <- "software"
  bad$metadata$communities <- list()
  bad$metadata$license <- list(id = "cc-by-nc-4.0")
  bad$metadata$access_right <- "restricted"
  bad$metadata$creators <- list(list(name = "Some Institute"))
  bad$metadata$version <- "1.0"
  bad$files <- list()
  report <- check_zenodo_record(bad, quiet = TRUE)
  messages <- as.data.frame(report)$message
  expect_equal(report$status, "MANDATORY UPDATES NEEDED")
  for (pattern in c("must be Dataset", "Social Contact Data", "CC BY",
                    "must be open", "not in the form", "does not follow the scheme",
                    "no files")) {
    expect_true(any(grepl(pattern, messages)), info = pattern)
  }

  # the NAME of the NAMETAG is the family name of the first creator
  other <- good_record()
  other$metadata$creators[[1]]$name <- "Jones, Pat"
  expect_msg(check_zenodo_record(other, quiet = TRUE), "R36", "family name of the first creator")

  file <- withr::local_tempfile(fileext = ".json")
  write_reference(dirname(file), good_reference(), basename(file))
  from_file <- check_zenodo_record(file, quiet = TRUE)
  expect_true(from_file$passed)
  expect_equal(from_file$counts[["warning"]], 0L)
})

test_that("record ids are read from DOIs and URLs, nothing else", {
  id <- contactdataformat:::zenodo_record_id
  expect_equal(id("10.5281/zenodo.4302055"), "4302055")
  expect_equal(id("https://doi.org/10.5281/zenodo.4302055"), "4302055")
  expect_equal(id("https://zenodo.org/records/4302055"), "4302055")
  expect_equal(id("4302055"), "4302055")
  expect_true(is.na(id("not a record")))
  expect_error(check_zenodo_record("not a record"), "not a file")
})

test_that("R21: contact count plausibility", {
  tables <- valid_survey()
  many <- tables
  extra <- data.frame(
    part_id = "P001",
    cnt_id = sprintf("X%03d", 1:120),
    stringsAsFactors = FALSE
  )
  template <- many$contact[rep(1, 120), ]
  template$part_id <- "P001"
  template$cnt_id <- extra$cnt_id
  many$contact <- rbind(many$contact, template)
  many$contact_extra <- rbind(many$contact_extra,
                              data.frame(cnt_id = extra$cnt_id,
                                         cnt_imputed = FALSE))
  report <- check_survey(many)
  expect_hit(report, "R21", "warning")
  expect_msg(report, "R21", "more than 100")

  zero <- tables
  zero$contact <- zero$contact[zero$contact$part_id %in% zero$participant$part_id[1:10], ]
  zero$contact_extra <- zero$contact_extra[zero$contact_extra$cnt_id %in%
                                             zero$contact$cnt_id, ]
  expect_msg(check_survey(zero), "R21", "no contact")

  imputed <- tables
  imputed$contact_extra$cnt_imputed[1:5] <- TRUE
  report <- check_survey(imputed)
  expect_hit(report, "R21", "note")
  expect_true(report$passed)
})

test_that("R23: imputation is flagged", {
  tables <- valid_survey()

  bad_flag <- tables
  bad_flag$contact_extra$cnt_imputed <- ifelse(bad_flag$contact_extra$cnt_imputed,
                                               "Y", "N")
  expect_hit(check_survey(bad_flag), "R23", "warning")
})

test_that("R03 and R25 list manual items without affecting the status", {
  report <- check_survey(valid_survey())
  manual <- as.data.frame(report)
  manual <- manual[manual$severity == "manual", ]
  expect_gte(nrow(manual), 5)
  expect_equal(report$status, "PASS")
  expect_equal(sum(report$counts[c("error", "warning")]), 0)
})

test_that("R26: personal identifiers in column names", {
  tables <- valid_survey()
  tables$participant_extra$surname <- "x"
  tables$participant_extra$phone_number <- "x"
  tables$participant_extra$school_name <- "x"
  report <- check_survey(tables)
  # direct identifiers are errors, the ambiguous token `name` a warning
  expect_hit(report, "R26", "error")
  expect_hit(report, "R26", "warning")
  r26 <- only_rule(report, "R26")
  expect_setequal(r26$column[r26$severity == "error"], c("surname", "phone_number"))
  expect_equal(r26$column[r26$severity == "warning"], "school_name")
})

test_that("R27: ZIP, address and coordinate columns are flagged", {
  tables <- valid_survey()
  geo <- tables
  geo$participant_extra$city_zip_code <- "9999"
  geo$participant_extra$latitude <- 51.2
  geo$participant_extra$longitude <- 4.4
  geo$contact_extra$street <- "x"
  report <- check_survey(redocument(geo))
  expect_hit(report, "R27")
  expect_setequal(only_rule(report, "R27")$column,
                  c("city_zip_code", "latitude", "longitude", "street"))
  expect_false(report$passed)
})

test_that("R27: municipality, city and NIS columns are always flagged", {
  tables <- valid_survey()
  tables$participant_extra$municipality <- "Faketown"
  tables <- redocument(tables)
  expect_hit(check_survey(tables), "R27")

  tables$participant_extra$zip <- "1000"
  expect_hit(check_survey(redocument(tables)), "R27")
})

test_that("R27: region and province columns are fine", {
  tables <- valid_survey()
  tables$participant_extra$part_region <- "BE-VLG"
  tables$participant_extra$part_province <- "BE-VAN"
  expect_clean(check_survey(redocument(tables)))
})

test_that("R27: value heuristics catch unnamed postal codes and coordinates", {
  tables <- valid_survey(60)
  zip <- tables
  zip$participant_extra$loc <- as.character(1000L + seq_len(60) * 37L)
  report <- check_survey(zip)
  expect_hit(report, "R27", "warning")
  expect_msg(report, "R27", "postal codes")

  coords <- tables
  coords$participant_extra$p1 <- 50.8 + seq_len(60) / 100
  coords$participant_extra$p2 <- 3.1 + seq_len(60) / 50
  report <- check_survey(coords)
  expect_hit(report, "R27", "warning")
  expect_msg(report, "R27", "coordinate")

  # an ordinary 4-digit column with few distinct values (years) is fine
  years <- tables
  years$participant_extra$birth_cohort <- rep(c("1950", "1960", "1970"), 20)
  expect_false("R27" %in% hits(check_survey(years), "warning"))
})

test_that("R27: a restricted geography file must not be in the public folder", {
  tables <- valid_survey()
  dir <- survey_dir(tables)
  # synthetic geography: invented ZIP/NIS pairs and municipalities
  geo <- data.frame(
    part_id = tables$participant$part_id,
    hh_id = tables$participant$hh_id,
    zip_code = rep(c("1000", "1001"), length.out = 30),
    nis_code = rep(c("00001", "00002"), length.out = 30),
    municipality_name = rep(c("Faketown", "Mocktown"), length.out = 30),
    latitude = 50.9, longitude = 4.3, geo_reference_year = 2012,
    geo_review_status = "reviewed_accepted"
  )
  write_csv_utf8(geo, file.path(dir, "DOE_2012_BELGIUM_geo_restricted.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R27")
  expect_msg(report, "R27", "restricted geography file")
  expect_false(report$passed)
  # the file is not reported twice as an unrecognised CSV
  expect_false("R01" %in% hits(report, "warning"))

  # an oddly named file with geography columns that would be merged
  dir <- survey_dir(tables)
  odd <- data.frame(part_id = tables$participant$part_id,
                    postcode = "1000", stringsAsFactors = FALSE)
  write_csv_utf8(odd, file.path(dir, "DOE_2012_BELGIUM_places.csv"))
  report <- check_contact_data(dir, quiet = TRUE)
  expect_hit(report, "R27")
  expect_msg(report, "R27", "would merge")
  expect_hit(report, "R01", "warning")
})

test_that("socialmixr merges a stray geography file (why R27 exists)", {
  skip_if_not_installed("socialmixr", "0.7.0")
  tables <- valid_survey()
  dir <- survey_dir(tables)
  geo <- data.frame(part_id = tables$participant$part_id,
                    zip_code = "1000", stringsAsFactors = FALSE)
  write_csv_utf8(geo, file.path(dir, "DOE_2012_BELGIUM_geo_restricted.csv"))
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  survey <- suppressMessages(suppressWarnings(socialmixr::load_survey(files)))
  expect_true("zip_code" %in% names(survey$participants))
})

test_that("R42: implausible household size", {
  tables <- valid_survey()
  tables$hh$hh_size[1] <- 40L
  report <- check_survey(tables)
  expect_hit(report, "R42", "warning")
  expect_msg(report, "R42", "above 20")
  expect_false("R42" %in% hits(check_survey(valid_survey()), "warning"))
})

test_that("R43: columns that are entirely NA", {
  tables <- valid_survey()
  tables$contact$duration_exact <- NA
  report <- check_survey(tables)
  expect_hit(report, "R43", "note")
  expect_msg(report, "R43", "duration_exact")
  expect_true(report$passed)

  no_location <- valid_survey()
  for (col in c("cnt_home", "cnt_work", "cnt_school", "cnt_transport",
                "cnt_leisure", "cnt_otherplace")) {
    no_location$contact[[col]] <- NA
  }
  report <- check_survey(no_location)
  expect_hit(report, "R43", "warning")
  expect_msg(report, "R43", "location columns are entirely")
  expect_false("R13" %in% hits(report, "error"))
})

test_that("R44: participants without a survey date", {
  tables <- valid_survey()
  expect_false("R44" %in% hits(check_survey(tables), "warning"))
  rows <- 1:6
  for (col in c("sday_id", "day", "month", "year", "dayofweek")) {
    tables$sday[[col]][rows] <- NA
  }
  report <- check_survey(tables)
  expect_hit(report, "R44", "warning")
  expect_msg(report, "R44", "6 of 30")
  expect_msg(report, "R44", "reported contacts")
})

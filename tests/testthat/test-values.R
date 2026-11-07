test_that("R07: missing values only as NA, no placeholder codes", {
  tables <- valid_survey()

  tokens <- tables
  tokens$contact$cnt_gender[1:3] <- c("N/A", "null", "-")
  report <- check_survey(tokens)
  expect_hit(report, "R07")
  expect_equal(only_rule(report, "R07")$n_fail[1], 3)

  placeholder <- tables
  placeholder$hh$hh_size[1] <- 999L
  placeholder$contact$duration_exact[1] <- -99
  report <- check_survey(placeholder)
  expect_hit(report, "R07", "warning")
  expect_false("R07" %in% hits(report, "error"))

  extra <- tables
  extra$participant_extra$bmi <- c(-999, rep(22.5, nrow(tables$participant) - 1))
  expect_hit(check_survey(extra), "R07", "warning")

  extra_token <- tables
  extra_token$participant_extra$smoker <- "NaN"
  expect_hit(check_survey(extra_token), "R07")
})

test_that("R08: column types and mandatory values", {
  tables <- valid_survey()

  boolean <- tables
  boolean$contact$phys_contact <- ifelse(boolean$contact$phys_contact, "Y", "N")
  boolean$contact$cnt_home <- ifelse(boolean$contact$cnt_home, "yes", "no")
  # integer 0/1 is accepted
  boolean$contact$cnt_work <- ifelse(boolean$contact$cnt_work, 1L, 0L)
  report <- check_survey(boolean)
  expect_hit(report, "R08")
  expect_msg(report, "R08", "TRUE or FALSE")
  expect_setequal(only_rule(report, "R08")$column, c("phys_contact", "cnt_home"))

  integer <- tables
  integer$participant$part_age_exact[1] <- 3.5
  integer$hh$hh_size <- as.character(integer$hh$hh_size)
  integer$hh$hh_size[2] <- "three"
  report <- check_survey(integer)
  expect_setequal(only_rule(report, "R08")$column, c("part_age_exact", "hh_size"))

  date <- tables
  date$sday$sday_id[1] <- "2011-01-10"
  expect_hit(check_survey(date), "R08")

  missing_key <- tables
  missing_key$hh$country[2] <- NA
  report <- check_survey(missing_key)
  expect_hit(report, "R08")
  expect_msg(report, "R08", "not allowed")
})

test_that("R09: enumerations and ranges", {
  tables <- valid_survey()

  gender <- tables
  gender$participant$part_gender[1] <- "X"
  gender$contact$cnt_gender[1] <- "Male"
  report <- check_survey(gender)
  expect_setequal(only_rule(report, "R09")$column, c("part_gender", "cnt_gender"))

  country <- tables
  country$hh$country[1] <- "XX"
  expect_hit(check_survey(country), "R09")
  # Namibia is valid even though the string NA looks like a missing token
  ok <- tables
  ok$hh$country[1] <- "NO"
  expect_clean(check_survey(ok))

  range <- tables
  range$sday$dayofweek[1] <- 7L
  range$sday$month[2] <- 13L
  report <- check_survey(range)
  expect_setequal(only_rule(report, "R09")$column, c("dayofweek", "month"))
  hh0 <- tables
  hh0$hh$hh_size[1] <- 0L
  expect_hit(check_survey(hh0), "R09")
})

test_that("R10: age encoding", {
  tables <- valid_survey()

  both <- tables
  both$participant$part_age_est_min[1] <- 20L
  both$participant$part_age_est_max[1] <- 29L
  both$participant$part_age_exact[1] <- 25L
  report <- check_survey(both)
  expect_hit(report, "R10")
  expect_msg(report, "R10", "never both")

  reversed <- tables
  reversed$contact$cnt_age_exact[2] <- NA
  reversed$contact$cnt_age_est_min[2] <- 40L
  reversed$contact$cnt_age_est_max[2] <- 30L
  expect_msg(check_survey(reversed), "R10", "larger than")

  range <- tables
  range$participant$part_age_exact[3] <- 130L
  range$contact$cnt_age_exact[1] <- -1L
  report <- check_survey(range)
  expect_setequal(only_rule(report, "R10")$column, c("part_age_exact", "cnt_age_exact"))

  open <- tables
  open$participant$part_age_exact[1] <- NA
  open$participant$part_age_est_min[1] <- 60L
  open$participant$part_age_est_max[1] <- NA
  # an open band needs an explicit upper bound by default ...
  expect_hit(check_survey(open), "R10")
  # ... which is a schema setting
  schema <- read_schema()
  schema$options$allow_open_age_interval <- TRUE
  expect_clean(check_survey(open, schema = schema))
})

test_that("R11: share of missing ages is a warning", {
  tables <- valid_survey()
  tables$participant$part_age_exact[1:6] <- NA
  tables$participant$part_age_est_min[1:6] <- NA
  tables$participant$part_age_est_max[1:6] <- NA
  report <- check_survey(tables)
  expect_hit(report, "R11", "warning")
  expect_true(report$passed)

  contacts <- valid_survey()
  contacts$contact$cnt_age_exact <- NA
  contacts$contact$cnt_age_est_min <- NA
  contacts$contact$cnt_age_est_max <- NA
  expect_hit(check_survey(contacts), "R11", "warning")
})

test_that("R12: survey day consistency", {
  tables <- valid_survey()

  invalid <- tables
  invalid$sday$sday_id[1] <- "20110231"
  expect_hit(check_survey(invalid), "R12")

  mismatch <- tables
  mismatch$sday$day[2] <- mismatch$sday$day[2] %% 28 + 1L
  mismatch$sday$year[3] <- 2012L
  report <- check_survey(mismatch)
  expect_setequal(only_rule(report, "R12")$column, c("day", "year"))
  expect_true(all(only_rule(report, "R12")$severity == "error"))

  weekday <- tables
  weekday$sday$dayofweek[1] <- (weekday$sday$dayofweek[1] + 1L) %% 7L
  report <- check_survey(weekday)
  expect_hit(report, "R12", "warning")
  expect_true(report$passed)
  # the weekday severity is a schema setting (Q4)
  schema <- read_schema()
  schema$rules[[which(purrr::map_chr(schema$rules, "id") == "R12")]]$severities$weekday <- "error"
  expect_hit(check_survey(weekday, schema = schema), "R12", "error")
})

test_that("R13: location logic", {
  tables <- valid_survey()
  loc <- c("cnt_home", "cnt_work", "cnt_school", "cnt_transport",
           "cnt_leisure", "cnt_otherplace")

  all_false <- tables
  all_false$contact[1, loc] <- FALSE
  report <- check_survey(all_false)
  expect_hit(report, "R13")
  expect_equal(only_rule(report, "R13")$n_fail, 1)

  unknown_partial <- tables
  unknown_partial$contact[2, loc] <- NA
  unknown_partial$contact$cnt_home[2] <- FALSE
  expect_hit(check_survey(unknown_partial), "R13")

  all_na <- tables
  all_na$contact[3, loc] <- NA
  expect_clean(check_survey(all_na))

  multiple <- tables
  multiple$contact[4, c("cnt_home", "cnt_leisure")] <- TRUE
  expect_clean(check_survey(multiple))
})

test_that("R14: category codes are documented", {
  tables <- valid_survey()

  undocumented <- tables
  undocumented$contact$frequency_multi[1] <- 7L
  report <- check_survey(undocumented)
  expect_hit(report, "R14")
  expect_hit(report, "R14", "note")

  nodoc <- tables
  nodoc$dictionary$values[nodoc$dictionary$variable == "duration_multi"] <- NA
  expect_msg(check_survey(nodoc), "R14", "mapping")

  own_scheme <- tables
  own_scheme$contact$duration_multi[1] <- 6L
  own_scheme$dictionary$values[own_scheme$dictionary$variable == "duration_multi"] <-
    "1: a, 2: b, 3: c, 4: d, 5: e, 6: f"
  report <- check_survey(own_scheme)
  expect_false("R14" %in% hits(report, "error"))
  expect_hit(report, "R14", "note")

  aged <- tables
  aged$contact$cnt_age_multi[1] <- "A1"
  expect_hit(check_survey(aged), "R14")
})

test_that("parse_codes understands common dictionary notations", {
  expect_equal(parse_codes("1 = daily; 2 = weekly"), c("1", "2"))
  expect_equal(parse_codes("1: daily, 2: weekly, 3: monthly"),
               c("1", "2", "3"))
  expect_equal(parse_codes("0=no\n1=yes"), c("0", "1"))
  expect_equal(parse_codes("free text"), character())
  expect_equal(parse_codes(NA), character())
})

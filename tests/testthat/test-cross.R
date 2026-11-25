test_that("R15: dictionary completeness", {
  tables <- valid_survey()

  undocumented <- tables
  undocumented$dictionary <- undocumented$dictionary[
    undocumented$dictionary$variable != "hh_size", ]
  report <- check_survey(undocumented)
  expect_hit(report, "R15")
  expect_msg(report, "R15", "hh_size")

  phantom <- tables
  phantom$dictionary <- rbind(phantom$dictionary, data.frame(
    file = "participant_common", variable = "city_nis_code",
    description = "x", type = "string", values = NA))
  report <- check_survey(phantom)
  expect_hit(report, "R15", "warning")
  expect_false("R15" %in% hits(report, "error"))

  wrong_type <- tables
  wrong_type$dictionary$type[wrong_type$dictionary$variable == "cnt_age_exact"] <-
    "string"
  expect_hit(check_survey(wrong_type), "R15", "warning")
  fine_type <- tables
  fine_type$dictionary$type[fine_type$dictionary$variable == "cnt_age_exact"] <-
    "integer (count)"
  expect_clean(check_survey(fine_type))

  no_sheet <- tables
  no_sheet$dictionary <- no_sheet$dictionary[no_sheet$dictionary$file != "sday", ]
  expect_msg(check_survey(no_sheet), "R15", "no sheet")

  no_field <- tables
  no_field$dictionary$values <- NULL
  expect_msg(check_survey(no_field), "R15", "no column for")

  none <- tables
  none$dictionary <- NULL
  expect_hit(check_survey(none), "R15", "note")
})

test_that("R16: keys are unique", {
  tables <- valid_survey()

  part <- tables
  part$participant$part_id[2] <- part$participant$part_id[1]
  report <- check_survey(part)
  expect_hit(report, "R16")
  expect_equal(only_rule(report, "R16")$column[1], "part_id")

  cont <- tables
  cont$contact$cnt_id[5] <- cont$contact$cnt_id[4]
  expect_equal(only_rule(check_survey(cont), "R16")$column[1], "cnt_id")

  hh <- tables
  hh$hh$hh_id[2] <- hh$hh$hh_id[1]
  expect_hit(check_survey(hh), "R16")

  sday <- tables
  sday$sday$part_id[2] <- sday$sday$part_id[1]
  expect_hit(check_survey(sday), "R16")

  extra <- tables
  extra$contact_extra$cnt_id[2] <- extra$contact_extra$cnt_id[1]
  expect_hit(check_survey(extra), "R16")

  # sday_id is a value, not a key: shared dates are fine
  expect_equal(anyDuplicated(tables$sday$sday_id) > 0, TRUE)
  expect_clean(check_survey(tables))
})

test_that("R17: foreign keys", {
  tables <- valid_survey()

  orphan_contact <- tables
  orphan_contact$contact$part_id[1] <- "P999"
  report <- check_survey(orphan_contact)
  expect_hit(report, "R17")
  expect_msg(report, "R17", "P999")

  orphan_hh <- tables
  orphan_hh$participant$hh_id[1] <- "H999"
  expect_hit(check_survey(orphan_hh), "R17")

  orphan_sday <- tables
  orphan_sday$sday$part_id[1] <- "P999"
  expect_hit(check_survey(orphan_sday), "R17")

  # hh_id is never NA (guideline: use hh_id = part_id without household data)
  na_hh <- tables
  na_hh$participant$hh_id[1] <- NA
  expect_hit(check_survey(na_hh), "R08")
})

test_that("R18: survey-day rows and zero-contact participants", {
  tables <- valid_survey()
  no_sday <- tables
  no_sday$sday <- no_sday$sday[-1, ]
  expect_hit(check_survey(no_sday), "R18")

  report <- check_survey(tables)
  expect_hit(report, "R18", "note")
  expect_msg(report, "R18", "kept in the participant file")
})

test_that("R19: household size", {
  tables <- valid_survey()
  tables$hh$hh_size[1] <- 1L
  report <- check_survey(tables)
  expect_hit(report, "R19", "warning")
  expect_true(report$passed)
})

test_that("R20: panel ids", {
  tables <- valid_survey()
  tables$participant_extra$panel_id[2] <- tables$participant_extra$panel_id[1]
  tables$sday$sday_id[2] <- tables$sday$sday_id[1]
  tables$sday$day[2] <- tables$sday$day[1]
  tables$sday$month[2] <- tables$sday$month[1]
  tables$sday$year[2] <- tables$sday$year[1]
  tables$sday$dayofweek[2] <- tables$sday$dayofweek[1]
  expect_hit(check_survey(tables), "R20", "warning")

  other_day <- valid_survey()
  other_day$participant_extra$panel_id[2] <- other_day$participant_extra$panel_id[1]
  expect_false("R20" %in% hits(check_survey(other_day), "warning"))
})

test_that("R41: a part_id has one panel_id, in every file", {
  # forward: the same part_id under two panel_ids
  tables <- valid_survey()
  extra <- tables$participant_extra
  tables$participant_extra <- rbind(extra[1, ], extra[1, ], extra[-1, ])
  tables$participant_extra$panel_id[2] <- "OTHER"
  expect_hit(check_survey(tables), "R41", "error")

  # reverse: panel_id repeated in sday must agree with participant_extra
  copy <- valid_survey()
  copy$sday$panel_id <- copy$participant_extra$panel_id[
    match(copy$sday$part_id, copy$participant_extra$part_id)]
  expect_false("R41" %in% hits(check_survey(copy), "error"))
  copy$sday$panel_id[1] <- "OTHER"
  expect_hit(check_survey(copy), "R41", "error")

  # several part_id per panel_id (several waves) is fine
  waves <- valid_survey()
  waves$participant_extra$panel_id[2] <- waves$participant_extra$panel_id[1]
  expect_false("R41" %in% hits(check_survey(waves), "error"))
})

test_that("R20: summary of the panel ids", {
  tables <- valid_survey()
  tables$participant_extra$panel_id[2] <- tables$participant_extra$panel_id[1]
  report <- check_survey(tables)
  expect_hit(report, "R20", "note")
  expect_msg(report, "R20", "29 individual\\(s\\), 1 with more than one")
  expect_true(report$passed)
})

test_that("R45: one panel_id is one person", {
  link <- function(tables) {
    tables$participant_extra$panel_id[2] <- tables$participant_extra$panel_id[1]
    tables
  }
  # same person, consistent gender and age (a day apart): clean
  ok <- link(valid_survey())
  ok$participant$part_gender[2] <- ok$participant$part_gender[1]
  ok$participant$part_age_exact[2] <- ok$participant$part_age_exact[1]
  expect_false("R45" %in% hits(check_survey(ok), "warning"))

  gender <- ok
  gender$participant$part_gender[2] <- "F"
  gender$participant$part_gender[1] <- "M"
  expect_msg(check_survey(gender), "R45", "part_gender")
  expect_hit(check_survey(gender), "R45", "warning")

  age <- ok
  age$participant$part_age_exact[2] <- age$participant$part_age_exact[1] + 20L
  expect_msg(check_survey(age), "R45", "exact age")
})

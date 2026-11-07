# Fixtures are built in code from the synthetic example_survey(); no real
# survey data are used anywhere in the tests.

valid_survey <- function(n_participants = 30) {
  example_survey(n_participants = n_participants)
}

# In-memory check of a list of tables as returned by example_survey()
check_survey <- function(tables, ...) {
  check_tables(
    participant = tables$participant, contact = tables$contact,
    hh = tables$hh, sday = tables$sday,
    participant_extra = tables$participant_extra,
    contact_extra = tables$contact_extra, hh_extra = tables$hh_extra,
    dictionary = tables$dictionary, metadata = tables$metadata,
    quiet = TRUE, ...
  )
}

# Write a survey to a fresh temporary folder and return the folder
survey_dir <- function(tables = valid_survey(), nametag = "DOE_2012_BELGIUM",
                       env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  write_survey(tables, dir, nametag)
  dir
}

# Rule ids with a given severity in a report
hits <- function(report, severity = NULL) {
  f <- as.data.frame(report)
  if (!is.null(severity)) f <- f[f$severity == severity, ]
  unique(f$rule_id)
}

expect_hit <- function(report, rule, severity = "error") {
  testthat::expect_true(
    rule %in% hits(report, severity),
    info = paste0("expected ", severity, " ", rule, "; got errors: ",
                  paste(hits(report, "error"), collapse = ","),
                  " warnings: ", paste(hits(report, "warning"), collapse = ","))
  )
}

expect_clean <- function(report) {
  testthat::expect_equal(report$status, "PASS",
                         info = paste(hits(report, "error"),
                                      hits(report, "warning")))
}

# Drop findings of the given rule so a test can focus on a single rule
only_rule <- function(report, rule) {
  f <- as.data.frame(report)
  f[f$rule_id == rule, ]
}

# Replace a file in a written folder
rewrite <- function(dir, suffix, lines) {
  file <- list.files(dir, pattern = paste0(suffix, "$"), full.names = TRUE)
  writeLines(lines, file, useBytes = TRUE)
  file
}

# At least one message of the rule matches the pattern
expect_msg <- function(report, rule, pattern) {
  messages <- only_rule(report, rule)$message
  testthat::expect_true(
    any(grepl(pattern, messages)),
    info = paste0("no ", rule, " message matches '", pattern, "'; got: ",
                  paste(messages, collapse = " | "))
  )
}

# Rebuild the dictionary after a test added columns to the tables
redocument <- function(tables) {
  ids <- c(participant = "participant_common", contact = "contact_common",
           hh = "hh_common", sday = "sday",
           participant_extra = "participant_extra",
           contact_extra = "contact_extra", hh_extra = "hh_extra")
  present <- intersect(names(ids), names(tables))
  docs <- stats::setNames(tables[present], ids[present])
  tables$dictionary <- contactdataformat:::example_dictionary(docs)
  tables
}

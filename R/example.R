#' Synthetic example dataset
#'
#' Builds a small, fully synthetic dataset that follows the guideline and
#' passes [check_contact_data()]. It is meant for tests, documentation and
#' for creating deliberately faulty variants. No real survey data are used.
#'
#' @param n_participants Number of participants.
#' @param seed Random seed.
#' @param with_holiday If `TRUE`, add the optional `holiday` column to the
#'   survey-day table (`sday`).
#' @return A named list of data frames: `participant`, `contact`, `hh`, `sday`,
#'   `participant_extra`, `contact_extra` and `dictionary`.
#' @export
#' @examples
#' tables <- example_survey(n_participants = 10)
#' names(tables)
#' head(tables$contact)
example_survey <- function(n_participants = 30, seed = 1,
                           with_holiday = FALSE) {
  withr::with_seed(seed, build_example_survey(n_participants, with_holiday))
}

build_example_survey <- function(n_participants, with_holiday = FALSE) {

  n_hh <- ceiling(n_participants / 2)
  part_id <- sprintf("P%03d", seq_len(n_participants))
  hh_id <- sprintf("H%03d", rep(seq_len(n_hh), length.out = n_participants))
  age <- sample(0:85, n_participants, replace = TRUE)
  use_interval <- seq_len(n_participants) %% 5 == 0
  participant <- data.frame(
    part_id = part_id,
    hh_id = hh_id,
    part_age_exact = ifelse(use_interval, NA, age),
    part_age_est_min = ifelse(use_interval, 10 * (age %/% 10), NA),
    part_age_est_max = ifelse(use_interval, 10 * (age %/% 10) + 9, NA),
    part_gender = sample(c("F", "M"), n_participants, replace = TRUE),
    stringsAsFactors = FALSE
  )

  n_contacts <- c(stats::rpois(n_participants - 1, 6) + 1, 0)
  contact_part <- rep(part_id, times = n_contacts)
  n <- length(contact_part)
  location <- c("cnt_home", "cnt_work", "cnt_school", "cnt_transport",
                "cnt_leisure", "cnt_otherplace")
  chosen <- sample(location, n, replace = TRUE)
  contact <- data.frame(
    part_id = contact_part,
    cnt_id = sprintf("C%05d", seq_len(n)),
    cnt_gender = sample(c("F", "M", NA), n, replace = TRUE),
    cnt_age_exact = sample(c(0:85, NA), n, replace = TRUE),
    cnt_age_est_min = NA_integer_,
    cnt_age_est_max = NA_integer_,
    cnt_age_multi = NA_character_,
    stringsAsFactors = FALSE
  )
  for (column in location) {
    contact[[column]] <- chosen == column
  }
  contact$frequency_multi <- sample(1:5, n, replace = TRUE)
  contact$duration_exact <- NA_real_
  contact$duration_multi <- sample(1:5, n, replace = TRUE)
  contact$phys_contact <- sample(c(TRUE, FALSE), n, replace = TRUE)
  # a contact with an estimated age instead of an exact age
  contact$cnt_age_est_min[2] <- 30L
  contact$cnt_age_est_max[2] <- 39L
  contact$cnt_age_exact[2] <- NA_integer_

  hh <- data.frame(
    hh_id = sprintf("H%03d", seq_len(n_hh)),
    country = "BE",
    hh_size = as.integer(table(factor(hh_id, levels = sprintf("H%03d", seq_len(n_hh)))) + 1L),
    stringsAsFactors = FALSE
  )

  dates <- as.Date("2011-01-10") + (seq_len(n_participants) %% 14)
  sday <- data.frame(
    part_id = part_id,
    sday_id = format(dates, "%Y%m%d"),
    day = as.integer(format(dates, "%d")),
    month = as.integer(format(dates, "%m")),
    year = as.integer(format(dates, "%Y")),
    dayofweek = as.POSIXlt(dates)$wday,
    stringsAsFactors = FALSE
  )

  if (with_holiday) {
    # synthetic: the second week of the example period is a "holiday"
    sday$holiday <- dates >= as.Date("2011-01-17")
  }

  participant_extra <- data.frame(
    part_id = part_id,
    panel_id = sprintf("PAN%03d", seq_len(n_participants)),
    employed = sample(c(TRUE, FALSE), n_participants, replace = TRUE),
    stringsAsFactors = FALSE
  )
  contact_extra <- data.frame(
    cnt_id = contact$cnt_id,
    cnt_imputed = FALSE,
    stringsAsFactors = FALSE
  )

  list(
    participant = participant, contact = contact, hh = hh, sday = sday,
    participant_extra = participant_extra, contact_extra = contact_extra,
    dictionary = example_dictionary(
      list(participant_common = participant, contact_common = contact,
           hh_common = hh, sday = sday, participant_extra = participant_extra,
           contact_extra = contact_extra)
    )
  )
}

example_dictionary <- function(tables) {
  schema <- read_schema("latest")
  codes <- c(
    frequency_multi = "1 = (almost) daily; 2 = weekly; 3 = monthly; 4 = less often; 5 = first time",
    duration_multi = "1 = <5 min; 2 = 5-15 min; 3 = 15-60 min; 4 = 1-4 h; 5 = >=4 h",
    cnt_age_multi = "no categories used"
  )
  rows <- lapply(names(tables), function(id) {
    columns <- names(tables[[id]])
    specs <- lapply(columns, function(column) schema_column(schema, id, column))
    data.frame(
      file = id,
      variable = columns,
      description = vapply(seq_along(columns), function(i) {
        specs[[i]]$description %||% paste("Survey-specific variable", columns[i])
      }, character(1)),
      type = vapply(seq_along(columns), function(i) {
        type <- specs[[i]]$type
        if (is.null(type)) "string" else if (type == "date_yyyymmdd") "string" else type
      }, character(1)),
      values = ifelse(columns %in% names(codes), codes[columns], NA_character_),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Write a dataset to a folder in the SOCRATES file layout
#'
#' @param tables A list as returned by [example_survey()] (or built the same
#'   way): `participant`, `contact`, `hh`, `sday`, optionally
#'   `participant_extra`, `contact_extra`, `hh_extra`, `dictionary`
#'   (written as a long-format CSV).
#' @param path Existing folder to write into.
#' @param nametag NAMETAG used as file name prefix.
#' @return The written file paths, invisibly.
#' @export
#' @examples
#' dir <- withr::local_tempdir()
#' write_survey(example_survey(), dir, nametag = "DOE_2012_BELGIUM")
#' list.files(dir)
write_survey <- function(tables, path, nametag) {
  if (!dir.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing folder.")
  }
  schema <- read_schema("latest")
  map <- c(participant = "participant_common", contact = "contact_common",
           hh = "hh_common", sday = "sday",
           participant_extra = "participant_extra",
           contact_extra = "contact_extra", hh_extra = "hh_extra")
  written <- character()
  for (name in intersect(names(map), names(tables))) {
    file <- file.path(path, paste0(nametag, "_", schema$files[[map[[name]]]]$suffix))
    write_csv_utf8(tables[[name]], file)
    written <- c(written, file)
  }
  if (!is.null(tables$dictionary)) {
    file <- file.path(path, paste0(nametag, "_dictionary.csv"))
    write_csv_utf8(tables$dictionary, file)
    written <- c(written, file)
  }
  invisible(written)
}

#' Create an empty submission skeleton
#'
#' Writes the required common files with the correct header and no rows, plus
#' a long-format dictionary skeleton pre-filled with the common variables and
#' their descriptions. Fill the files, then run [check_contact_data()].
#'
#' @param path Existing folder to write into.
#' @param nametag NAMETAG used as file name prefix.
#' @inheritParams read_schema
#' @return The written file paths, invisibly.
#' @export
#' @examples
#' dir <- withr::local_tempdir()
#' use_template(dir, "DOE_2012_BELGIUM")
#' list.files(dir)
use_template <- function(path, nametag, schema = "latest") {
  schema <- read_schema(schema)
  if (!dir.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing folder.")
  }
  written <- character()
  dictionary <- list()
  for (id in schema_ids(schema, "common")) {
    columns <- schema_column_names(schema, id)
    empty <- as.data.frame(
      stats::setNames(rep(list(character()), length(columns)), columns),
      stringsAsFactors = FALSE
    )
    file <- file.path(path, paste0(nametag, "_", schema$files[[id]]$suffix))
    write_csv_utf8(empty, file)
    written <- c(written, file)
    specs <- schema_columns(schema, id)
    dictionary[[id]] <- data.frame(
      file = id, variable = columns,
      description = purrr::map_chr(specs, function(s) s$description %||% ""),
      type = purrr::map_chr(specs, "type"),
      values = purrr::map_chr(specs, function(s) {
        if (!is.null(s$enum)) {
          paste(unlist(s$enum), collapse = "; ")
        } else if (identical(s$type, "boolean")) {
          "TRUE; FALSE"
        } else {
          ""
        }
      }),
      stringsAsFactors = FALSE
    )
  }
  file <- file.path(path, paste0(nametag, "_dictionary.csv"))
  write_csv_utf8(do.call(rbind, dictionary), file)
  invisible(c(written, file))
}

# data.table::fwrite always writes UTF-8, independent of the session locale
write_csv_utf8 <- function(x, file) {
  data.table::fwrite(x, file, na = "NA", quote = TRUE, row.names = FALSE)
}

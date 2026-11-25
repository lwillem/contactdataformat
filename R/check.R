#' Check a social contact dataset against the SOCRATES guidelines
#'
#' `check_contact_data()` reads the files in a dataset folder and runs the
#' rules of the bundled schema: automatic rules (class A: file, column, type
#' and value checks), cross-table rules (class X: keys, foreign keys,
#' dictionary), heuristic rules (class H, reported as warnings), and lists the
#' items that need human judgement (class M) as a checklist. Nothing is
#' modified. Use [list_rules()] for the catalogue.
#'
#' @param path Folder with the dataset files.
#' @param nametag NAMETAG (file name prefix). Inferred from the file names when
#'   `NULL`.
#' @param schema Bundled guideline version (`"latest"`, `"1.0"`) or path to a
#'   schema YAML file, see [read_schema()].
#' @param rules Character vector of rule ids to run. All rules when `NULL`.
#' @param skip Character vector of rule ids that are not run.
#' @param waive Named character vector: names are rule ids, values are the
#'   reasons (required). Waived findings stay in the report with severity
#'   `"waived"` and do not affect the status.
#' @param smoke_test If `TRUE`, also run rule R22: load the files with
#'   `socialmixr::load_survey()` and compute a small contact matrix. Needs the
#'   suggested package socialmixr.
#' @param quiet If `TRUE`, no progress message is shown.
#' @param stop_on_error If `TRUE`, throw an error when the report contains
#'   errors (useful in scripts and CI).
#' @return A `socrates_report` object, see [print.socrates_report()].
#' @export
#' @examples
#' dir <- withr::local_tempdir()
#' write_survey(example_survey(), dir, nametag = "DOE_2012_BELGIUM")
#' report <- check_contact_data(dir, quiet = TRUE)
#' report$status
#'
#' # waive a rule, with a reason
#' check_contact_data(dir, waive = c(R21 = "group contacts are documented"),
#'                    quiet = TRUE)$status
check_contact_data <- function(path,
                               nametag = NULL,
                               schema = "latest",
                               rules = NULL,
                               skip = NULL,
                               waive = NULL,
                               smoke_test = FALSE,
                               quiet = FALSE,
                               stop_on_error = FALSE) {
  if (!quiet) {
    cli::cli_alert_info("Reading {.path {path}}")
  }
  ctx <- read_socrates(path, nametag = nametag, schema = schema)
  ctx$smoke_test <- smoke_test
  run_checks(ctx, rules = rules, skip = skip, waive = waive,
             quiet = quiet, stop_on_error = stop_on_error)
}

#' Check data frames that are already in memory
#'
#' In-memory variant of [check_contact_data()] for scripts and tests. File
#' level rules (file names, encoding, delimiter) do not apply; all other rules
#' run on the supplied tables.
#'
#' @param participant,contact,hh,sday Data frames for the participant, contact,
#'   household and survey-day common files. `hh` and `sday` may be `NULL`, in
#'   which case the rule for the missing required table reports it.
#' @param participant_extra,contact_extra,hh_extra Optional extra tables.
#' @param dictionary Optional data frame in long format with columns `file`,
#'   `variable`, `description`, `type` and `values`.
#' @param ... Passed on to the checker: `rules`, `skip`, `waive`, `quiet`,
#'   `stop_on_error`.
#' @inheritParams check_contact_data
#' @return A `socrates_report` object.
#' @export
#' @examples
#' tables <- example_survey()
#' check_tables(tables$participant, tables$contact, tables$hh, tables$sday,
#'              quiet = TRUE)
check_tables <- function(participant,
                         contact,
                         hh = NULL,
                         sday = NULL,
                         participant_extra = NULL,
                         contact_extra = NULL,
                         hh_extra = NULL,
                         dictionary = NULL,
                         nametag = NULL,
                         schema = "latest",
                         ...) {
  schema <- read_schema(schema)
  supplied <- list(
    participant_common = participant, contact_common = contact,
    hh_common = hh, sday = sday, participant_extra = participant_extra,
    contact_extra = contact_extra, hh_extra = hh_extra
  )
  supplied <- Filter(Negate(is.null), supplied)
  tables <- lapply(supplied, as_text_table)
  dict <- NULL
  if (!is.null(dictionary)) {
    norm <- normalise_dictionary(dictionary, nametag %||% NA_character_,
                                 schema)
    dict <- list(path = NA_character_, format = "data.frame",
                 data = norm$data,
                 missing_fields = setdiff(schema$dictionary$fields,
                                          norm$found_fields),
                 note = NULL)
  }
  ctx <- structure(
    list(
      schema = schema, source = "memory", path = NA_character_,
      nametag = nametag %||% NA_character_, nametag_candidates = character(),
      tables = tables, file_names = list(), file_info = list(),
      all_files = character(), misnamed = character(),
      unrecognised = character(), restricted = character(),
      dictionary = dict, dictionary_file = NULL, smoke_test = FALSE,
      cache = new.env(parent = emptyenv())
    ),
    class = "socrates_data"
  )
  run_checks(apply_aliases(ctx), ...)
}

# Engine -------------------------------------------------------------------

run_checks <- function(ctx,
                       rules = NULL,
                       skip = NULL,
                       waive = NULL,
                       quiet = FALSE,
                       stop_on_error = FALSE) {
  schema <- ctx$schema
  all_ids <- purrr::map_chr(schema$rules, "id")
  check_rule_ids(rules, all_ids, "rules")
  check_rule_ids(skip, all_ids, "skip")
  waive <- validate_waive(waive, all_ids)

  planned <- purrr::map_lgl(schema$rules, function(r) isTRUE(r$planned))
  run_ids <- all_ids[!planned]
  if (!is.null(rules)) run_ids <- intersect(run_ids, rules)
  skipped <- intersect(skip %||% character(), run_ids)
  run_ids <- setdiff(run_ids, skipped)

  results <- lapply(run_ids, function(id) run_rule(id, ctx))
  findings <- bind_findings(results)
  findings <- finalise_findings(findings, schema, waive)

  report <- new_socrates_report(findings, ctx, run_ids, skipped,
                                all_ids[planned], waive)
  if (!quiet) {
    counts <- report$counts
    cli::cli_alert_info(paste0(
      "Status: ", report$status, " (", counts[["error"]], " error(s), ",
      counts[["warning"]], " warning(s))"
    ))
  }
  if (stop_on_error && !report$passed) {
    cli::cli_abort(c(
      "The dataset has {report$counts[['error']]} error(s).",
      i = "Use {.code print()} on the report to see them."
    ))
  }
  report
}

check_rule_ids <- function(ids, valid, arg) {
  unknown <- setdiff(ids, valid)
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown rule id(s) in {.arg {arg}}: {.val {unknown}}.")
  }
  invisible(ids)
}

validate_waive <- function(waive, valid) {
  if (is.null(waive) || length(waive) == 0) {
    return(character())
  }
  if (is.null(names(waive)) || any(names(waive) == "") ||
        !is.character(waive)) {
    cli::cli_abort(
      "{.arg waive} must be a named character vector: {.code c(R12 = \"reason\")}."
    )
  }
  check_rule_ids(names(waive), valid, "waive")
  empty <- names(waive)[is.na(waive) | trimws(waive) == ""]
  if (length(empty) > 0) {
    cli::cli_abort("Waiving {.val {empty}} requires a reason.")
  }
  waive
}

run_rule <- function(id, ctx) {
  fun <- get0(paste0("rule_", tolower(id)), envir = asNamespace("contactdataformat"),
              mode = "function")
  if (is.null(fun)) {
    return(finding(id, "Rule is in the schema but has no implementation.",
                   variant = "default"))
  }
  tryCatch(
    fun(ctx),
    error = function(e) {
      finding(id, paste0("The rule could not be evaluated: ",
                         conditionMessage(e)))
    }
  )
}

finalise_findings <- function(findings, schema, waive) {
  if (nrow(findings) == 0) {
    findings$severity <- character()
    findings$guideline_ref <- character()
    return(findings[, report_columns()])
  }
  findings$severity <- purrr::map2_chr(
    findings$rule_id, findings$variant,
    function(id, variant) severity_of(schema, id, variant)
  )
  findings$guideline_ref <- purrr::map_chr(findings$rule_id, function(id) {
    schema_rule(schema, id)$ref %||% NA_character_
  })
  is_waived <- findings$rule_id %in% names(waive) &
    findings$severity %in% c("error", "warning")
  findings$severity[is_waived] <- "waived"
  # the reason is kept in front of the message
  findings$message[is_waived] <- paste0(
    "Waived (", unname(waive[findings$rule_id[is_waived]]), "): ",
    findings$message[is_waived]
  )
  rank <- match(findings$severity, severity_levels)
  findings <- findings[order(rank, findings$rule_id, findings$file), ]
  rownames(findings) <- NULL
  findings[, report_columns()]
}

report_columns <- function() {
  c("rule_id", "severity", "file", "column", "n_fail", "n_total",
    "example_rows", "message", "guideline_ref")
}

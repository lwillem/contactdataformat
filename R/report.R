#' The `socrates_report` object
#'
#' [check_contact_data()] and [check_tables()] return a `socrates_report`: a
#' list with
#' * `findings`: data frame with columns `rule_id`, `severity` (`error`,
#'   `warning`, `note`, `manual`, `waived`), `file`, `column`, `n_fail`,
#'   `n_total`, `example_rows` (file lines, header = line 1), `message`,
#'   `guideline_ref`, `class`, `original_severity` and `waiver_reason`;
#' * `status`: `"PASS"`, `"PASS WITH WARNINGS"` or `"MANDATORY UPDATES NEEDED"`;
#' * `passed`: `TRUE` when there is no error;
#' * `counts`: number of findings per severity;
#' * `meta`: package, guideline and schema version, schema hash, timestamp,
#'   NAMETAG, folder checksum, the rules that were run, skipped or planned.
#'
#' @param x,object A `socrates_report`.
#' @param row.names,optional Ignored; for compatibility with the generic.
#' @param ... Ignored.
#' @return `print()` returns `x` invisibly; `summary()` returns a
#'   `summary.socrates_report` object; `as.data.frame()` returns the findings.
#' @name socrates_report
#' @examples
#' dir <- withr::local_tempdir()
#' write_survey(example_survey(), dir, nametag = "DOE_2012_BELGIUM")
#' report <- check_contact_data(dir, quiet = TRUE)
#' report
#' summary(report)
#' head(as.data.frame(report))
NULL

new_socrates_report <- function(findings, ctx, run_ids, skipped, planned,
                                waive) {
  counts <- vapply(severity_levels, function(level) {
    sum(findings$severity == level)
  }, integer(1))
  status <- if (counts[["error"]] > 0) {
    "MANDATORY UPDATES NEEDED"
  } else if (counts[["warning"]] > 0) {
    "PASS WITH WARNINGS"
  } else {
    "PASS"
  }
  schema <- ctx$schema
  meta <- list(
    package_version = as.character(utils::packageVersion("contactdataformat")),
    guideline_version = schema$guideline_version,
    guideline_status = schema$status,
    schema_hash = attr(schema, "hash") %||% NA_character_,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    nametag = ctx$nametag,
    source = ctx$source,
    folder_checksum = folder_checksum(ctx),
    rules_run = run_ids,
    rules_skipped = skipped,
    rules_planned = planned,
    waived = as.list(waive)
  )
  structure(
    list(findings = findings, status = status, passed = counts[["error"]] == 0,
         counts = counts, meta = meta),
    class = "socrates_report"
  )
}

folder_checksum <- function(ctx) {
  if (!identical(ctx$source, "folder")) {
    return(NA_character_)
  }
  files <- file.path(ctx$path, ctx$all_files)
  sums <- unname(tools::md5sum(files))
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(paste(ctx$all_files, sums), tmp)
  unname(tools::md5sum(tmp))
}

#' @rdname socrates_report
#' @export
as.data.frame.socrates_report <- function(x, row.names = NULL, # nolint: object_name_linter
                                          optional = FALSE, ...) {
  x$findings
}

#' @rdname socrates_report
#' @export
print.socrates_report <- function(x, ...) {
  # nolint start: object_usage_linter (variables used inside cli glue strings)
  meta <- x$meta
  status_text <- switch(x$status, `MANDATORY UPDATES NEEDED` = cli::col_red(x$status),
                        `PASS WITH WARNINGS` = cli::col_yellow(x$status),
                        cli::col_green(x$status))
  cli::cli_h1("SOCRATES data check: {status_text}")
  tag <- if (!is.na(meta$nametag)) {
    meta$nametag
  } else if (identical(meta$source, "memory")) {
    "(in-memory tables)"
  } else {
    "(could not be determined)"
  }
  cli::cli_text(
    "NAMETAG {.val {tag}}, guideline {meta$guideline_version} ",
    "({meta$guideline_status}), contactdataformat {meta$package_version}"
  )
  n <- x$counts
  cli::cli_text(
    "{n[['error']]} error(s), {n[['warning']]} warning(s), ",
    "{n[['note']]} note(s), {n[['waived']]} waived, ",
    "{n[['manual']]} manual item(s)"
  )
  # nolint end
  print_findings <- function(level, title, marker) {
    rows <- x$findings[x$findings$severity == level, , drop = FALSE]
    if (nrow(rows) == 0) return(invisible())
    cli::cli_h2(title)
    for (i in seq_len(nrow(rows))) {
      where <- location_label(rows[i, ])
      cli::cli_bullets(stats::setNames(
        paste0("{.strong ", rows$rule_id[i], "} ", where,
               cli_escape(rows$message[i])),
        marker
      ))
    }
  }
  print_findings("error", "Errors", "x")
  print_findings("warning", "Warnings", "!")
  print_findings("note", "Notes", "i")
  waived <- x$findings[x$findings$severity == "waived", , drop = FALSE]
  if (nrow(waived) > 0) {
    cli::cli_h2("Waived")
    for (i in seq_len(nrow(waived))) {
      cli::cli_bullets(c(
        "*" = paste0(waived$rule_id[i], ": ",
                     cli_escape(waived$waiver_reason[i]))
      ))
    }
  }
  manual <- x$findings[x$findings$severity == "manual", , drop = FALSE]
  if (nrow(manual) > 0) {
    cli::cli_h2("Manual checklist (needs review)")
    for (i in seq_len(nrow(manual))) {
      cli::cli_text("[ ] {manual$rule_id[i]}: {manual$message[i]}")
    }
  }
  invisible(x)
}

location_label <- function(row) {
  parts <- c(row$file, row$column)
  parts <- parts[!is.na(parts)]
  label <- paste(parts, collapse = " / ")
  if (nzchar(label)) {
    label <- paste0("[", label, "] ")
  }
  if (!is.na(row$n_fail) && !is.na(row$n_total)) {
    label <- paste0(label, "(", row$n_fail, "/", row$n_total, ") ")
  } else if (!is.na(row$n_fail)) {
    label <- paste0(label, "(", row$n_fail, ") ")
  }
  cli_escape(label)
}

# Braces in messages (e.g. the regex `[0-9]{4}`) must not be read as cli glue
cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

#' @rdname socrates_report
#' @export
summary.socrates_report <- function(object, ...) {
  findings <- object$findings
  by_rule <- if (nrow(findings) > 0) {
    as.data.frame(table(rule_id = findings$rule_id,
                        severity = findings$severity),
                  stringsAsFactors = FALSE)
  } else {
    data.frame(rule_id = character(), severity = character(),
               Freq = integer())
  }
  by_rule <- by_rule[by_rule$Freq > 0, , drop = FALSE]
  names(by_rule)[names(by_rule) == "Freq"] <- "n"
  rownames(by_rule) <- NULL
  structure(
    list(status = object$status, passed = object$passed,
         counts = object$counts, by_rule = by_rule, meta = object$meta),
    class = "summary.socrates_report"
  )
}

#' @rdname socrates_report
#' @export
print.summary.socrates_report <- function(x, ...) {
  cat("SOCRATES data check:", x$status, "\n")
  cat("Guideline", x$meta$guideline_version, "| contactdataformat",
      x$meta$package_version, "| NAMETAG", x$meta$nametag, "\n\n")
  print(x$counts)
  if (nrow(x$by_rule) > 0) {
    cat("\nFindings per rule:\n")
    print(x$by_rule, row.names = FALSE)
  }
  invisible(x)
}

#' Write a report to Markdown, CSV or JSON
#'
#' @param report A [socrates_report][socrates_report] object.
#' @param file Output file.
#' @param format `"md"`, `"csv"` or `"json"`. Inferred from the extension of
#'   `file` when `NULL`.
#' @return `file`, invisibly.
#' @export
#' @examples
#' dir <- withr::local_tempdir()
#' write_survey(example_survey(), dir, nametag = "DOE_2012_BELGIUM")
#' report <- check_contact_data(dir, quiet = TRUE)
#' out <- report_write(report, file.path(dir, "report.md"))
#' readLines(out, n = 3)
report_write <- function(report, file, format = NULL) {
  if (!inherits(report, "socrates_report")) {
    cli::cli_abort("{.arg report} must be a {.cls socrates_report}.")
  }
  format <- format %||% tolower(tools::file_ext(file))
  format <- match.arg(format, c("md", "csv", "json"))
  switch(
    format,
    csv = write_csv_utf8(report$findings, file),
    json = writeLines(report_json(report), file, useBytes = TRUE),
    md = writeLines(report_markdown(report), file, useBytes = TRUE)
  )
  invisible(file)
}

report_json <- function(report) {
  jsonlite::toJSON(
    list(status = report$status, passed = report$passed,
         counts = as.list(report$counts), meta = report$meta,
         findings = report$findings),
    auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null"
  )
}

md_escape <- function(x) {
  x <- gsub("|", "\\|", x, fixed = TRUE)
  gsub("\n", " ", x, fixed = TRUE)
}

report_markdown <- function(report) {
  meta <- report$meta
  n <- report$counts
  lines <- c(
    paste0("# SOCRATES data check: ", report$status),
    "",
    paste0("* NAMETAG: `", meta$nametag, "`"),
    paste0("* Guideline version: ", meta$guideline_version, " (",
           meta$guideline_status, "), schema hash `", meta$schema_hash, "`"),
    paste0("* contactdataformat ", meta$package_version, ", ", meta$timestamp),
    paste0("* Folder checksum: `", meta$folder_checksum, "`"),
    paste0("* ", n[["error"]], " error(s), ", n[["warning"]], " warning(s), ",
           n[["note"]], " note(s), ", n[["waived"]], " waived, ",
           n[["manual"]], " manual item(s)"),
    ""
  )
  section <- function(level, title) {
    rows <- report$findings[report$findings$severity == level, , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)
    c(
      paste0("## ", title), "",
      "| Rule | File | Column | Failing | Rows | Message |",
      "|---|---|---|---|---|---|",
      paste0("| ", rows$rule_id, " | ", md_escape(ifelse(is.na(rows$file), "", rows$file)),
             " | ", ifelse(is.na(rows$column), "", rows$column), " | ",
             ifelse(is.na(rows$n_fail), "",
                    paste0(rows$n_fail, ifelse(is.na(rows$n_total), "",
                                               paste0("/", rows$n_total)))),
             " | ", ifelse(is.na(rows$example_rows), "", rows$example_rows),
             " | ", md_escape(rows$message), " |"),
      ""
    )
  }
  lines <- c(lines, section("error", "Errors"), section("warning", "Warnings"),
             section("note", "Notes"))
  waived <- report$findings[report$findings$severity == "waived", , drop = FALSE]
  if (nrow(waived) > 0) {
    lines <- c(lines, "## Waived", "",
               paste0("* ", waived$rule_id, ": ", waived$waiver_reason,
                      " (", md_escape(waived$message), ")"), "")
  }
  manual <- report$findings[report$findings$severity == "manual", , drop = FALSE]
  if (nrow(manual) > 0) {
    lines <- c(lines, "## Manual checklist", "",
               paste0("- [ ] ", manual$rule_id, ": ", manual$message), "")
  }
  if (length(meta$rules_planned) > 0) {
    lines <- c(lines, paste0("*Not yet implemented: ",
                             paste(meta$rules_planned, collapse = ", "), "*"))
  }
  lines
}

#' Check one dataset, or every dataset in a folder, and write the reports
#'
#' `run_check()` is the one-call entry point for RStudio and scripts. It runs
#' [check_contact_data()] on a dataset folder, or on every dataset sub-folder
#' of a parent folder, writes a report per dataset and one summary table, and
#' prints the summary. The data are only read; reports are never written into
#' a dataset folder.
#'
#' Each sub-folder of `path` that holds at least one `.csv` file is a dataset
#' (one NAMETAG per sub-folder). When there is no such sub-folder, `path`
#' itself is the dataset. Use `single` to override this. Hidden folders, folders starting with `_` and
#' the output folder are skipped.
#'
#' @param path A dataset folder, or a folder with one sub-folder per dataset.
#' @param output_dir Folder for the reports. Default: `check_reports` inside
#'   `path` when `path` holds several datasets, and next to `path` (in its
#'   parent folder) when `path` is one dataset. It is created when needed.
#' @param formats Report formats to write per dataset, any of `"csv"`, `"md"`
#'   and `"json"`, see [report_write()]. A table `check_summary.csv` is always
#'   written.
#' @param write If `FALSE`, nothing is written and only the summary is
#'   returned and printed.
#' @param single `TRUE`: `path` is one dataset; `FALSE`: `path` holds one
#'   sub-folder per dataset. Detected when `NULL` (see Details).
#' @param show What to print per dataset: `"summary"` (only the summary
#'   table), `"errors"` (also the errors and warnings of each dataset) or
#'   `"all"` (the full report of each dataset).
#' @param smoke_test If `TRUE`, also load each dataset with socialmixr (rule
#'   R22; slower). Needs the suggested package socialmixr.
#' @param ... Further arguments for [check_contact_data()], such as `schema`,
#'   `skip` or `waive`. A `nametag` can only be given for a single dataset.
#' @return A `socrates_run` object, invisibly: a list with `summary` (one row
#'   per dataset), `reports` (the `socrates_report` objects, named by dataset),
#'   `output_dir` (`NULL` when nothing was written) and `files` (the files
#'   written).
#' @seealso [check_contact_data()] for one dataset without writing files.
#' @export
#' @examples
#' dir <- withr::local_tempdir()
#' dir.create(file.path(dir, "DOE_2012_BELGIUM"))
#' write_survey(example_survey(), file.path(dir, "DOE_2012_BELGIUM"),
#'              nametag = "DOE_2012_BELGIUM")
#' run <- run_check(dir)
#' run$summary
#' run$reports[["DOE_2012_BELGIUM"]]$status
run_check <- function(path,
                      output_dir = NULL,
                      formats = c("csv", "md"),
                      write = TRUE,
                      single = NULL,
                      show = c("summary", "errors", "all"),
                      smoke_test = FALSE,
                      ...) {
  show <- match.arg(show)
  formats <- match.arg(formats, c("csv", "md", "json"), several.ok = TRUE)
  if (!is.character(path) || length(path) != 1 || !dir.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing folder.")
  }
  path <- normalizePath(path, mustWork = TRUE)
  if (is.null(single)) {
    # sub-folders with data win: loose .csv files next to them are usually
    # earlier output (summary tables), not a dataset
    single <- has_csv(path) && length(dataset_folders(path, "check_reports")) == 0
  }
  if (is.null(output_dir)) {
    output_dir <- if (single) {
      file.path(dirname(path), "check_reports")
    } else {
      file.path(path, "check_reports")
    }
  }
  folders <- if (single) path else dataset_folders(path, output_dir)
  if (length(folders) == 0) {
    cli::cli_abort("No dataset folders with {.file .csv} files found in {.path {path}}.")
  }
  names(folders) <- basename(folders)
  if (anyDuplicated(names(folders))) {
    cli::cli_abort("Dataset folder names must be unique.")
  }
  if (write) {
    if (!single) {
      inside <- vapply(folders, function(f) path_inside(output_dir, f),
                       logical(1))
      if (any(inside)) {
        cli::cli_abort(
          "{.arg output_dir} must not be inside a dataset folder."
        )
      }
    } else if (path_inside(output_dir, path)) {
      cli::cli_abort("{.arg output_dir} must not be inside the dataset folder.")
    }
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_dir <- normalizePath(output_dir, mustWork = TRUE)
  }
  cli::cli_alert_info("Checking {length(folders)} dataset{?s} in {.path {path}}")

  reports <- list()
  rows <- list()
  files <- character()
  for (name in names(folders)) {
    cli::cli_h2(name)
    report <- tryCatch(
      check_contact_data(folders[[name]], smoke_test = smoke_test,
                         quiet = TRUE, ...),
      error = function(e) {
        cli::cli_alert_danger("The check failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(report)) {
      rows[[name]] <- summary_row(name, NULL)
      next
    }
    reports[[name]] <- report
    rows[[name]] <- summary_row(name, report)
    if (write) {
      for (format in formats) {
        file <- file.path(output_dir, paste0(name, ".", format))
        report_write(report, file, format = format)
        files <- c(files, file)
      }
    }
    if (show == "all") {
      print(report)
    } else if (show == "errors") {
      print_problems(report)
    } else {
      counts <- report$counts
      cli::cli_text(
        "{report$status}: {counts[['error']]} error(s), ",
        "{counts[['warning']]} warning(s)"
      )
    }
  }

  summary_table <- do.call(rbind, rows)
  rownames(summary_table) <- NULL
  if (write) {
    summary_file <- file.path(output_dir, "check_summary.csv")
    write_csv_utf8(summary_table, summary_file)
    files <- c(files, summary_file)
  }
  run <- structure(
    list(summary = summary_table, reports = reports,
         output_dir = if (write) output_dir else NULL, files = files),
    class = "socrates_run"
  )
  print(run)
  invisible(run)
}

#' @export
print.socrates_run <- function(x, ...) {
  cli::cli_h1("Summary")
  print(x$summary, row.names = FALSE)
  if (!is.null(x$output_dir)) {
    cli::cli_alert_success(
      "{length(x$files)} file{?s} written to {.path {x$output_dir}}"
    )
  }
  invisible(x)
}

summary_row <- function(name, report) {
  if (is.null(report)) {
    return(data.frame(dataset = name, status = "NOT CHECKED", errors = NA,
                      warnings = NA, notes = NA, manual = NA,
                      stringsAsFactors = FALSE))
  }
  data.frame(
    dataset = name,
    status = report$status,
    errors = report$counts[["error"]],
    warnings = report$counts[["warning"]],
    notes = report$counts[["note"]],
    manual = report$counts[["manual"]],
    stringsAsFactors = FALSE
  )
}

# Errors and warnings of one report, without the notes and the checklist
print_problems <- function(report) {
  shown <- report
  shown$findings <- report$findings[
    report$findings$severity %in% c("error", "warning"), , drop = FALSE
  ]
  print(shown)
}

has_csv <- function(folder) {
  length(list.files(folder, pattern = "\\.csv$", ignore.case = TRUE)) > 0
}

dataset_folders <- function(path, output_dir) {
  folders <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  skip <- folders == absolute_path(output_dir) |
    basename(folders) == basename(output_dir) |
    grepl("^[._]", basename(folders))
  folders <- folders[!skip]
  folders[vapply(folders, has_csv, logical(1))]
}

# Absolute path, also for a folder that does not exist yet: the nearest
# existing parent is resolved (symlinks such as /var -> /private/var)
absolute_path <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  rest <- character()
  while (!file.exists(path) && dirname(path) != path) {
    rest <- c(basename(path), rest)
    path <- dirname(path)
  }
  do.call(file.path, as.list(c(normalizePath(path, mustWork = FALSE), rest)))
}

path_inside <- function(path, folder) {
  path <- absolute_path(path)
  folder <- absolute_path(folder)
  path == folder || startsWith(path, paste0(folder, .Platform$file.sep))
}

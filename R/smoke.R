# R22 and R35: reference-implementation smoke test ------------------------------

# Run socialmixr once per context and cache the outcome
smoke_outcome <- function(ctx) {
  if (!is.null(ctx$cache$smoke)) {
    return(ctx$cache$smoke)
  }
  files <- list.files(ctx$path, pattern = "\\.csv$", full.names = TRUE,
                      ignore.case = TRUE)
  seen <- new.env(parent = emptyenv())
  seen$load_warnings <- character()
  seen$matrix_warnings <- character()
  seen$stage <- "load"
  collect <- function(w) {
    key <- if (identical(seen$stage, "load")) "load_warnings" else "matrix_warnings"
    seen[[key]] <- c(seen[[key]], conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  survey <- NULL
  error <- tryCatch(
    withCallingHandlers(
      suppressMessages({
        survey <- socialmixr::load_survey(files)
        seen$stage <- "matrix"
        socialmixr::contact_matrix(
          survey, age_limits = c(0, 18, 60),
          missing_participant_age = "remove", missing_contact_age = "remove"
        )
        NULL
      }),
      warning = collect
    ),
    error = function(e) conditionMessage(e)
  )
  outcome <- list(
    error = error, stage = seen$stage, load_warnings = seen$load_warnings,
    matrix_warnings = seen$matrix_warnings,
    n_participants = if (is.null(survey)) NA_integer_ else nrow(survey$participants),
    n_contacts = if (is.null(survey)) NA_integer_ else nrow(survey$contacts)
  )
  ctx$cache$smoke <- outcome
  outcome
}

# NULL when the smoke test should run, otherwise a finding explaining why not
smoke_unavailable <- function(ctx, rule_id) {
  if (!isTRUE(ctx$smoke_test)) {
    return(list(finding = NULL))
  }
  if (identical(ctx$source, "memory")) {
    return(list(finding = finding(
      rule_id, variant = "skipped",
      message = "The smoke test needs files on disk; skipped."
    )))
  }
  if (!requireNamespace("socialmixr", quietly = TRUE)) {
    return(list(finding = finding(
      rule_id, variant = "skipped",
      message = "Package socialmixr is not installed; skipped."
    )))
  }
  NULL
}

rule_r22 <- function(ctx) {
  skip <- smoke_unavailable(ctx, "R22")
  if (!is.null(skip)) {
    return(skip$finding)
  }
  out <- list()
  reference <- ctx$schema$reference_implementation$socialmixr
  installed <- utils::packageVersion("socialmixr")
  if (!is.null(reference) && installed < reference) {
    out[[1]] <- finding(
      "R22", variant = "skipped",
      message = paste0("Installed socialmixr ", installed, " is older than ",
                       "the reference implementation ", reference, "; results ",
                       "may differ.")
    )
  }
  outcome <- smoke_outcome(ctx)
  if (!is.null(outcome$error)) {
    out[[length(out) + 1]] <- finding(
      "R22", file = basename(ctx$path),
      message = paste0("socialmixr (", installed, ") failed to ",
                       if (identical(outcome$stage, "load")) "load the files"
                       else "compute a contact matrix", ": ", outcome$error)
    )
  }
  deprecated <- grep("deprecat", c(outcome$load_warnings,
                                   outcome$matrix_warnings), value = TRUE)
  if (length(deprecated) > 0) {
    out[[length(out) + 1]] <- finding(
      "R22", file = basename(ctx$path),
      message = paste0("socialmixr emitted a deprecation warning: ",
                       deprecated[1])
    )
  }
  bind_findings(out)
}

rule_r35 <- function(ctx) {
  skip <- smoke_unavailable(ctx, "R35")
  if (!is.null(skip)) {
    return(skip$finding)
  }
  outcome <- smoke_outcome(ctx)
  if (identical(outcome$stage, "load") && !is.null(outcome$error)) {
    return(NULL)
  }
  out <- list()
  merge_warnings <- grep("merge|cartesian|duplicat|key", outcome$load_warnings,
                         value = TRUE, ignore.case = TRUE)
  dictionary <- ctx$dictionary_file
  if (!is.null(dictionary) && grepl("\\.csv$", dictionary)) {
    about_dictionary <- grepl(dictionary, merge_warnings, fixed = TRUE)
    if (any(about_dictionary)) {
      out[[length(out) + 1]] <- finding(
        "R35", variant = "dictionary_csv", file = dictionary,
        message = paste0(
          "socialmixr tries to merge the CSV dictionary and warns ",
          "(harmless, but noisy). The guideline dictionary is `.xlsx`; ",
          "prefer it in the public record."
        )
      )
    }
    merge_warnings <- merge_warnings[!about_dictionary]
  }
  if (length(merge_warnings) > 0) {
    out[[1]] <- finding(
      "R35", file = basename(ctx$path),
      message = paste0("socialmixr reported a merge problem while loading: ",
                       merge_warnings[1])
    )
  }
  expected <- c(participants = nrow(ctx$tables$participant_common),
                contacts = nrow(ctx$tables$contact_common))
  found <- c(participants = outcome$n_participants,
             contacts = outcome$n_contacts)
  if (length(expected) == 2 && !anyNA(expected) && !anyNA(found) &&
        !identical(unname(expected), unname(found))) {
    out[[length(out) + 1]] <- finding(
      "R35", file = basename(ctx$path),
      message = paste0(
        "Row counts after loading differ from the common files (participants ",
        found[["participants"]], " vs ", expected[["participants"]],
        ", contacts ", found[["contacts"]], " vs ", expected[["contacts"]],
        "): files were merged unintentionally."
      )
    )
  }
  bind_findings(out)
}

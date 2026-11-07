# Internal helpers -----------------------------------------------------------

# Rule classes and severities as documented in the review (sec. 3.6)
severity_levels <- c("error", "warning", "note", "manual", "waived")

# Build one finding. `variant` selects an entry of `severities` in the schema.
finding <- function(rule_id,
                    message,
                    variant = "default",
                    file = NA_character_,
                    column = NA_character_,
                    n_fail = NA_integer_,
                    n_total = NA_integer_,
                    rows = NULL) {
  data.frame(
    rule_id = rule_id,
    variant = variant,
    file = as.character(file),
    column = as.character(column),
    n_fail = as.integer(n_fail),
    n_total = as.integer(n_total),
    example_rows = example_rows(rows),
    message = message,
    stringsAsFactors = FALSE
  )
}

bind_findings <- function(x) {
  x <- Filter(Negate(is.null), x)
  if (length(x) == 0) {
    return(empty_findings())
  }
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}

empty_findings <- function() {
  data.frame(
    rule_id = character(), variant = character(), file = character(),
    column = character(), n_fail = integer(), n_total = integer(),
    example_rows = character(), message = character(),
    stringsAsFactors = FALSE
  )
}

# Show up to `max_n` row numbers; row numbers are file lines (header = line 1)
example_rows <- function(rows, max_n = 5) {
  if (is.null(rows) || length(rows) == 0) {
    return(NA_character_)
  }
  rows <- sort(unique(rows)) + 1L
  shown <- paste(utils::head(rows, max_n), collapse = ", ")
  if (length(rows) > max_n) {
    shown <- paste0(shown, ", ... (", length(rows) - max_n, " more)")
  }
  shown
}

# Show up to `max_n` distinct values, quoted
show_values <- function(x, max_n = 5) {
  x <- unique(x[!is.na(x)])
  shown <- paste0("'", utils::head(x, max_n), "'", collapse = ", ")
  if (length(x) > max_n) {
    shown <- paste0(shown, ", ...")
  }
  shown
}

collapse_names <- function(x, max_n = 8) {
  shown <- paste0("`", utils::head(x, max_n), "`", collapse = ", ")
  if (length(x) > max_n) {
    shown <- paste0(shown, ", ... (", length(x) - max_n, " more)")
  }
  shown
}

# Convert a vector of strings to numeric, silently giving NA for non-numbers
to_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

matches <- function(x, pattern) {
  grepl(pattern, x, perl = TRUE)
}

has_table <- function(ctx, id) {
  !is.null(ctx$tables[[id]])
}

has_column <- function(ctx, id, column) {
  has_table(ctx, id) && column %in% names(ctx$tables[[id]])
}

get_column <- function(ctx, id, column) {
  if (has_column(ctx, id, column)) ctx$tables[[id]][[column]] else NULL
}

# Name of the file used in messages: real file name, or schema id in memory
file_label <- function(ctx, id) {
  ctx$file_names[[id]] %||% id
}

#' Strip a NAMETAG prefix and `.csv` from a file or sheet name.
#' @noRd
normalise_file_id <- function(x, nametag = NA_character_) {
  x <- sub("\\.(csv|xlsx|xls)$", "", trimws(x), ignore.case = TRUE)
  if (!is.na(nametag)) {
    x <- sub(paste0("^", nametag, "_"), "", x)
  }
  tolower(x)
}

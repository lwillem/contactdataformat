#' Bundled schema versions
#'
#' The schema is a YAML file that is the single source of truth for the
#' checker (file names, columns, types, allowed values, keys, severities and
#' thresholds). One file is bundled per guideline version.
#'
#' @return `guideline_versions()` returns a character vector of bundled
#'   guideline versions, oldest first.
#' @export
#' @examples
#' guideline_versions()
guideline_versions <- function() {
  files <- schema_files()
  versions <- sub("^socrates-guidelines-(.*)\\.yaml$", "\\1", basename(files))
  versions[order(numeric_version(versions))]
}

schema_files <- function() {
  dir <- system.file("schema", package = "contactdataformat")
  list.files(dir, pattern = "^socrates-guidelines-.*\\.yaml$",
             full.names = TRUE)
}

#' Read a schema
#'
#' @param schema Either a bundled guideline version (`"latest"`, `"1.0"`) or
#'   the path to a schema YAML file. A path lets you try a modified schema,
#'   for example with a different severity or an open decision resolved
#'   differently.
#' @return A list with the parsed schema and attributes `path` and `hash`
#'   (MD5 of the file).
#' @export
#' @examples
#' schema <- read_schema("latest")
#' schema$guideline_version
read_schema <- function(schema = "latest") {
  if (is.list(schema) && !is.null(schema$guideline_version)) {
    return(schema)
  }
  if (!is.character(schema) || length(schema) != 1) {
    cli::cli_abort("{.arg schema} must be a version string or a file path.")
  }
  if (file.exists(schema)) {
    path <- schema
  } else {
    versions <- guideline_versions()
    version <- if (identical(schema, "latest")) utils::tail(versions, 1) else schema
    if (!version %in% versions) {
      cli::cli_abort(c(
        "Unknown schema {.val {schema}}.",
        i = "Bundled guideline versions: {.val {versions}}."
      ))
    }
    path <- system.file("schema",
                        paste0("socrates-guidelines-", version, ".yaml"),
                        package = "contactdataformat")
  }
  parsed <- yaml::read_yaml(path)
  attr(parsed, "path") <- path
  attr(parsed, "hash") <- unname(tools::md5sum(path))
  parsed
}

#' Rule catalogue
#'
#' @inheritParams read_schema
#' @return A data frame with one row per rule: `rule_id`, `class`
#'   (A = automatic, X = cross-table, H = heuristic, M = manual), default
#'   `severity`, `title`, `guideline_ref` and `implemented`.
#' @export
#' @examples
#' list_rules()
list_rules <- function(schema = "latest") {
  schema <- read_schema(schema)
  rules <- schema$rules
  rules <- rules[order(purrr::map_chr(rules, "id"))]
  data.frame(
    rule_id = purrr::map_chr(rules, "id"),
    class = purrr::map_chr(rules, "class"),
    severity = purrr::map_chr(rules, "severity"),
    title = purrr::map_chr(rules, "title"),
    guideline_ref = purrr::map_chr(rules, function(r) r$ref %||% NA_character_),
    implemented = !purrr::map_lgl(rules, function(r) isTRUE(r$planned)),
    stringsAsFactors = FALSE
  )
}

#' Open decisions of the guideline
#'
#' Lists the choices that are still open in the guideline and the schema key
#' that encodes the current choice, so that they are easy to change.
#'
#' @inheritParams read_schema
#' @return A data frame with `id`, `topic` and `where`.
#' @export
#' @examples
#' open_decisions()
open_decisions <- function(schema = "latest") {
  schema <- read_schema(schema)
  data.frame(
    id = purrr::map_chr(schema$decisions, "id"),
    topic = purrr::map_chr(schema$decisions, "topic"),
    where = purrr::map_chr(schema$decisions, "where"),
    stringsAsFactors = FALSE
  )
}

# Schema accessors ------------------------------------------------------------

schema_rule <- function(schema, rule_id) {
  hit <- Filter(function(r) identical(r$id, rule_id), schema$rules)
  if (length(hit) == 0) NULL else hit[[1]]
}

schema_columns <- function(schema, id) {
  schema$files[[id]]$columns %||% list()
}

schema_column_names <- function(schema, id, required_only = FALSE) {
  cols <- schema_columns(schema, id)
  if (required_only) {
    cols <- Filter(function(col) !isFALSE(col$required), cols)
  }
  purrr::map_chr(cols, "name")
}

schema_column <- function(schema, id, name) {
  hit <- Filter(function(col) identical(col$name, name),
                schema_columns(schema, id))
  if (length(hit) == 0) NULL else hit[[1]]
}

schema_ids <- function(schema, kind = NULL) {
  ids <- names(schema$files)
  if (is.null(kind)) {
    return(ids)
  }
  ids[purrr::map_chr(schema$files, "kind") == kind]
}

# Key column of a file id (extras use the key of their common file)
schema_key <- function(schema, id) {
  spec <- schema$files[[id]]
  if (identical(spec$kind, "extra")) {
    spec <- schema$files[[spec$common]]
  }
  spec$key
}

severity_of <- function(schema, rule_id, variant = "default") {
  rule <- schema_rule(schema, rule_id)
  if (is.null(rule)) {
    return("error")
  }
  rule$severities[[variant]] %||% rule$severity
}

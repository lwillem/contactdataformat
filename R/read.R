#' Read a SOCRATES dataset the way the checker sees it
#'
#' Reads all files of a dataset folder as text (every column is a character
#' vector, the string `NA` is the only missing-value token) and records
#' format problems (encoding, delimiter, blank cells) for the format rules.
#' Nothing is modified on disk.
#'
#' @param path Folder with the dataset files.
#' @param nametag NAMETAG used as file name prefix. Inferred from the file
#'   names when `NULL`.
#' @inheritParams read_schema
#' @return An object of class `socrates_data`: a list with the parsed
#'   `tables` (named by schema file id), `file_names`, per-file format
#'   information (`file_info`), the dictionary and metadata when present.
#' @export
#' @examples
#' dir <- withr::local_tempdir()
#' write_survey(example_survey(), dir, nametag = "DOE_2012_BELGIUM")
#' x <- read_socrates(dir)
#' names(x$tables)
read_socrates <- function(path, nametag = NULL, schema = "latest") {
  schema <- read_schema(schema)
  if (!dir.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing folder: {.path {path}}.")
  }
  all_files <- sort(list.files(path, all.files = FALSE))
  all_files <- all_files[!dir.exists(file.path(path, all_files))]

  known <- known_suffixes(schema)
  inferred <- infer_nametags(all_files, known)
  tag <- nametag %||% (if (length(inferred) > 0) names(inferred)[1] else NA)
  tag <- as.character(tag)

  ids <- names(schema$files)
  file_names <- list()
  tables <- list()
  file_info <- list()
  for (id in ids) {
    expected <- paste0(tag, "_", schema$files[[id]]$suffix)
    if (!is.na(tag) && expected %in% all_files) {
      file_names[[id]] <- expected
      parsed <- read_csv_text(file.path(path, expected))
      file_info[[id]] <- parsed$info
      tables[id] <- list(parsed$data)
    }
  }

  recognised <- unlist(file_names, use.names = FALSE)
  misnamed <- setdiff(find_misnamed(all_files, known, tag), recognised)
  dictionary_file <- find_dictionary(all_files, tag, schema)
  metadata_file <- paste0(tag, "_", schema$metadata$basename)
  metadata_file <- if (metadata_file %in% all_files) metadata_file else NULL
  other <- setdiff(all_files, c(recognised, misnamed, dictionary_file,
                                metadata_file))
  csv_other <- other[grepl("\\.csv$", other, ignore.case = TRUE)]
  restricted <- csv_other[restricted_file_name(csv_other, schema)]

  dictionary <- if (!is.null(dictionary_file)) {
    read_dictionary(file.path(path, dictionary_file), tag, schema)
  }
  metadata <- if (!is.null(metadata_file)) {
    read_metadata(file.path(path, metadata_file))
  }

  structure(
    list(
      schema = schema, source = "folder", path = path, nametag = tag,
      nametag_candidates = names(inferred), tables = tables,
      file_names = file_names, file_info = file_info,
      all_files = all_files, misnamed = misnamed,
      unrecognised = setdiff(csv_other, restricted),
      restricted = restricted, dictionary = dictionary,
      dictionary_file = dictionary_file, metadata = metadata,
      metadata_file = metadata_file, cache = new.env(parent = emptyenv())
    ),
    class = "socrates_data"
  )
}

#' @export
print.socrates_data <- function(x, ...) {
  cli::cli_text("{.cls socrates_data} for NAMETAG {.val {x$nametag}}")
  cli::cli_text("Tables read: {.val {names(x$tables)}}")
  invisible(x)
}

known_suffixes <- function(schema) {
  main <- purrr::map_chr(schema$files, "suffix")
  restricted <- purrr::map_chr(schema$restricted_files, "suffix")
  dict <- paste0(schema$dictionary$basename,
                 ".", schema$options$dictionary_extensions)
  unname(c(main, restricted, dict, schema$metadata$basename))
}

# Prefixes (candidate NAMETAGs) of files that end in a known suffix, most
# frequent first
infer_nametags <- function(files, suffixes) {
  prefixes <- character()
  for (suffix in suffixes) {
    pattern <- paste0("^(.+)_", gsub(".", "\\.", suffix, fixed = TRUE), "$")
    hit <- files[grepl(pattern, files)]
    prefixes <- c(prefixes, sub(pattern, "\\1", hit))
  }
  counts <- sort(table(prefixes), decreasing = TRUE)
  stats::setNames(as.integer(counts), names(counts))
}

find_misnamed <- function(files, suffixes, tag) {
  out <- character()
  for (suffix in suffixes) {
    pattern <- paste0("_", gsub(".", "\\.", suffix, fixed = TRUE), "$")
    hit <- files[grepl(pattern, files)]
    out <- c(out, hit[!startsWith(hit, paste0(tag, "_"))])
  }
  unique(out)
}

find_dictionary <- function(files, tag, schema) {
  candidates <- paste0(tag, "_", schema$dictionary$basename, ".",
                       schema$options$dictionary_extensions)
  hit <- candidates[candidates %in% files]
  if (length(hit) == 0) NULL else hit[1]
}

# File names (without nametag) that mark a restricted geography file
restricted_file_name <- function(files, schema) {
  tokens <- schema$privacy$restricted_file_tokens
  purrr::map_lgl(files, function(file) {
    parts <- strsplit(tolower(sub("\\.[^.]*$", "", file)), "[^a-z0-9]+")[[1]]
    any(parts %in% tokens)
  })
}

# Read one CSV as text and collect format information ------------------------

read_csv_text <- function(file) {
  info <- list(path = file, utf8 = TRUE, bom = FALSE, delimiter = ",",
               parse_error = NULL, parse_warnings = character(),
               header = character(), blank = list(), n_rows = 0L)
  raw_start <- readBin(file, "raw", n = 3)
  info$bom <- length(raw_start) == 3 &&
    identical(as.integer(raw_start), c(239L, 187L, 191L))
  lines <- readLines(file, warn = FALSE, encoding = "bytes")
  info$utf8 <- all(validUTF8(lines))
  header_line <- if (length(lines) > 0) lines[1] else ""
  if (!info$utf8) {
    header_line <- iconv(header_line, "latin1", "UTF-8")
  }
  if (!grepl(",", header_line, fixed = TRUE)) {
    if (grepl(";", header_line, fixed = TRUE)) info$delimiter <- ";"
    if (grepl("\t", header_line, fixed = TRUE)) info$delimiter <- "\t"
  }

  warnings_seen <- character()
  data <- tryCatch(
    withCallingHandlers(
      fread(file, sep = ",", header = TRUE, colClasses = "character",
            na.strings = "NA", strip.white = FALSE, data.table = FALSE,
            check.names = FALSE, showProgress = FALSE),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      info$parse_error <<- conditionMessage(e)
      NULL
    }
  )
  info$parse_warnings <- warnings_seen
  if (is.null(data)) {
    return(list(data = NULL, info = info))
  }
  if (!info$utf8) {
    names(data) <- iconv(names(data), "latin1", "UTF-8")
    data[] <- lapply(data, function(col) iconv(col, "latin1", "UTF-8"))
  }
  names(data) <- sub("^\ufeff", "", names(data))
  info$header <- names(data)
  info$n_rows <- nrow(data)
  for (nm in names(data)) {
    col <- data[[nm]]
    blank <- which(!is.na(col) & trimws(col) == "")
    if (length(blank) > 0) {
      info$blank[[nm]] <- blank
      col[blank] <- NA_character_
      data[[nm]] <- col
    }
  }
  list(data = data, info = info)
}

# Convert user-supplied data frames to the text representation used by rules
as_text_table <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x[] <- lapply(x, function(col) {
    if (is.logical(col)) {
      out <- ifelse(is.na(col), NA_character_, ifelse(col, "TRUE", "FALSE"))
    } else if (is.numeric(col)) {
      out <- ifelse(is.na(col), NA_character_, format(col, scientific = FALSE,
                                                      trim = TRUE, digits = 15,
                                                      drop0trailing = TRUE))
    } else {
      out <- as.character(col)
    }
    out[!is.na(out) & out == ""] <- NA_character_
    out
  })
  x
}

# Dictionary and metadata ------------------------------------------------------

dictionary_field_aliases <- list(
  file = c("file", "table", "sheet"),
  variable = c("variable", "name", "column", "var", "variable name",
               "column name"),
  description = c("description", "label", "definition", "short description"),
  type = c("type", "data type", "datatype", "variable type"),
  values = c("values", "allowed values", "allowed values and meaning",
             "coding", "codes", "categories", "allowed value", "value labels")
)

canonical_field <- function(header) {
  key <- trimws(gsub("[^a-z]+", " ", tolower(header)))
  out <- rep(NA_character_, length(key))
  for (field in names(dictionary_field_aliases)) {
    out[is.na(out) & key %in% dictionary_field_aliases[[field]]] <- field
  }
  out
}

# Normalise a dictionary (data frame with a `file` column) to the standard
# columns. Returns a list(data, missing_fields, note).
normalise_dictionary <- function(x, nametag, schema, from_sheets = FALSE) {
  fields <- schema$dictionary$fields
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  canon <- canonical_field(names(x))
  keep <- !is.na(canon) & !duplicated(canon)
  x <- x[, keep, drop = FALSE]
  names(x) <- canon[keep]
  for (field in c("file", fields)) {
    if (!field %in% names(x)) x[[field]] <- NA_character_
  }
  x$file <- normalise_file_id(x$file, nametag)
  x <- x[!is.na(x$variable), c("file", fields), drop = FALSE]
  x$variable <- trimws(x$variable)
  list(data = x, found_fields = intersect(unique(canon), fields))
}

read_dictionary <- function(file, nametag, schema) {
  extension <- tolower(tools::file_ext(file))
  out <- list(path = file, format = extension, data = NULL,
              missing_fields = character(), note = NULL)
  if (extension == "csv") {
    raw <- tryCatch(
      fread(file, colClasses = "character", na.strings = "NA",
            data.table = FALSE, check.names = FALSE, showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(raw)) {
      out$note <- "The dictionary CSV could not be read."
      return(out)
    }
    if (!any(canonical_field(names(raw)) %in% "file")) {
      out$note <- paste("A CSV dictionary needs a `file` column naming the",
                        "data file of each variable.")
      return(out)
    }
    norm <- normalise_dictionary(raw, nametag, schema)
  } else {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      out$note <- "Package {.pkg readxl} is not installed; dictionary skipped."
      return(out)
    }
    sheets <- readxl::excel_sheets(file)
    pieces <- lapply(sheets, function(sheet) {
      d <- as.data.frame(
        readxl::read_excel(file, sheet = sheet, col_types = "text"),
        stringsAsFactors = FALSE
      )
      d$file <- sheet
      d
    })
    # keep canonical field detection per sheet, then bind
    normed <- lapply(pieces, normalise_dictionary, nametag = nametag,
                     schema = schema, from_sheets = TRUE)
    found <- unique(unlist(lapply(normed, `[[`, "found_fields")))
    data <- do.call(rbind, lapply(normed, `[[`, "data"))
    norm <- list(data = data, found_fields = found)
  }
  out$data <- norm$data
  out$missing_fields <- setdiff(schema$dictionary$fields, norm$found_fields)
  out
}

read_metadata <- function(file) {
  tryCatch(
    {
      parsed <- yaml::read_yaml(file)
      if (!is.list(parsed)) {
        stop("the file does not contain key: value pairs", call. = FALSE)
      }
      parsed
    },
    error = function(e) structure(list(), parse_error = conditionMessage(e))
  )
}

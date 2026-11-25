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
#'   information (`file_info`) and the dictionary when present.
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
  # Office lock files (`~$name.xlsx`) exist only while the file is open
  all_files <- all_files[!startsWith(all_files, "~$")]

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
  dictionary_misnamed <- FALSE
  if (is.null(dictionary_file)) {
    dictionary_file <- find_dictionary_near_miss(all_files, tag, schema)
    dictionary_misnamed <- !is.null(dictionary_file)
  }
  other <- setdiff(all_files, c(recognised, misnamed, dictionary_file))
  csv_other <- other[grepl("\\.csv$", other, ignore.case = TRUE)]
  restricted <- csv_other[restricted_file_name(csv_other, schema)]

  dictionary <- if (!is.null(dictionary_file)) {
    read_dictionary(file.path(path, dictionary_file), tag, schema)
  }

  ctx <- structure(
    list(
      schema = schema, source = "folder", path = path, nametag = tag,
      nametag_candidates = names(inferred), tables = tables,
      file_names = file_names, file_info = file_info,
      all_files = all_files, misnamed = misnamed,
      unrecognised = setdiff(csv_other, restricted),
      restricted = restricted, dictionary = dictionary,
      dictionary_file = dictionary_file,
      dictionary_misnamed = dictionary_misnamed,
      cache = new.env(parent = emptyenv())
    ),
    class = "socrates_data"
  )
  apply_aliases(ctx)
}

#' @export
print.socrates_data <- function(x, ...) {
  cli::cli_text("{.cls socrates_data} for NAMETAG {.val {x$nametag}}")
  cli::cli_text("Tables read: {.val {names(x$tables)}}")
  invisible(x)
}

# Accepted legacy column names (schema `aliases`, e.g. `cont_id` for `cnt_id`):
# rename them to the canonical name so that all rules work on one spelling,
# and remember the spelling used (`ctx$renamed`, for R40 and for messages).
# Extra files are only renamed for their key column.
apply_aliases <- function(ctx) {
  schema <- ctx$schema
  renamed <- list()
  rename_in <- function(tab, specs) {
    used <- character()
    for (spec in specs) {
      alias <- intersect(unlist(spec$aliases), names(tab))
      if (!spec$name %in% names(tab) && length(alias) == 1) {
        names(tab)[names(tab) == alias] <- spec$name
        used[[spec$name]] <- alias
      }
    }
    list(table = tab, used = used)
  }
  for (id in names(ctx$tables)) {
    kind <- schema$files[[id]]$kind
    specs <- if (identical(kind, "extra")) {
      common <- schema$files[[id]]$common
      Filter(function(col) identical(col$name, schema_key(schema, id)),
             schema_columns(schema, common))
    } else {
      schema_columns(schema, id)
    }
    out <- rename_in(ctx$tables[[id]], specs)
    ctx$tables[[id]] <- out$table
    if (length(out$used) > 0) renamed[[id]] <- out$used
  }
  # the dictionary may document either spelling
  if (!is.null(ctx$dictionary$data)) {
    data <- ctx$dictionary$data
    for (id in names(renamed)) {
      for (canonical in names(renamed[[id]])) {
        hit <- data$file == id & data$variable == renamed[[id]][[canonical]] &
          !(data$file == id & data$variable == canonical)
        data$variable[hit] <- canonical
      }
    }
    ctx$dictionary$data <- data
  }
  ctx$renamed <- renamed
  ctx
}

known_suffixes <- function(schema) {
  main <- purrr::map_chr(schema$files, "suffix")
  restricted <- purrr::map_chr(schema$restricted_files, "suffix")
  dict <- paste0(schema$dictionary$basename,
                 ".", c(unlist(schema$options$dictionary_extensions),
                        unlist(schema$options$dictionary_local_extensions)))
  unname(c(main, restricted, dict))
}

# Prefixes (candidate NAMETAGs) of files that end in a known suffix, most
# frequent first
infer_nametags <- function(files, suffixes) {
  prefixes_of <- function(suffix) {
    pattern <- paste0("^(.+)_", gsub(".", "\\.", suffix, fixed = TRUE), "$")
    hit <- files[grepl(pattern, files)]
    sub(pattern, "\\1", hit)
  }
  prefixes <- unlist(lapply(suffixes, prefixes_of))
  # `NAMETAG_data_dictionary.xlsx` would give the prefix `NAMETAG_data`: a
  # prefix that only a dictionary file suggests is not a second NAMETAG
  dict <- grepl("dictionary", suffixes, fixed = TRUE)
  from_dict <- unlist(lapply(suffixes[dict], prefixes_of))
  from_main <- unlist(lapply(suffixes[!dict], prefixes_of))
  if (length(from_main) > 0) {
    prefixes <- c(from_main, from_dict[from_dict %in% from_main])
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
                       c(unlist(schema$options$dictionary_extensions),
                         unlist(schema$options$dictionary_local_extensions)))
  hit <- candidates[candidates %in% files]
  if (length(hit) == 0) NULL else hit[1]
}

# A dictionary that starts with the NAMETAG but is named differently
# (`NAMETAG_data_dictionary.xlsx`, `NAMETAG_DICTIONARY.xlsx`): still read, so
# its content is checked, while R01 reports the name.
find_dictionary_near_miss <- function(files, tag, schema) {
  if (is.na(tag)) {
    return(NULL)
  }
  extensions <- c(unlist(schema$options$dictionary_extensions),
                  unlist(schema$options$dictionary_local_extensions))
  ext_ok <- tolower(tools::file_ext(files)) %in% extensions
  name_ok <- grepl(schema$dictionary$basename, files, ignore.case = TRUE) &
    startsWith(tolower(files), tolower(paste0(tag, "_")))
  hit <- files[ext_ok & name_ok]
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

# Dictionary ------------------------------------------------------

dictionary_field_aliases <- list(
  file = c("file", "table", "sheet"),
  variable = c("variable", "name", "column", "var", "variable name",
               "column name", "data repository name"),
  description = c("description", "label", "definition", "short description",
                  "variable label"),
  type = c("type", "data type", "datatype", "variable type"),
  values = c("values", "allowed values", "allowed values and meaning",
             "coding", "codes", "categories", "allowed value", "value labels",
             "allowed values coding", "allowed values or coding")
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
normalise_dictionary <- function(x, nametag, schema) {
  fields <- schema$dictionary$fields
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  canon <- canonical_field(names(x))
  keep <- !is.na(canon) & !duplicated(canon)
  x <- x[, keep, drop = FALSE]
  names(x) <- canon[keep]
  for (field in c("file", fields)) {
    if (!field %in% names(x)) x[[field]] <- NA_character_
  }
  x <- fold_continuation_rows(x)
  x$file <- normalise_file_id(x$file, nametag)
  x <- x[!is.na(x$variable), c("file", fields), drop = FALSE]
  x$variable <- trimws(x$variable)
  list(data = x, found_fields = intersect(unique(canon), fields))
}

# Spreadsheet dictionaries often list one value label per row, leaving the
# variable name empty on the continuation rows. Fold those into the `values`
# of the variable above (same file/sheet only).
fold_continuation_rows <- function(x) {
  keep <- rep(TRUE, nrow(x))
  last <- NA_integer_
  for (i in seq_len(nrow(x))) {
    if (!is.na(x$variable[i])) {
      last <- i
    } else if (!is.na(last) && !is.na(x$values[i]) &&
                 identical(x$file[i], x$file[last])) {
      x$values[last] <- paste(c(x$values[last], x$values[i])[
        !is.na(c(x$values[last], x$values[i]))], collapse = "\n")
      keep[i] <- FALSE
    }
  }
  x[keep, , drop = FALSE]
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
    norm <- tryCatch(read_excel_dictionary(file, nametag, schema),
                     error = function(e) {
                       out$note <<- paste0("The dictionary could not be read: ",
                                           conditionMessage(e))
                       NULL
                     })
    if (is.null(norm)) {
      return(out)
    }
  }
  out$data <- norm$data
  out$layout <- norm$layout
  out$missing_fields <- setdiff(schema$dictionary$fields, norm$found_fields)
  out
}

# A dictionary sheet may start with a title (the file name) and a line on the
# keys before the header row (`Variable | Description | Type ...`). Use the
# first of the top rows that has a variable column and at least one more
# dictionary field as the header; otherwise the first row, as in a plain table.
dictionary_sheet_table <- function(raw, max_rows = 20) {
  if (nrow(raw) == 0 || ncol(raw) == 0) {
    return(raw)
  }
  header_at <- 1L
  for (i in seq_len(min(max_rows, nrow(raw)))) {
    fields <- canonical_field(vapply(raw[i, ], function(x) {
      if (is.na(x)) "" else as.character(x)
    }, character(1)))
    if ("variable" %in% fields && sum(!is.na(unique(fields))) >= 2) {
      header_at <- i
      break
    }
  }
  header <- vapply(raw[header_at, ], function(x) {
    if (is.na(x)) "" else trimws(as.character(x))
  }, character(1))
  empty <- header == ""
  header[empty] <- paste0("...", which(empty))
  out <- raw[-seq_len(header_at), , drop = FALSE]
  names(out) <- header
  rownames(out) <- NULL
  attr(out, "header_row") <- header_at
  out
}

# Names of the four dictionary columns of the guideline (the header of a
# sheet should be exactly these, in row 1)
dictionary_header_names <- function(schema) {
  as.character(unlist(schema$dictionary$fields))
}

# Layout problems of one sheet: header not in row 1, or column names other
# than the guideline names. NULL when the sheet follows the guideline.
dictionary_layout <- function(sheet_table, nametag, sheet, schema) {
  header <- names(sheet_table)
  header <- header[!startsWith(header, "...")]
  wanted <- dictionary_header_names(schema)
  row <- attr(sheet_table, "header_row") %||% 1L
  canon <- canonical_field(header)
  recognised <- header[!is.na(canon) & canon %in% wanted]
  # letter case alone is not a deviation (`Variable` for `variable`)
  odd <- recognised[tolower(trimws(recognised)) !=
                      canon[!is.na(canon) & canon %in% wanted]]
  if (row == 1L && length(odd) == 0) {
    return(NULL)
  }
  list(file = normalise_file_id(sheet, nametag), header_row = row,
       names = odd)
}

read_excel_dictionary <- function(file, nametag, schema) {
  sheets <- readxl::excel_sheets(file)
  layout <- list()
  pieces <- lapply(sheets, function(sheet) {
    raw <- as.data.frame(
      suppressMessages(
        readxl::read_excel(file, sheet = sheet, col_types = "text",
                           col_names = FALSE)
      ),
      stringsAsFactors = FALSE
    )
    d <- dictionary_sheet_table(raw)
    layout <<- c(layout, list(dictionary_layout(d, nametag, sheet, schema)))
    d$file <- sheet
    d
  })
  normed <- lapply(pieces, normalise_dictionary, nametag = nametag,
                   schema = schema)
  found <- unique(unlist(lapply(normed, `[[`, "found_fields")))
  list(data = do.call(rbind, lapply(normed, `[[`, "data")),
       found_fields = found, layout = Filter(Negate(is.null), layout))
}

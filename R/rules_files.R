# Rules about files and file format: R01-R06 ----------------------------------

rule_r01 <- function(ctx) {
  schema <- ctx$schema
  out <- list()
  if (identical(ctx$source, "memory")) {
    for (id in names(schema$files)) {
      spec <- schema$files[[id]]
      if (isTRUE(spec$required) && !has_table(ctx, id)) {
        out[[length(out) + 1]] <- finding(
          "R01", file = id,
          message = paste0("Required table `", id, "` was not supplied.")
        )
      }
    }
    return(bind_findings(out))
  }

  tag <- ctx$nametag
  if (is.na(tag)) {
    return(finding(
      "R01",
      message = paste0(
        "No file name ends with a known suffix (e.g. `_participant_common.csv`, ",
        "`_contact_common.csv`, `_hh_common.csv`, `_sday.csv`), so the NAMETAG ",
        "cannot be determined and no further file checks are possible. ",
        "Files found: ", found_files_label(ctx), ". Rename the files as ",
        "`NAMETAG_<name>.csv` (all data files must be `.csv`), or pass ",
        "`nametag =` to see which files are missing."
      )
    ))
  }
  if (length(ctx$nametag_candidates) > 1) {
    out[[length(out) + 1]] <- finding(
      "R01",
      message = paste0(
        "Files carry more than one NAMETAG prefix: ",
        collapse_names(ctx$nametag_candidates), "; using `", tag, "`."
      )
    )
  }
  duplicates <- duplicate_common_files(ctx)
  for (type in names(duplicates)) {
    out[[length(out) + 1]] <- finding(
      "R01", variant = "duplicate_common",
      file = paste(duplicates[[type]], collapse = ", "),
      n_fail = length(duplicates[[type]]),
      message = paste0(
        "More than one ", type, " common file. `socialmixr::load_survey()` ",
        "stacks every file whose name matches `_", type, "s?_common*.csv` into ",
        "one table, so a second file adds duplicated or mixed rows. Keep ",
        "exactly one `", tag, "_", type, "_common.csv` per record (merge the ",
        "files, or move the extra ones out of the record)."
      )
    )
  }
  for (id in names(schema$files)) {
    spec <- schema$files[[id]]
    expected <- paste0(tag, "_", spec$suffix)
    if (isTRUE(spec$required) && !id %in% names(ctx$file_names)) {
      out[[length(out) + 1]] <- finding(
        "R01", file = expected,
        message = paste0("Required file `", expected, "` is missing.")
      )
    }
  }
  if (isTRUE(ctx$dictionary_misnamed)) {
    out[[length(out) + 1]] <- finding(
      "R01", file = ctx$dictionary_file,
      message = paste0(
        "The dictionary file does not follow the naming `", tag, "_",
        schema$dictionary$basename, ".<ext>`; rename it. Its content was ",
        "checked anyway (R14, R15)."
      )
    )
  }
  if (isTRUE(schema$dictionary$required) && is.null(ctx$dictionary_file)) {
    wanted <- paste0(tag, "_", schema$dictionary$basename, ".",
                     schema$options$dictionary_extensions, collapse = " or ")
    similar <- ctx$all_files[grepl(schema$dictionary$basename,
                                   ctx$all_files, ignore.case = TRUE)]
    hint <- if (length(similar) > 0) {
      paste0(" Found ", collapse_names(similar), ", which does not follow the ",
             "naming (`NAMETAG_dictionary.<ext>`) or format (",
             paste0("`.", unlist(schema$options$dictionary_extensions), "`",
                    collapse = ", "), ") and is ignored.")
    } else {
      ""
    }
    out[[length(out) + 1]] <- finding(
      "R01", file = wanted,
      message = paste0("Required dictionary file is missing (", wanted, ").",
                       hint)
    )
  }
  for (file in setdiff(ctx$misnamed, unlist(duplicates))) {
    out[[length(out) + 1]] <- finding(
      "R01", file = file,
      message = paste0("File name does not start with the NAMETAG `", tag,
                       "`.")
    )
  }
  dict <- ctx$dictionary_file
  if (!is.null(dict)) {
    ext <- tolower(tools::file_ext(dict))
    if (!ext %in% unlist(schema$options$dictionary_extensions)) {
      out[[length(out) + 1]] <- finding(
        "R01", file = dict,
        message = paste0(
          "The dictionary is a `.", ext, "` file. The dictionary in the ",
          "published record must be a spreadsheet (",
          paste0("`.", unlist(schema$options$dictionary_extensions), "`",
                 collapse = " or "),
          "). A `.csv` dictionary is read here so that the other checks can ",
          "run, but it must not be part of the record."
        )
      )
    }
  }
  for (file in setdiff(ctx$unrecognised, unlist(duplicates))) {
    if (!startsWith(file, paste0(tag, "_"))) {
      out[[length(out) + 1]] <- finding(
        "R01", file = file,
        message = paste0(
          "File name does not start with the NAMETAG `", tag, "` (it seems to ",
          "belong to another record) and the file is not part of the ",
          "guideline file set."
        )
      )
      next
    }
    out[[length(out) + 1]] <- finding(
      "R01", variant = "unrecognised", file = file,
      message = paste0(
        "CSV file is not part of the guideline file set. socialmixr merges ",
        "every CSV that shares a column name with the participant or contact ",
        "table, so remove it from the submission or rename it as an extra ",
        "file (`", tag, "_<entity>_extra.csv`)."
      )
    )
  }
  bind_findings(out)
}

# CSV files beyond the expected one that socialmixr would treat as a
# participant or contact common file (it matches `_participants?_common.*.csv`
# and `_contacts?_common.*.csv` and row-binds all hits). Named by type.
duplicate_common_files <- function(ctx) {
  out <- list()
  for (type in c("participant", "contact")) {
    expected <- ctx$file_names[[paste0(type, "_common")]]
    pattern <- paste0("_", type, "s?_common.*\\.csv$")
    hit <- ctx$all_files[grepl(pattern, ctx$all_files)]
    present <- unique(c(expected, hit))
    if (length(present) > 1) out[[type]] <- present
  }
  out
}

# Short description of the files in the folder, for messages
found_files_label <- function(ctx, max_n = 6) {
  files <- ctx$all_files
  if (length(files) == 0) {
    return("none")
  }
  shown <- paste0("`", utils::head(files, max_n), "`", collapse = ", ")
  if (length(files) > max_n) {
    shown <- paste0(shown, ", ... (", length(files), " in total)")
  }
  not_csv <- files[!grepl("\\.csv$", files, ignore.case = TRUE)]
  if (length(not_csv) > 0) {
    shown <- paste0(shown, "; ", length(not_csv),
                    " file(s) are not `.csv` and are ignored")
  }
  shown
}

rule_r02 <- function(ctx) {
  tag <- ctx$nametag
  if (is.na(tag)) {
    return(NULL)
  }
  nt <- ctx$schema$nametag
  if (!is.null(nt$valid_pattern) && !matches(tag, nt$valid_pattern)) {
    return(finding(
      "R02",
      message = paste0(
        "NAMETAG `", tag, "` cannot be used as a file name prefix: use ASCII ",
        "letters and digits, separated by underscores or hyphens (pattern `",
        nt$valid_pattern, "`)."
      )
    ))
  }
  if (matches(tag, nt$pattern)) {
    return(NULL)
  }
  legacy <- nt$legacy_pattern
  if (!is.null(legacy) && matches(tag, legacy)) {
    return(finding(
      "R02", variant = "legacy_prefix",
      message = paste0(
        "NAMETAG `", tag, "` starts with the year. Accepted for records ",
        "published before the recommendation; new submissions use e.g. `",
        nt$example, "`."
      )
    ))
  }
  finding(
    "R02", variant = "not_recommended",
    message = paste0(
      "NAMETAG `", tag, "` does not follow the recommended form ",
      "NAME_YEAR[_SUFFIX] in upper case (pattern `", nt$pattern,
      "`), e.g. `", nt$example, "`. The name is accepted as long as every ",
      "file carries the same prefix."
    )
  )
}

rule_r04 <- function(ctx) {
  if (identical(ctx$source, "memory")) {
    return(NULL)
  }
  out <- list()
  rownames_ids <- ctx$schema$options$rowname_column_names
  for (id in names(ctx$file_info)) {
    info <- ctx$file_info[[id]]
    file <- file_label(ctx, id)
    if (!is.null(info$parse_error)) {
      out[[length(out) + 1]] <- finding(
        "R04", file = file,
        message = paste0("The file cannot be read as CSV: ", info$parse_error)
      )
      next
    }
    problems <- format_problems(info, ctx, id, rownames_ids)
    out <- c(out, lapply(problems, function(message) {
      finding("R04", file = file, message = message)
    }))
    for (column in names(info$blank)) {
      rows <- info$blank[[column]]
      out[[length(out) + 1]] <- finding(
        "R04", file = file, column = column, n_fail = length(rows),
        n_total = info$n_rows, rows = rows,
        message = paste0(
          "Empty or whitespace-only cells; write `NA` for missing values."
        )
      )
    }
  }
  bind_findings(out)
}

# File-level format problems of one file, as messages
format_problems <- function(info, ctx, id, rownames_ids) {
  problems <- character()
  if (length(info$parse_warnings) > 0) {
    problems <- c(problems, paste0(
      "Problem while reading the file (inconsistent number of fields or ",
      "quoting?): ", info$parse_warnings[1]
    ))
  }
  if (!info$utf8) {
    problems <- c(problems,
                  "The file is not valid UTF-8 (probably Latin-1). Save it as UTF-8.")
  }
  if (info$bom) {
    problems <- c(problems,
                  "The file starts with a byte-order mark (BOM); save without BOM.")
  }
  if (!identical(info$delimiter, ",")) {
    problems <- c(problems, paste0(
      "The delimiter is not a comma (found `",
      gsub("\t", "\\t", info$delimiter, fixed = TRUE), "`)."
    ))
  }
  header <- info$header
  if (anyDuplicated(header) > 0) {
    problems <- c(problems, paste0(
      "Duplicated column names: ",
      collapse_names(unique(header[duplicated(header)]))
    ))
  }
  own_columns <- schema_column_names(ctx$schema, id)
  if (length(header) > 0 && header[1] %in% rownames_ids &&
        !header[1] %in% own_columns) {
    problems <- c(problems, paste0(
      "The first column `", header[1],
      "` looks like exported row names or an index column."
    ))
  }
  if (info$n_rows == 0 && identical(ctx$schema$files[[id]]$kind, "common")) {
    problems <- c(problems, "The file has a header but no data rows.")
  }
  problems
}

rule_r05 <- function(ctx) {
  schema <- ctx$schema
  out <- list()
  for (id in schema_ids(schema, "common")) {
    if (!has_table(ctx, id)) next
    all_columns <- schema_column_names(schema, id)
    required <- schema_column_names(schema, id, required_only = TRUE)
    actual <- names(ctx$tables[[id]])
    file <- file_label(ctx, id)
    missing <- setdiff(required, actual)
    extra <- setdiff(actual, all_columns)
    if (length(missing) > 0) {
      hints <- vapply(missing, function(col) {
        spec <- schema_column(schema, id, col)
        found <- intersect(unlist(spec$common_mistakes), actual)
        if (length(found) == 0) found <- similar_name(col, extra)
        if (length(found) == 0) "" else paste0(" (found `", found[1], "`)")
      }, character(1))
      out[[length(out) + 1]] <- finding(
        "R05", file = file, n_fail = length(missing),
        n_total = length(required),
        message = paste0(
          "Missing required column(s): ",
          paste0("`", missing, "`", hints, collapse = ", "),
          ". Columns that were not collected must still be present, filled ",
          "with `NA`."
        )
      )
    }
    out <- c(out, extra_column_findings(ctx, id, extra, file))
    for (canonical in names(ctx$renamed[[id]])) {
      out[[length(out) + 1]] <- finding(
        "R05", variant = "alias", file = file, column = canonical,
        message = paste0("`", ctx$renamed[[id]][[canonical]], "` is the ",
                         "legacy spelling of `", canonical, "`; accepted, ",
                         "but use one spelling in all files of the record.")
      )
    }
    ordered <- identical(schema$options$column_order, "strict")
    present_expected <- intersect(all_columns, actual)
    if (ordered && length(extra) == 0 &&
          !identical(present_expected, actual)) {
      out[[length(out) + 1]] <- finding(
        "R05", variant = "order", file = file,
        message = paste0("Columns are not in the specified order: ",
                         paste0("`", present_expected, "`", collapse = ", "),
                         ".")
      )
    }
  }
  bind_findings(out)
}

# Columns of a common file that are not in its specification, sorted by what
# the fix is: a retired column (remove), a misspelling of a specified column
# (rename), a column that belongs to another table, e.g. `sday_id` (remove it
# here, make sure it is there), or a survey-specific variable (extra file).
extra_column_findings <- function(ctx, id, extra, file) {
  if (length(extra) == 0) {
    return(list())
  }
  schema <- ctx$schema
  own <- schema_columns(schema, id)
  retired <- schema$files[[id]]$retired_columns
  other_ids <- setdiff(schema_ids(schema), id)
  extra_id <- schema$files[[id]]$extra_file %||%
    names(Filter(function(f) {
      identical(f$kind, "extra") && identical(f$common, id)
    }, schema$files))[1]
  n_total <- length(own)
  out <- list()
  add <- function(variant, columns, message) {
    if (length(columns) == 0) {
      return(invisible())
    }
    out[[length(out) + 1]] <<- finding(
      "R05", variant = variant, file = file, n_fail = length(columns),
      n_total = n_total, message = message
    )
  }

  is_retired <- extra %in% vapply(retired, function(r) r$name, character(1))
  for (column in extra[is_retired]) {
    note <- Filter(function(r) identical(r$name, column), retired)[[1]]$note
    add("retired", column, paste0("`", column, "` is no longer part of the ",
                                  "format: ", note, "."))
  }
  rest <- extra[!is_retired]

  target <- vapply(rest, function(column) {
    hit <- Filter(function(col) column %in% unlist(col$common_mistakes), own)
    if (length(hit) == 0) NA_character_ else hit[[1]]$name
  }, character(1))
  for (column in rest[!is.na(target)]) {
    canonical <- target[[column]]
    add("misspelled", column, paste0(
      "`", column, "` is not a specified column; it looks like `", canonical,
      "`. Rename it, or remove it if `", canonical, "` is present as well."
    ))
  }
  rest <- rest[is.na(target)]

  home <- vapply(rest, function(column) {
    ids <- Filter(function(other) {
      column %in% schema_column_names(schema, other)
    }, other_ids)
    if (length(ids) == 0) NA_character_ else ids[[1]]
  }, character(1))
  for (home_id in unique(home[!is.na(home)])) {
    columns <- rest[!is.na(home) & home == home_id]
    key <- schema_key(schema, home_id)
    add("misplaced", columns, paste0(
      "Column(s) ", collapse_names(columns), " belong(s) to the `", home_id,
      "` file (", file_label(ctx, home_id), "), which is linked to this file by `",
      key, "`. Remove them here and keep them in that file; a column that ",
      "occurs in two tables is merged twice by socialmixr."
    ))
  }
  rest <- rest[is.na(home)]

  where <- if (is.na(extra_id)) {
    "Remove columns that are not needed."
  } else {
    paste0("Survey-specific variables belong in the `", extra_id,
           "` file, linked by the key `", schema_key(schema, id),
           "`; remove columns that are not needed.")
  }
  add("extra", rest, paste0(
    "Column(s) not in the common file specification: ", collapse_names(rest),
    ". ", where
  ))
  out
}

# Names in `candidates` that look like a misspelling of `name`: equal after
# dropping case, spaces and punctuation (`Part_ID`, `cnt_ age_exact`), or one
# character apart for names of six or more characters (`cnt_gendr`).
similar_name <- function(name, candidates) {
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  if (length(candidates) == 0) {
    return(character())
  }
  same <- norm(candidates) == norm(name)
  close <- nchar(norm(name)) >= 6 &
    as.vector(utils::adist(norm(name), norm(candidates))) == 1
  candidates[same | close]
}

rule_r06 <- function(ctx) {
  schema <- ctx$schema
  out <- list()
  for (id in schema_ids(schema, "extra")) {
    if (!has_table(ctx, id)) next
    common_id <- schema$files[[id]]$common
    key <- schema_key(schema, id)
    extra <- ctx$tables[[id]]
    file <- file_label(ctx, id)
    if (!key %in% names(extra)) {
      out[[length(out) + 1]] <- finding(
        "R06", file = file, column = key,
        message = paste0("The extra file has no key column `", key, "`.")
      )
      next
    }
    if (has_table(ctx, common_id) && key %in% names(ctx$tables[[common_id]])) {
      common_keys <- ctx$tables[[common_id]][[key]]
      extra_keys <- extra[[key]]
      only_extra <- which(!extra_keys %in% common_keys)
      if (length(only_extra) > 0) {
        out[[length(out) + 1]] <- finding(
          "R06", file = file, column = key, n_fail = length(only_extra),
          n_total = length(extra_keys), rows = only_extra,
          message = paste0("`", key, "` values not present in the common ",
                           "file (", file_label(ctx, common_id), ").")
        )
      }
      only_common <- sum(!common_keys %in% extra_keys)
      if (only_common > 0) {
        out[[length(out) + 1]] <- finding(
          "R06", file = file, column = key, n_fail = only_common,
          n_total = length(common_keys),
          message = paste0(
            "Records of the common file have no row in the extra file; the ",
            "files must match one-to-one."
          )
        )
      }
      duplicated_cols <- setdiff(intersect(names(extra),
                                           names(ctx$tables[[common_id]])), key)
      if (length(duplicated_cols) > 0) {
        out[[length(out) + 1]] <- finding(
          "R06", file = file, n_fail = length(duplicated_cols),
          message = paste0(
            "Column(s) duplicated from the common file: ",
            collapse_names(duplicated_cols),
            ". The extra file may only hold other variables (socialmixr ",
            "merges on every shared column name)."
          )
        )
      }
    }
    others <- setdiff(names(ctx$tables), c(id, common_id))
    shared_with <- list()
    for (other in others) {
      shared <- setdiff(intersect(names(extra), names(ctx$tables[[other]])),
                        key)
      if (length(shared) > 0) {
        shared_with[[other]] <- shared
      }
    }
    if (length(shared_with) > 0) {
      columns <- unique(unlist(shared_with))
      labels <- vapply(names(shared_with), function(o) file_label(ctx, o),
                       character(1))
      tables <- paste(labels, collapse = ", ")
      out[[length(out) + 1]] <- finding(
        "R06", variant = "shared", file = file, n_fail = length(columns),
        message = paste0(
          "Column(s) ", collapse_names(columns), " also occur in ", tables,
          "; socialmixr merges files on every shared column name."
        )
      )
    }
  }
  bind_findings(out)
}

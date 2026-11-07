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
    out[[1]] <- finding(
      "R01",
      message = paste0(
        "No file name ends with a known suffix (e.g. `_participant_common.csv`), ",
        "so the NAMETAG cannot be determined."
      )
    )
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
  if (isTRUE(schema$dictionary$required) && is.null(ctx$dictionary_file)) {
    wanted <- paste0(tag, "_", schema$dictionary$basename, ".",
                     schema$options$dictionary_extensions, collapse = " or ")
    out[[length(out) + 1]] <- finding(
      "R01", file = wanted,
      message = paste0("Required dictionary file is missing (", wanted, ").")
    )
  }
  for (file in ctx$misnamed) {
    out[[length(out) + 1]] <- finding(
      "R01", file = file,
      message = paste0("File name does not start with the NAMETAG `", tag,
                       "`.")
    )
  }
  for (file in ctx$unrecognised) {
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

rule_r02 <- function(ctx) {
  tag <- ctx$nametag
  if (is.na(tag)) {
    return(NULL)
  }
  pattern <- ctx$schema$nametag$pattern
  if (!matches(tag, pattern)) {
    return(finding(
      "R02",
      message = paste0(
        "NAMETAG `", tag, "` does not match NAME_YEAR_COUNTRY[_SUFFIX] in ",
        "ASCII upper case (pattern `", pattern, "`), e.g. `",
        ctx$schema$nametag$example, "`."
      )
    ))
  }
  NULL
}

rule_r03 <- function(ctx) {
  item <- Filter(function(i) identical(i$id, "R03"), ctx$schema$manual_items)
  tag <- if (is.na(ctx$nametag)) "(unknown)" else ctx$nametag
  finding("R03", message = paste0("[", tag, "] ", item[[1]]$prompt))
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
        found <- intersect(unlist(spec$aliases), actual)
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
    if (length(extra) > 0) {
      out[[length(out) + 1]] <- finding(
        "R05", variant = "extra", file = file, n_fail = length(extra),
        n_total = length(all_columns),
        message = paste0(
          "Column(s) not in the common file specification: ",
          collapse_names(extra),
          ". Move survey-specific variables to the extra file."
        )
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
    for (other in others) {
      shared <- setdiff(intersect(names(extra), names(ctx$tables[[other]])),
                        key)
      if (length(shared) > 0) {
        out[[length(out) + 1]] <- finding(
          "R06", variant = "shared", file = file, n_fail = length(shared),
          message = paste0(
            "Column(s) ", collapse_names(shared), " also occur in ",
            file_label(ctx, other), "; socialmixr merges files on every ",
            "shared column name."
          )
        )
      }
    }
  }
  bind_findings(out)
}

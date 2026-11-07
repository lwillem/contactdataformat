# Record hygiene and identifier consistency: R34, R38, R39 -----------------------

# Columns that appear in any table that socialmixr treats as a main table or
# merges into one
main_table_columns <- function(ctx) {
  unique(c(unlist(lapply(ctx$tables, names)),
           unlist(ctx$renamed, use.names = FALSE)))
}

rule_r34 <- function(ctx) {
  if (identical(ctx$source, "memory")) {
    return(NULL)
  }
  hygiene <- ctx$schema$record_hygiene
  out <- list()
  main_columns <- main_table_columns(ctx)
  others <- c(ctx$unrecognised, ctx$restricted)
  headers <- list()
  for (file in others) {
    header <- read_header(file.path(ctx$path, file))
    headers[[file]] <- header
    shared <- intersect(header, main_columns)
    if (length(shared) > 0) {
      out[[length(out) + 1]] <- finding(
        "R34", file = file,
        message = paste0(
          "CSV outside the guideline file set shares column(s) ",
          collapse_names(shared), " with the survey tables, so ",
          "`socialmixr::load_survey()` merges it into them. Remove it from ",
          "the record or turn it into a declared extra file."
        )
      )
    }
    if ("part_id" %in% header) {
      ids <- tryCatch(
        fread(file.path(ctx$path, file), select = "part_id",
              colClasses = "character", data.table = FALSE)$part_id,
        error = function(e) character()
      )
      repeated <- which(duplicated(ids) & !is.na(ids))
      if (length(repeated) > 0) {
        out[[length(out) + 1]] <- finding(
          "R34", file = file, column = "part_id", n_fail = length(repeated),
          n_total = length(ids), rows = repeated,
          message = paste0(
            "Repeated `part_id` in a file outside the main tables: merging ",
            "it would replicate participants."
          )
        )
      }
    }
  }
  keys <- vapply(headers, function(h) paste(sort(h), collapse = "|"),
                 character(1))
  same <- unique(keys[duplicated(keys)])
  for (key in same) {
    files <- names(keys)[keys == key]
    out[[length(out) + 1]] <- finding(
      "R34", file = paste(files, collapse = ", "),
      message = paste0(
        "Several CSV files outside the file set have identical columns; ",
        "socialmixr would row-bind them silently."
      )
    )
  }
  json <- setdiff(
    ctx$all_files[grepl("\\.json$", ctx$all_files, ignore.case = TRUE)],
    unlist(hygiene$allowed_other_files)
  )
  allowed_json <- sum(ctx$all_files %in% unlist(hygiene$allowed_other_files))
  if (length(json) + allowed_json > hygiene$max_json_files) {
    out[[length(out) + 1]] <- finding(
      "R34", file = paste(c(json, intersect(ctx$all_files,
                                            unlist(hygiene$allowed_other_files))),
                          collapse = ", "),
      message = paste0(
        "More than ", hygiene$max_json_files, " JSON file(s) in the record: ",
        "socialmixr takes the first `.json` as the citation reference. Use ",
        "YAML for the metadata file."
      )
    )
  }
  bind_findings(out)
}

# Storage class of an identifier column: "integer" or "string"
id_class <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  if (all(matches(x, "^-?[0-9]+$"))) "integer" else "string"
}

rule_r38 <- function(ctx) {
  out <- list()
  keys <- c("part_id", "hh_id", schema_key(ctx$schema, "contact_common"))
  for (key in keys) {
    classes <- list()
    for (id in names(ctx$tables)) {
      if (key %in% names(ctx$tables[[id]])) {
        classes[[id]] <- id_class(ctx$tables[[id]][[key]])
      }
    }
    classes <- Filter(Negate(is.na), classes)
    if (length(unique(unlist(classes))) > 1) {
      detail <- paste0(names(classes), " (", unlist(classes), ")",
                       collapse = ", ")
      out[[length(out) + 1]] <- finding(
        "R38", column = key,
        message = paste0("`", key, "` has a different type in different ",
                         "files: ", detail, ". Use one type everywhere ",
                         "(integer or string).")
      )
    }
  }
  bind_findings(out)
}

rule_r39 <- function(ctx) {
  weight <- get_column(ctx, "participant_extra", "diary_weight")
  if (is.null(weight)) {
    return(NULL)
  }
  file <- file_label(ctx, "participant_extra")
  out <- list()
  bad <- which(!is.na(weight) &
                 !matches(weight, ctx$schema$types$numeric$pattern))
  missing <- which(is.na(weight))
  if (length(bad) > 0) {
    out[[1]] <- finding(
      "R39", file = file, column = "diary_weight", n_fail = length(bad),
      n_total = length(weight), rows = bad,
      message = "`diary_weight` must be numeric."
    )
  }
  if (length(missing) > 0) {
    out[[length(out) + 1]] <- finding(
      "R39", file = file, column = "diary_weight", n_fail = length(missing),
      n_total = length(weight), rows = missing,
      message = paste0("`diary_weight` has missing values; weights without ",
                       "`NA` are required (or drop the column).")
    )
  }
  bind_findings(out)
}

# One spelling of a key per record: an extra file that spells the key
# differently from its common file shares only other columns with it, so
# socialmixr silently drops the extra columns.
rule_r40 <- function(ctx) {
  schema <- ctx$schema
  out <- list()
  spelling <- function(id, key) {
    ctx$renamed[[id]][[key]] %||% key
  }
  for (id in schema_ids(schema, "extra")) {
    common <- schema$files[[id]]$common
    key <- schema_key(schema, id)
    if (!has_table(ctx, id) || !has_table(ctx, common) ||
          !has_column(ctx, id, key) || !has_column(ctx, common, key)) {
      next
    }
    in_common <- spelling(common, key)
    in_extra <- spelling(id, key)
    if (!identical(in_common, in_extra)) {
      out[[length(out) + 1]] <- finding(
        "R40", file = file_label(ctx, id), column = key,
        message = paste0(
          "The key is spelled `", in_common, "` in ", file_label(ctx, common),
          " but `", in_extra, "` in ", file_label(ctx, id),
          ". Use one spelling per record, otherwise socialmixr cannot join ",
          "the two files and silently drops the extra columns."
        )
      )
    }
  }
  bind_findings(out)
}

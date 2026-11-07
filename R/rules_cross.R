# Cross-table rules: R15-R20 ----------------------------------------------------

dictionary_type_class <- function(x) {
  # first word only: "Integer (years)", "String of 2 chars", "floating point"
  x <- tolower(sub("^([A-Za-z]+).*$", "\\1", trimws(x)))
  if (x %in% c("integer", "int", "whole")) return("integer")
  if (x %in% c("numeric", "float", "floating", "double", "decimal",
               "number")) {
    return("numeric")
  }
  if (x %in% c("string", "character", "text", "char")) return("string")
  if (x %in% c("boolean", "logical", "bool")) return("boolean")
  if (x %in% c("date")) return("date")
  if (x %in% c("code", "categorical", "factor", "category")) return("code")
  if (x %in% c("id", "identifier")) return("id")
  NA_character_
}

# Dictionary types that are compatible with a schema type
compatible_types <- function(schema_type) {
  switch(
    schema_type,
    integer = c("integer"),
    numeric = c("numeric", "integer"),
    string = c("string", "code"),
    boolean = c("boolean", "integer"),  # integer: legacy 0/1 coding (R08 warns)
    id = c("id", "string", "integer", "code"),
    code = c("code", "string", "integer", "id"),
    date_yyyymmdd = c("string", "date", "integer", "code"),
    schema_type
  )
}

rule_r15 <- function(ctx) {
  schema <- ctx$schema
  dict <- ctx$dictionary
  if (is.null(dict)) {
    if (identical(ctx$source, "memory")) {
      return(finding("R15", variant = "skipped",
                     message = "No dictionary was supplied; rule not evaluated."))
    }
    return(NULL)
  }
  file <- basename(ctx$dictionary_file %||% "dictionary")
  if (!is.null(dict$note)) {
    return(finding("R15", variant = "skipped", file = file,
                   message = gsub("[{}]|\\.pkg ", "", dict$note)))
  }
  out <- list()
  if (length(dict$missing_fields) > 0) {
    out[[1]] <- finding(
      "R15", file = file,
      message = paste0(
        "The dictionary has no column for: ",
        paste(dict$missing_fields, collapse = ", "),
        ". Every variable needs name, description, type and allowed values."
      )
    )
  }
  data <- dict$data
  for (id in names(ctx$tables)) {
    columns <- names(ctx$tables[[id]])
    entries <- data[data$file == id, , drop = FALSE]
    if (nrow(entries) == 0) {
      out[[length(out) + 1]] <- finding(
        "R15", file = file, n_fail = length(columns), n_total = length(columns),
        message = paste0("The dictionary has no sheet/section for `", id,
                         "` (", file_label(ctx, id), ").")
      )
      next
    }
    undocumented <- setdiff(columns, entries$variable)
    if (length(undocumented) > 0) {
      out[[length(out) + 1]] <- finding(
        "R15", file = file, n_fail = length(undocumented),
        n_total = length(columns),
        message = paste0("Column(s) of `", id, "` missing from the ",
                         "dictionary: ", collapse_names(undocumented), ".")
      )
    }
    phantom <- setdiff(entries$variable, columns)
    if (length(phantom) > 0) {
      out[[length(out) + 1]] <- finding(
        "R15", variant = "phantom", file = file, n_fail = length(phantom),
        message = paste0("Dictionary describes variable(s) that are not in `",
                         id, "`: ", collapse_names(phantom), ".")
      )
    }
    for (spec in schema_columns(schema, id)) {
      entry <- entries[entries$variable == spec$name, , drop = FALSE]
      if (nrow(entry) == 0 || is.na(entry$type[1])) next
      class <- dictionary_type_class(entry$type[1])
      if (!is.na(class) && !class %in% compatible_types(spec$type)) {
        out[[length(out) + 1]] <- finding(
          "R15", variant = "type", file = file, column = spec$name,
          message = paste0("Dictionary type `", entry$type[1],
                           "` does not match the specified type `",
                           spec$type, "` of `", spec$name, "` in `", id, "`.")
        )
      }
    }
  }
  bind_findings(out)
}

rule_r16 <- function(ctx) {
  schema <- ctx$schema
  out <- list()
  check_unique <- function(id, column) {
    x <- get_column(ctx, id, column)
    if (is.null(x)) return(NULL)
    dup <- which(!is.na(x) & (duplicated(x) | duplicated(x, fromLast = TRUE)))
    if (length(dup) == 0) return(NULL)
    finding("R16", file = file_label(ctx, id), column = column,
            n_fail = length(dup), n_total = length(x), rows = dup,
            message = paste0("Duplicated ", "`", column, "` values (",
                             show_values(x[dup]), "); it must be unique."))
  }
  for (id in schema_ids(schema, "common")) {
    for (spec in schema_columns(schema, id)) {
      if (isTRUE(spec$unique)) {
        out[[length(out) + 1]] <- check_unique(id, spec$name)
      }
    }
  }
  for (id in schema_ids(schema, "extra")) {
    out[[length(out) + 1]] <- check_unique(id, schema_key(schema, id))
  }
  bind_findings(out)
}

rule_r17 <- function(ctx) {
  schema <- ctx$schema
  out <- list()
  for (id in names(schema$files)) {
    for (fk in schema$files[[id]]$foreign_keys) {
      target <- strsplit(fk$references, ".", fixed = TRUE)[[1]]
      x <- get_column(ctx, id, fk$column)
      y <- get_column(ctx, target[1], target[2])
      if (is.null(x) || is.null(y)) next
      orphan <- which(!is.na(x) & !x %in% y)
      if (length(orphan) > 0) {
        out[[length(out) + 1]] <- finding(
          "R17", file = file_label(ctx, id), column = fk$column,
          n_fail = length(orphan), n_total = length(x), rows = orphan,
          message = paste0(
            "`", fk$column, "` values without a matching `", target[2],
            "` in ", file_label(ctx, target[1]), ": ",
            show_values(x[orphan]), "."
          )
        )
      }
    }
  }
  bind_findings(out)
}

rule_r18 <- function(ctx) {
  out <- list()
  participants <- get_column(ctx, "participant_common", "part_id")
  if (is.null(participants)) {
    return(NULL)
  }
  sday <- get_column(ctx, "sday", "part_id")
  if (!is.null(sday)) {
    without <- which(!participants %in% sday)
    if (length(without) > 0) {
      out[[1]] <- finding(
        "R18", file = file_label(ctx, "participant_common"),
        column = "part_id", n_fail = length(without),
        n_total = length(participants), rows = without,
        message = "Participants without a row in the survey-day file."
      )
    }
  }
  contacts <- get_column(ctx, "contact_common", "part_id")
  if (!is.null(contacts)) {
    zero <- which(!participants %in% contacts)
    if (length(zero) > 0) {
      out[[length(out) + 1]] <- finding(
        "R18", variant = "zero_contacts",
        file = file_label(ctx, "participant_common"), column = "part_id",
        n_fail = length(zero), n_total = length(participants), rows = zero,
        message = paste0(
          length(zero), " participant(s) reported no contacts (kept in the ",
          "participant file, as required)."
        )
      )
    }
  }
  bind_findings(out)
}

rule_r19 <- function(ctx) {
  hh_id <- get_column(ctx, "hh_common", "hh_id")
  hh_size <- get_column(ctx, "hh_common", "hh_size")
  part_hh <- get_column(ctx, "participant_common", "hh_id")
  if (is.null(hh_id) || is.null(hh_size) || is.null(part_hh)) {
    return(NULL)
  }
  counts <- table(part_hh[!is.na(part_hh)])
  n_part <- as.integer(counts[hh_id])
  size <- to_number(hh_size)
  bad <- which(!is.na(n_part) & !is.na(size) & size < n_part)
  if (length(bad) == 0) {
    return(NULL)
  }
  finding(
    "R19", file = file_label(ctx, "hh_common"), column = "hh_size",
    n_fail = length(bad), n_total = length(hh_id), rows = bad,
    message = paste0(
      "`hh_size` is smaller than the number of participant records of the ",
      "household; correct `hh_size` or document the exception."
    )
  )
}

rule_r20 <- function(ctx) {
  panel <- get_column(ctx, "participant_extra", "panel_id")
  part <- get_column(ctx, "participant_extra", "part_id")
  sday_part <- get_column(ctx, "sday", "part_id")
  sday_id <- get_column(ctx, "sday", "sday_id")
  if (is.null(panel) || is.null(part) || is.null(sday_part) ||
        is.null(sday_id)) {
    return(NULL)
  }
  date <- sday_id[match(part, sday_part)]
  key <- paste(panel, date, sep = "|")
  dup <- which(!is.na(panel) & !is.na(date) &
                 (duplicated(key) | duplicated(key, fromLast = TRUE)))
  if (length(dup) == 0) {
    return(NULL)
  }
  finding(
    "R20", file = file_label(ctx, "participant_extra"), column = "panel_id",
    n_fail = length(dup), n_total = length(panel), rows = dup,
    message = paste0(
      "The same `panel_id` has several `part_id` records on the same ",
      "survey day; each survey occasion of an individual needs its own day."
    )
  )
}

# Cross-table rules: R15-R20, R41, R45 ----------------------------------------------------

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
    boolean = c("boolean", "integer"),  # integer: legacy 0/1 coding (R08 reports it)
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

# The panel_id column and its part_id: the participant extra file (guideline),
# or the survey-day file when a submission put it there
panel_columns <- function(ctx) {
  for (id in c("participant_extra", "sday")) {
    panel <- get_column(ctx, id, "panel_id")
    part <- get_column(ctx, id, "part_id")
    if (!is.null(panel) && !is.null(part)) {
      return(list(id = id, panel = panel, part = part))
    }
  }
  NULL
}

rule_r20 <- function(ctx) {
  out <- list()
  pc <- panel_columns(ctx)
  if (!is.null(pc)) {
    keep <- !is.na(pc$panel) & !is.na(pc$part)
    if (any(keep)) {
      records <- tapply(pc$part[keep], pc$panel[keep],
                        function(x) length(unique(x)))
      out[[1]] <- finding(
        "R20", variant = "summary", file = file_label(ctx, pc$id),
        column = "panel_id", n_fail = sum(records > 1),
        n_total = length(records),
        message = sprintf(
          paste0("panel_id: %d individual(s), %d with more than one ",
                 "part_id (records per individual: median %s, maximum %d)."),
          length(records), sum(records > 1),
          format(stats::median(records)), max(records)
        )
      )
    }
  }
  panel <- get_column(ctx, "participant_extra", "panel_id")
  part <- get_column(ctx, "participant_extra", "part_id")
  sday_part <- get_column(ctx, "sday", "part_id")
  sday_id <- get_column(ctx, "sday", "sday_id")
  if (is.null(panel) || is.null(part) || is.null(sday_part) ||
        is.null(sday_id)) {
    return(bind_findings(out))
  }
  date <- sday_id[match(part, sday_part)]
  key <- paste(panel, date, sep = "|")
  dup <- which(!is.na(panel) & !is.na(date) &
                 (duplicated(key) | duplicated(key, fromLast = TRUE)))
  if (length(dup) > 0) {
    out[[length(out) + 1]] <- finding(
      "R20", file = file_label(ctx, "participant_extra"), column = "panel_id",
      n_fail = length(dup), n_total = length(panel), rows = dup,
      message = paste0(
        "The same `panel_id` has several `part_id` records on the same ",
        "survey day; each survey occasion of an individual needs its own day."
      )
    )
  }
  bind_findings(out)
}

# R45: the records of one panel_id are one person ---------------------------

rule_r45 <- function(ctx) {
  pc <- panel_columns(ctx)
  participants <- ctx$tables[["participant_common"]]
  if (is.null(pc) || is.null(participants)) {
    return(NULL)
  }
  file <- file_label(ctx, pc$id)
  at <- match(pc$part, participants$part_id)
  out <- list()

  # gender does not change between records of one individual
  if ("part_gender" %in% names(participants)) {
    gender <- participants$part_gender[at]
    ok <- !is.na(pc$panel) & !is.na(gender)
    n_gender <- tapply(gender[ok], pc$panel[ok],
                       function(x) length(unique(x)))
    multi <- names(n_gender)[n_gender > 1]
    if (length(multi) > 0) {
      out[[1]] <- finding(
        "R45", file = file, column = "panel_id", n_fail = length(multi),
        n_total = length(n_gender), rows = which(pc$panel %in% multi),
        message = paste0(
          length(multi), " `panel_id` value(s) have more than one `part_gender` ",
          "over their records (e.g. ", show_values(multi, 3), "): one ",
          "`panel_id` must be one individual."
        )
      )
    }
  }

  # exact age follows the survey dates: no decrease, no jump
  sday_part <- get_column(ctx, "sday", "part_id")
  sday_id <- get_column(ctx, "sday", "sday_id")
  if ("part_age_exact" %in% names(participants) && !is.null(sday_part) &&
        !is.null(sday_id)) {
    df <- data.frame(
      row = seq_along(pc$panel), panel = pc$panel,
      age = to_number(participants$part_age_exact[at]),
      date = as.Date(sday_id[match(pc$part, sday_part)], format = "%Y%m%d"),
      stringsAsFactors = FALSE
    )
    df <- df[!is.na(df$panel) & !is.na(df$age) & !is.na(df$date), ]
    df <- df[order(df$panel, df$date), ]
    if (nrow(df) > 1) {
      n <- nrow(df)
      same <- df$panel[-1] == df$panel[-n]
      change <- diff(df$age)
      years <- as.numeric(diff(df$date)) / 365.25
      bad <- same & (change < floor(years) | change > ceiling(years))
      if (any(bad)) {
        out[[length(out) + 1]] <- finding(
          "R45", variant = "age", file = file, column = "panel_id",
          n_fail = length(unique(df$panel[-1][bad])),
          n_total = length(unique(df$panel)), rows = df$row[-1][bad],
          message = paste0(
            length(unique(df$panel[-1][bad])), " `panel_id` value(s) have an ",
            "exact age that decreases or changes by more than the time between ",
            "the survey dates (e.g. ", show_values(df$panel[-1][bad], 3), ")."
          )
        )
      }
    }
  }
  bind_findings(out)
}

# R41: one panel_id per part_id, in every file -------------------------------

rule_r41 <- function(ctx) {
  panel <- get_column(ctx, "participant_extra", "panel_id")
  part <- get_column(ctx, "participant_extra", "part_id")
  if (is.null(panel) || is.null(part)) {
    return(NULL)
  }
  out <- list()

  # forward: a part_id (one survey occasion) belongs to one individual only
  ok <- !is.na(panel) & !is.na(part)
  n_panel <- tapply(panel[ok], part[ok], function(x) length(unique(x)))
  multi <- names(n_panel)[n_panel > 1]
  if (length(multi) > 0) {
    bad <- which(part %in% multi)
    out[[1]] <- finding(
      "R41", file = file_label(ctx, "participant_extra"), column = "panel_id",
      n_fail = length(multi), n_total = length(n_panel), rows = bad,
      message = paste0(
        length(multi), " `part_id` value(s) are linked to more than one ",
        "`panel_id` (e.g. ", paste0("'", utils::head(multi, 3), "'",
                                    collapse = ", "), "). A `part_id` is one ",
        "survey occasion of one individual; give each occasion its own ",
        "`part_id` and share the `panel_id` across the occasions."
      )
    )
  }

  # reverse: where another file repeats panel_id, it must give the same link
  # (panel_id belongs in the participant extra file only, but if it is there
  # twice the two copies must agree)
  reference <- panel[ok][!duplicated(part[ok])]
  names(reference) <- part[ok][!duplicated(part[ok])]
  for (id in setdiff(names(ctx$tables), "participant_extra")) {
    other_panel <- get_column(ctx, id, "panel_id")
    other_part <- get_column(ctx, id, "part_id")
    if (is.null(other_panel) || is.null(other_part)) next
    expected <- unname(reference[other_part])
    differ <- which(!is.na(other_panel) & !is.na(expected) &
                      other_panel != expected)
    if (length(differ) == 0) next
    out[[length(out) + 1]] <- finding(
      "R41", file = file_label(ctx, id), column = "panel_id",
      n_fail = length(differ), n_total = length(other_panel), rows = differ,
      message = paste0(
        "`panel_id` differs from the `panel_id` of the same `part_id` in ",
        file_label(ctx, "participant_extra"), "; every file must give the ",
        "same individual for a `part_id`."
      )
    )
  }
  bind_findings(out)
}

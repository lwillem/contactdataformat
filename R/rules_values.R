# Value-level rules: R07-R14 ---------------------------------------------------

# Apply `fun(id, spec, x, file)` to every schema column that is present in the
# supplied common tables; collect the returned findings.
each_schema_column <- function(ctx, fun) {
  out <- list()
  for (id in schema_ids(ctx$schema, "common")) {
    if (!has_table(ctx, id)) next
    for (spec in schema_columns(ctx$schema, id)) {
      if (!spec$name %in% names(ctx$tables[[id]])) next
      out[[length(out) + 1]] <- fun(id, spec, ctx$tables[[id]][[spec$name]],
                                    file_label(ctx, id))
    }
  }
  bind_findings(out)
}

# Columns that hold numbers (not identifiers or category codes)
is_measure <- function(spec) {
  spec$type %in% c("integer", "numeric") && !isTRUE(spec$codes_in_dictionary)
}

rule_r07 <- function(ctx) {
  aliases <- unlist(ctx$schema$options$missing_token_aliases)
  placeholders <- unlist(ctx$schema$options$placeholder_values)
  out <- list()
  # Missing-value tokens other than NA, in every column of every table
  for (id in names(ctx$tables)) {
    tab <- ctx$tables[[id]]
    for (column in names(tab)) {
      x <- tab[[column]]
      bad <- which(x %in% aliases)
      if (length(bad) > 0) {
        out[[length(out) + 1]] <- finding(
          "R07", file = file_label(ctx, id), column = column,
          n_fail = length(bad), n_total = length(x), rows = bad,
          message = paste0("Missing values must be written as `NA`, found ",
                           show_values(x[bad]), ".")
        )
      }
    }
  }
  # Placeholder numbers in measure columns (schema) and numeric extra columns
  placeholder_scan <- function(id, spec, x, file) {
    if (!is_measure(spec)) return(NULL)
    codes <- c(placeholders, unlist(spec$placeholder_values))
    bad <- which(x %in% codes)
    if (length(bad) == 0) return(NULL)
    finding("R07", variant = "placeholder", file = file, column = spec$name,
            n_fail = length(bad), n_total = length(x), rows = bad,
            message = paste0("Possible placeholder code(s) ",
                             show_values(x[bad]),
                             " instead of `NA`."))
  }
  out[[length(out) + 1]] <- each_schema_column(ctx, placeholder_scan)
  for (id in schema_ids(ctx$schema, "extra")) {
    if (!has_table(ctx, id)) next
    tab <- ctx$tables[[id]]
    for (column in setdiff(names(tab), schema_key(ctx$schema, id))) {
      x <- tab[[column]]
      numeric_like <- !grepl("_id$", column) && any(!is.na(x)) &&
        all(is.na(x) | matches(x, ctx$schema$types$numeric$pattern))
      bad <- which(x %in% placeholders)
      if (numeric_like && length(bad) > 0) {
        out[[length(out) + 1]] <- finding(
          "R07", variant = "placeholder", file = file_label(ctx, id),
          column = column, n_fail = length(bad), n_total = length(x),
          rows = bad,
          message = paste0("Possible placeholder code(s) ",
                           show_values(x[bad]), " instead of `NA`.")
        )
      }
    }
  }
  bind_findings(out)
}

type_problem <- function(schema, spec, x) {
  type <- spec$type
  def <- schema$types[[type]]
  non_missing <- !is.na(x)
  if (identical(type, "boolean")) {
    return(non_missing & !x %in% unlist(def$values))
  }
  if (is.null(def$pattern)) {
    return(rep(FALSE, length(x)))
  }
  non_missing & !matches(ifelse(is.na(x), "", x), def$pattern)
}

rule_r08 <- function(ctx) {
  schema <- ctx$schema
  each_schema_column(ctx, function(id, spec, x, file) {
    out <- list()
    bad <- which(type_problem(schema, spec, x))
    if (length(bad) > 0) {
      description <- schema$types[[spec$type]]$description %||% spec$type
      hint <- if (identical(spec$type, "boolean")) {
        paste0(" Use ", paste(unlist(schema$types$boolean$values),
                              collapse = " or "),
               " (not 0/1, Y/N, yes/no).")
      } else {
        ""
      }
      out[[1]] <- finding(
        "R08", file = file, column = spec$name, n_fail = length(bad),
        n_total = length(x), rows = bad,
        message = paste0("Expected ", spec$type, " (", description,
                         "), found ", show_values(x[bad]), ".", hint)
      )
    }
    quoted <- ctx$file_info[[id]]$quoted[[spec$name]]
    if (length(quoted) > 0 && !is.null(spec$legacy_quoted_digits)) {
      out[[length(out) + 1]] <- finding(
        "R08", variant = "legacy_quoted", file = file, column = spec$name,
        n_fail = length(quoted), n_total = length(x), rows = quoted,
        message = paste0(
          "Quoted digits (\"0\"/\"1\") are a legacy form (e.g. Zenodo v1.2) ",
          "and accepted for now; write unquoted TRUE/FALSE (or 0/1)."
        )
      )
    }
    if (isFALSE(spec$nullable)) {
      missing <- which(is.na(x))
      if (length(missing) > 0) {
        out[[length(out) + 1]] <- finding(
          "R08", file = file, column = spec$name, n_fail = length(missing),
          n_total = length(x), rows = missing,
          message = "Missing values (`NA`) are not allowed in this column."
        )
      }
    }
    bind_findings(out)
  })
}

rule_r09 <- function(ctx) {
  schema <- ctx$schema
  each_schema_column(ctx, function(id, spec, x, file) {
    out <- list()
    allowed <- unlist(spec$enum)
    if (!is.null(spec$enum_ref)) {
      allowed <- unlist(schema$code_lists[[spec$enum_ref]])
    }
    if (length(allowed) > 0) {
      bad <- which(!is.na(x) & !x %in% allowed)
      if (length(bad) > 0) {
        shown <- if (length(allowed) <= 6) {
          paste0(" Allowed: ", paste(allowed, collapse = ", "), ".")
        } else {
          ""
        }
        out[[1]] <- finding(
          "R09", file = file, column = spec$name, n_fail = length(bad),
          n_total = length(x), rows = bad,
          message = paste0("Value(s) not allowed: ", show_values(x[bad]), ".",
                           shown)
        )
      }
    }
    in_r09 <- is.null(spec$range_rule) || identical(spec$range_rule, "R09")
    if (!is.null(spec$range) && in_r09) {
      out[[length(out) + 1]] <- range_finding("R09", spec, x, file)
    }
    bind_findings(out)
  })
}

range_finding <- function(rule_id, spec, x, file) {
  value <- to_number(x)
  bounds <- unlist(spec$range)
  bad <- which(!is.na(value) & (value < bounds[1] | value > bounds[2]))
  if (length(bad) == 0) {
    return(NULL)
  }
  finding(
    rule_id, file = file, column = spec$name, n_fail = length(bad),
    n_total = length(x), rows = bad,
    message = paste0("Value(s) outside the range ", bounds[1], "-", bounds[2],
                     ": ", show_values(x[bad]), ".")
  )
}

# Numeric age column, all NA when the (optional) column is absent
age_column <- function(tab, column) {
  if (column %in% names(tab)) to_number(tab[[column]]) else rep(NA_real_, nrow(tab))
}

rule_r10 <- function(ctx) {
  schema <- ctx$schema
  out <- list()
  # range of every age column
  out[[1]] <- each_schema_column(ctx, function(id, spec, x, file) {
    if (identical(spec$range_rule, "R10")) range_finding("R10", spec, x, file)
  })
  for (def in schema$cross_column_rules$age_intervals) {
    id <- def$table
    if (!has_table(ctx, id)) next
    tab <- ctx$tables[[id]]
    if (!any(c(def$exact, def$min, def$max) %in% names(tab))) next
    file <- file_label(ctx, id)
    exact <- age_column(tab, def$exact)
    min <- age_column(tab, def$min)
    max <- age_column(tab, def$max)
    both <- which(!is.na(exact) & (!is.na(min) | !is.na(max)))
    if (length(both) > 0) {
      out[[length(out) + 1]] <- finding(
        "R10", file = file, column = def$exact, n_fail = length(both),
        n_total = nrow(tab), rows = both,
        message = paste0(
          "Exact age and age interval are both filled; use `", def$exact,
          "` or `", def$min, "`/`", def$max, "`, never both (the other ",
          "fields must be `NA`)."
        )
      )
    }
    if (!isTRUE(schema$options$allow_open_age_interval)) {
      half <- which(xor(is.na(min), is.na(max)))
      if (length(half) > 0) {
        out[[length(out) + 1]] <- finding(
          "R10", file = file, column = def$min, n_fail = length(half),
          n_total = nrow(tab), rows = half,
          message = "Age intervals need both a lower and an upper bound."
        )
      }
    }
    reversed <- which(!is.na(min) & !is.na(max) & min > max)
    if (length(reversed) > 0) {
      out[[length(out) + 1]] <- finding(
        "R10", file = file, column = def$min, n_fail = length(reversed),
        n_total = nrow(tab), rows = reversed,
        message = paste0("Lower bound `", def$min, "` is larger than the ",
                         "upper bound `", def$max, "`.")
      )
    }
  }
  bind_findings(out)
}

rule_r11 <- function(ctx) {
  schema <- ctx$schema
  out <- list()
  for (def in schema$cross_column_rules$age_intervals) {
    id <- def$table
    if (!has_table(ctx, id)) next
    tab <- ctx$tables[[id]]
    if (!any(c(def$exact, def$min, def$max) %in% names(tab))) next
    unknown <- is.na(age_column(tab, def$exact)) &
      is.na(age_column(tab, def$min)) & is.na(age_column(tab, def$max))
    limit <- if (identical(id, "participant_common")) {
      schema$options$max_share_missing_participant_age
    } else {
      if ("cnt_age_multi" %in% names(tab)) {
        unknown <- unknown & is.na(tab$cnt_age_multi)
      }
      schema$options$max_share_missing_contact_age
    }
    share <- mean(unknown)
    if (nrow(tab) > 0 && share > limit) {
      out[[length(out) + 1]] <- finding(
        "R11", file = file_label(ctx, id), n_fail = sum(unknown),
        n_total = nrow(tab),
        message = sprintf("%.0f%% of the records have no age information (threshold %.0f%%).",
                          100 * share, 100 * limit)
      )
    }
  }
  bind_findings(out)
}

rule_r12 <- function(ctx) {
  id <- "sday"
  if (!has_table(ctx, id) || !has_column(ctx, id, "sday_id")) {
    return(NULL)
  }
  tab <- ctx$tables[[id]]
  file <- file_label(ctx, id)
  out <- list()
  x <- tab$sday_id
  parsed <- as.Date(x, format = "%Y%m%d")
  valid <- !is.na(x) & !is.na(parsed) & format(parsed, "%Y%m%d") == x
  bad <- which(!is.na(x) & !valid)
  if (length(bad) > 0) {
    out[[1]] <- finding(
      "R12", file = file, column = "sday_id", n_fail = length(bad),
      n_total = length(x), rows = bad,
      message = paste0("Not a valid calendar date in YYYYMMDD format: ",
                       show_values(x[bad]), ".")
    )
  }
  parts <- list(day = "%d", month = "%m", year = "%Y")
  for (column in names(parts)) {
    if (!column %in% names(tab)) next
    given <- to_number(tab[[column]])
    actual <- as.numeric(format(parsed, parts[[column]]))
    wrong <- which(valid & !is.na(given) & given != actual)
    if (length(wrong) > 0) {
      out[[length(out) + 1]] <- finding(
        "R12", file = file, column = column, n_fail = length(wrong),
        n_total = length(x), rows = wrong,
        message = paste0("`", column, "` does not match `sday_id`.")
      )
    }
  }
  if ("dayofweek" %in% names(tab)) {
    given <- to_number(tab$dayofweek)
    actual <- as.POSIXlt(parsed)$wday
    wrong <- which(valid & !is.na(given) & given != actual)
    if (length(wrong) > 0) {
      out[[length(out) + 1]] <- finding(
        "R12", variant = "weekday", file = file, column = "dayofweek",
        n_fail = length(wrong), n_total = length(x), rows = wrong,
        message = paste0(
          "`dayofweek` (0 = Sunday) differs from the weekday of `sday_id`. ",
          "If `sday_id` is the date of entry and `dayofweek` the assigned ",
          "diary day, waive this rule and document it in the dictionary."
        )
      )
    }
  }
  bind_findings(out)
}

rule_r13 <- function(ctx) {
  id <- "contact_common"
  if (!has_table(ctx, id)) {
    return(NULL)
  }
  cols <- Filter(function(s) identical(s$group, "location"),
                 schema_columns(ctx$schema, id))
  names_loc <- purrr::map_chr(cols, "name")
  names_loc <- intersect(names_loc, names(ctx$tables[[id]]))
  if (length(names_loc) < 2) {
    return(NULL)
  }
  tab <- ctx$tables[[id]][, names_loc, drop = FALSE]
  true_values <- unlist(ctx$schema$types$boolean$true_values)
  is_true <- vapply(tab, function(col) !is.na(col) & col %in% true_values,
                    logical(nrow(tab)))
  is_known <- vapply(tab, function(col) !is.na(col), logical(nrow(tab)))
  if (!is.matrix(is_true)) {
    is_true <- matrix(is_true, nrow = nrow(tab))
    is_known <- matrix(is_known, nrow = nrow(tab))
  }
  bad <- which(rowSums(is_true) == 0 & rowSums(is_known) > 0)
  if (length(bad) == 0) {
    return(NULL)
  }
  finding(
    "R13", file = file_label(ctx, id), n_fail = length(bad),
    n_total = nrow(tab), rows = bad,
    message = paste0(
      "Contact(s) with locations ", collapse_names(names_loc),
      " but none `TRUE`. Set the closest location to `TRUE` (map extra ",
      "categories such as kindergarten as documented) or, if the location ",
      "is unknown, set all location columns to `NA`."
    )
  )
}

# Codes documented in a free-text `values` cell ("1 = daily; 2 = weekly")
parse_codes <- function(values) {
  if (is.na(values) || trimws(values) == "") {
    return(character())
  }
  pieces <- trimws(strsplit(values, "[;|\n]+")[[1]])
  codes <- character()
  for (piece in pieces) {
    if (lengths(regmatches(piece, gregexpr("[=:]", piece))) > 1) {
      sub_pieces <- trimws(strsplit(piece, ",")[[1]])
    } else {
      sub_pieces <- piece
    }
    for (p in sub_pieces) {
      if (grepl("^[^=:]+[=:]", p)) {
        codes <- c(codes, trimws(sub("^([^=:]+)[=:].*$", "\\1", p)))
      }
    }
  }
  gsub("^[\"']|[\"']$", "", codes)
}

dictionary_entry <- function(ctx, id, variable) {
  dict <- ctx$dictionary$data
  if (is.null(dict)) {
    return(NULL)
  }
  hit <- dict[dict$file == id & dict$variable == variable, , drop = FALSE]
  if (nrow(hit) == 0) NULL else hit[1, , drop = FALSE]
}

rule_r14 <- function(ctx) {
  if (is.null(ctx$dictionary) || is.null(ctx$dictionary$data)) {
    return(NULL)
  }
  each_schema_column(ctx, function(id, spec, x, file) {
    if (!isTRUE(spec$codes_in_dictionary)) return(NULL)
    present <- unique(x[!is.na(x)])
    if (length(present) == 0) return(NULL)
    out <- list()
    entry <- dictionary_entry(ctx, id, spec$name)
    documented <- if (is.null(entry)) character() else parse_codes(entry$values)
    undocumented <- setdiff(present, documented)
    if (length(undocumented) > 0) {
      reason <- if (is.null(entry)) {
        "the variable is not in the dictionary"
      } else if (length(documented) == 0) {
        "no `code = label` mapping found in the dictionary"
      } else {
        paste0("documented codes: ", paste(documented, collapse = ", "))
      }
      out[[1]] <- finding(
        "R14", file = file, column = spec$name,
        n_fail = sum(x %in% undocumented), n_total = length(x),
        message = paste0("Code(s) ", show_values(undocumented),
                         " used in the data are not documented (", reason,
                         ").")
      )
    }
    recommended <- unlist(spec$recommended_codes)
    outside <- setdiff(present, recommended)
    if (length(recommended) > 0 && length(outside) > 0) {
      out[[length(out) + 1]] <- finding(
        "R14", variant = "scheme", file = file, column = spec$name,
        n_fail = sum(x %in% outside), n_total = length(x),
        message = paste0(
          "Codes ", show_values(outside), " differ from the recommended ",
          "scheme (", paste(recommended, collapse = ", "),
          "); keep your own codes only with an explicit mapping in the ",
          "dictionary."
        )
      )
    }
    bind_findings(out)
  })
}

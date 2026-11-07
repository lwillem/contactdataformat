# Heuristic, metadata, privacy rules: R21, R23-R27 -------------------------------

rule_r21 <- function(ctx) {
  schema <- ctx$schema
  contacts <- get_column(ctx, "contact_common", "part_id")
  participants <- get_column(ctx, "participant_common", "part_id")
  if (is.null(contacts) || is.null(participants)) {
    return(NULL)
  }
  out <- list()
  per_part <- table(contacts[!is.na(contacts)])
  too_many <- names(per_part)[per_part > schema$options$max_contacts_per_participant]
  if (length(too_many) > 0) {
    out[[1]] <- finding(
      "R21", file = file_label(ctx, "contact_common"), column = "part_id",
      n_fail = length(too_many), n_total = length(participants),
      message = paste0(
        length(too_many), " participant(s) have more than ",
        schema$options$max_contacts_per_participant, " contacts (",
        show_values(too_many), "); check for aggregated or imputed contacts."
      )
    )
  }
  zero_share <- mean(!participants %in% contacts)
  limit <- schema$options$max_share_zero_contact_participants
  if (length(participants) > 0 && zero_share > limit) {
    out[[length(out) + 1]] <- finding(
      "R21", file = file_label(ctx, "participant_common"), column = "part_id",
      n_fail = sum(!participants %in% contacts), n_total = length(participants),
      message = sprintf("%.0f%% of the participants reported no contact (threshold %.0f%%).",
                        100 * zero_share, 100 * limit)
    )
  }
  flag <- get_column(ctx, "contact_extra", "cnt_imputed")
  if (!is.null(flag) && any(flag == "TRUE", na.rm = TRUE)) {
    out[[length(out) + 1]] <- finding(
      "R21", variant = "imputed", file = file_label(ctx, "contact_extra"),
      column = "cnt_imputed", n_fail = sum(flag == "TRUE", na.rm = TRUE),
      n_total = length(flag),
      message = sprintf("%.0f%% of the contacts are flagged as imputed.",
                        100 * mean(flag == "TRUE", na.rm = TRUE))
    )
  }
  bind_findings(out)
}

# Does the metadata declare imputation or aggregation?
declares_imputation <- function(metadata) {
  value <- metadata$imputation
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(FALSE)
  }
  text <- tolower(trimws(paste(value, collapse = " ")))
  !text %in% c("", "no", "none", "false", "n", "not applicable", "n/a")
}

rule_r23 <- function(ctx) {
  out <- list()
  flag <- get_column(ctx, "contact_extra", "cnt_imputed")
  if (!is.null(flag)) {
    bad <- which(!is.na(flag) & !flag %in% c("TRUE", "FALSE"))
    if (length(bad) > 0) {
      out[[1]] <- finding(
        "R23", file = file_label(ctx, "contact_extra"),
        column = "cnt_imputed", n_fail = length(bad), n_total = length(flag),
        rows = bad,
        message = "`cnt_imputed` must be TRUE or FALSE (or NA)."
      )
    }
  } else if (!is.null(ctx$metadata) && declares_imputation(ctx$metadata)) {
    out[[1]] <- finding(
      "R23", file = file_label(ctx, "contact_extra"),
      message = paste0(
        "The metadata declares imputed or aggregated contacts but the ",
        "contact extra file has no `cnt_imputed` flag. Describe the imputation ",
        "in the dictionary and the Zenodo record (content: manual review)."
      )
    )
  }
  bind_findings(out)
}

rule_r24 <- function(ctx) {
  schema <- ctx$schema
  if (identical(ctx$source, "memory") && is.null(ctx$metadata)) {
    return(NULL)
  }
  if (identical(ctx$source, "folder") && is.na(ctx$nametag)) {
    return(NULL)
  }
  if (is.null(ctx$metadata)) {
    return(finding(
      "R24",
      message = paste0(
        "No metadata file `", ctx$nametag, "_", schema$metadata$basename,
        "` found. Study metadata (", paste(unlist(schema$metadata$required_fields),
                                          collapse = ", "),
        ") must accompany the data."
      )
    ))
  }
  file <- ctx$metadata_file %||% "metadata"
  parse_error <- attr(ctx$metadata, "parse_error")
  if (!is.null(parse_error)) {
    return(finding("R24", file = file,
                   message = paste0("The metadata file is not valid YAML: ",
                                    parse_error)))
  }
  required <- unlist(schema$metadata$required_fields)
  present <- vapply(required, function(field) {
    value <- ctx$metadata[[field]]
    !(is.null(value) || length(value) == 0 || all(is.na(value)) ||
        all(trimws(as.character(value)) == ""))
  }, logical(1))
  if (any(!present)) {
    return(finding(
      "R24", file = file, n_fail = sum(!present), n_total = length(required),
      message = paste0("Metadata field(s) missing or empty: ",
                       collapse_names(required[!present]),
                       ". Content must also be reviewed manually.")
    ))
  }
  NULL
}

rule_r25 <- function(ctx) {
  items <- Filter(function(i) startsWith(i$id, "R25") || startsWith(i$id, "R24"),
                  ctx$schema$manual_items)
  # items with `when_column: <file id>.<column>` apply only when that column
  # is present
  applies <- function(item) {
    if (is.null(item$when_column)) {
      return(TRUE)
    }
    target <- strsplit(item$when_column, ".", fixed = TRUE)[[1]]
    has_column(ctx, target[1], target[2])
  }
  items <- Filter(applies, items)
  purrr::map(items, function(item) {
    finding("R25", message = paste0(item$title, ": ", item$prompt))
  }) |>
    bind_findings()
}

name_tokens <- function(name) {
  name <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", name)
  tokens <- strsplit(tolower(name), "[^a-z0-9]+")[[1]]
  tokens[tokens != ""]
}

# Columns of the supplied tables that are not part of the schema
non_schema_columns <- function(ctx) {
  out <- list()
  for (id in names(ctx$tables)) {
    known <- schema_column_names(ctx$schema, id)
    for (column in setdiff(names(ctx$tables[[id]]), known)) {
      out[[length(out) + 1]] <- list(id = id, column = column)
    }
  }
  out
}

rule_r26 <- function(ctx) {
  tokens <- unlist(ctx$schema$privacy$identifier_tokens)
  out <- list()
  for (item in non_schema_columns(ctx)) {
    if (any(name_tokens(item$column) %in% tokens)) {
      out[[length(out) + 1]] <- finding(
        "R26", file = file_label(ctx, item$id), column = item$column,
        message = paste0("Column name suggests a personal identifier; remove ",
                         "it or justify it in the dictionary.")
      )
    }
  }
  bind_findings(out)
}

# Header of a CSV that was not read as a table
read_header <- function(path) {
  line <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
  if (length(line) == 0) {
    return(character())
  }
  line <- iconv(line, "latin1", "UTF-8")
  gsub("^\"|\"$", "", strsplit(line, ",", fixed = TRUE)[[1]])
}

rule_r27 <- function(ctx) {
  privacy <- ctx$schema$privacy
  strict <- unlist(privacy$geography_tokens_strict)
  declared <- unlist(privacy$geography_tokens_declared)
  level <- ctx$metadata$geography_level %||% NA_character_
  declared_ok <- !is.na(level) &&
    tolower(level[1]) %in% unlist(privacy$geography_declared_levels)
  tokens <- c(strict, if (declared_ok) character() else declared)
  geography_name <- function(column) {
    any(name_tokens(column) %in% tokens)
  }
  out <- list()

  # 1. restricted geography file inside the public folder
  for (file in ctx$restricted) {
    out[[length(out) + 1]] <- finding(
      "R27", file = file,
      message = paste0(
        "File looks like a restricted geography file (ZIP, coordinates). It ",
        "must not be part of the public record: socialmixr merges every CSV ",
        "that shares a column (e.g. `part_id`) with the participant table, ",
        "so it would be loaded and published with the survey."
      )
    )
  }
  # 2. other CSVs with geography columns, and the merge hazard
  main_columns <- main_table_columns(ctx)
  for (file in c(ctx$unrecognised, ctx$restricted)) {
    header <- read_header(file.path(ctx$path, file))
    geo <- Filter(geography_name, header)
    if (length(geo) > 0 && !file %in% ctx$restricted) {
      shares <- intersect(header, main_columns)
      out[[length(out) + 1]] <- finding(
        "R27", file = file,
        message = paste0(
          "Geography column(s) ", collapse_names(geo), " in a public file",
          if (length(shares) > 0) {
            paste0("; it shares ", collapse_names(shares),
                   " with the survey tables, so socialmixr would merge it")
          },
          ". Keep ZIP codes, addresses and coordinates in a separate ",
          "restricted file."
        )
      )
    }
  }
  # 3. geography column names in the tables themselves
  for (id in names(ctx$tables)) {
    for (column in names(ctx$tables[[id]])) {
      if (column %in% schema_column_names(ctx$schema, id)) next
      if (geography_name(column)) {
        out[[length(out) + 1]] <- finding(
          "R27", file = file_label(ctx, id), column = column,
          message = paste0(
            "Column name indicates geography (ZIP, address, coordinates",
            if (!declared_ok) ", municipality or NIS code",
            "). Public files may only carry geography at the declared level; ",
            "finer information belongs in a separate restricted file."
          )
        )
      }
    }
  }
  # 4. value heuristics for columns not in the schema
  out[[length(out) + 1]] <- scan_geography_values(ctx, geography_name)
  bind_findings(out)
}

scan_geography_values <- function(ctx, geography_name) {
  zip <- ctx$schema$privacy$zip_like
  boxes <- ctx$schema$privacy$coordinate_boxes
  out <- list()
  for (id in names(ctx$tables)) {
    known <- schema_column_names(ctx$schema, id)
    tab <- ctx$tables[[id]]
    candidates <- setdiff(names(tab), known)
    candidates <- Filter(Negate(geography_name), candidates)
    lat_like <- character()
    lon_like <- character()
    for (column in candidates) {
      x <- tab[[column]]
      values <- to_number(x[!is.na(x)])
      if (length(values) < 10 || anyNA(values)) next
      tokens <- name_tokens(column)
      skip <- any(tokens %in% c("id", "year", "weight", "age", "n", "count"))
      is_zip <- !skip && all(values == round(values)) &&
        all(values >= zip$min & values <= zip$max) &&
        length(unique(values)) >= zip$min_distinct &&
        all(nchar(x[!is.na(x)]) == 4)
      if (is_zip) {
        out[[length(out) + 1]] <- finding(
          "R27", variant = "zip_values", file = file_label(ctx, id),
          column = column,
          message = paste0("Values look like four-digit postal codes (",
                           length(unique(values)), " distinct); postal codes ",
                           "must not be in the public record.")
        )
      }
      for (box in boxes) {
        if (all(values >= box$lat[[1]] & values <= box$lat[[2]]) &&
              any(values != round(values))) {
          lat_like <- c(lat_like, column)
        }
        if (all(values >= box$lon[[1]] & values <= box$lon[[2]]) &&
              any(values != round(values))) {
          lon_like <- c(lon_like, column)
        }
      }
    }
    if (length(lat_like) > 0 && length(lon_like) > 0) {
      out[[length(out) + 1]] <- finding(
        "R27", variant = "coords_values", file = file_label(ctx, id),
        column = paste(c(lat_like[1], lon_like[1]), collapse = ", "),
        message = paste0(
          "Two numeric columns lie within geographic coordinate ranges ",
          "(latitude/longitude pair); coordinates must not be in the public ",
          "record."
        )
      )
    }
  }
  bind_findings(out)
}

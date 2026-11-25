# Zenodo metadata: rule R36 and check_zenodo_record() -----------------------------
#
# A local folder has no Zenodo metadata, so R36 has nothing to check there and
# gives no finding. It runs when the metadata is available:
#   * a folder downloaded with contactsurveys holds `NAMETAG_reference.json`
#     (title, authors, year, version, DOI, references);
#   * `check_zenodo_record()` reads the metadata of a published record (by DOI,
#     record number, URL or a saved JSON file), which needs a network
#     connection for a DOI.

zenodo_community <- "social_contact_data"

#' Check the Zenodo metadata of a record (optional)
#'
#' The metadata of the Zenodo record is the source of the citation of a dataset
#' (`contactsurveys::get_citation()`). This function checks it against the
#' conventions of the guideline (rule R36): resource type, title without
#' version, creators as persons, publication year, version such as `v1.0`,
#' description, the Social Contact Data community, an open licence, open access
#' and files, and the recommended references and keywords. It is optional and
#' not part of [check_contact_data()], because a local folder has no Zenodo
#' metadata. When a folder was downloaded from Zenodo with contactsurveys,
#' [check_contact_data()] checks the `NAMETAG_reference.json` it contains.
#'
#' @param x One of: a DOI (`10.5281/zenodo.123456`), a Zenodo record number or
#'   URL (needs a network connection; only the record number is sent), the
#'   path to a saved record (`.json` from the Zenodo API) or to a
#'   `NAMETAG_reference.json` file, a folder with such a reference file, or an
#'   already parsed record (a list).
#' @inheritParams check_contact_data
#' @param ... Passed on to the checker (`waive`, `quiet`).
#' @return A `socrates_report` object that only holds rule R36.
#' @export
#' @examples
#' ref <- tempfile(fileext = ".json")
#' writeLines('{"title":["Social contact survey in Belgium, 2010-2011"],
#'   "author":["Willem, Lander","Hens, Niel"],"year":[2020],
#'   "note":["Version v1.0"],"doi":["10.5281/zenodo.4302055"]}', ref)
#' check_zenodo_record(ref, quiet = TRUE)
check_zenodo_record <- function(x, schema = "latest", ...) {
  schema <- read_schema(schema)
  zenodo <- zenodo_input(x)
  ctx <- structure(
    list(
      schema = schema, source = "zenodo", path = NA_character_,
      nametag = zenodo$nametag %||% NA_character_,
      nametag_candidates = character(), tables = list(), file_names = list(),
      file_info = list(), all_files = character(), misnamed = character(),
      unrecognised = character(), restricted = character(),
      dictionary = NULL, dictionary_file = NULL, zenodo = zenodo,
      cache = new.env(parent = emptyenv())
    ),
    class = "socrates_data"
  )
  run_checks(ctx, rules = "R36", ...)
}

# Input handling -----------------------------------------------------------

zenodo_input <- function(x) {
  if (is.list(x)) {
    return(zenodo_from_parsed(x))
  }
  x <- as.character(x)[1]
  if (dir.exists(x)) {
    files <- list.files(x, pattern = "_reference\\.json$", full.names = TRUE)
    if (length(files) == 0) {
      cli::cli_abort("No `NAMETAG_reference.json` file in {.path {x}}.")
    }
    return(zenodo_from_file(files[1]))
  }
  if (file.exists(x)) {
    return(zenodo_from_file(x))
  }
  id <- zenodo_record_id(x)
  if (is.na(id)) {
    cli::cli_abort(c(
      "{.arg x} is not a file, a folder, a DOI or a Zenodo record.",
      i = "Use a DOI such as `10.5281/zenodo.123456`, a record URL, or a JSON file."
    ))
  }
  zenodo_from_parsed(zenodo_fetch(id))
}

zenodo_record_id <- function(x) {
  hit <- regmatches(x, regexpr("(zenodo\\.|records?/)[0-9]+", x))
  if (length(hit) == 1) {
    return(sub("^.*?([0-9]+)$", "\\1", hit))
  }
  if (grepl("^[0-9]+$", trimws(x))) trimws(x) else NA_character_
}

# Only the record number is sent to Zenodo
zenodo_fetch <- function(id) {
  url <- paste0("https://zenodo.org/api/records/", id)
  old <- options(timeout = 30)
  on.exit(options(old))
  tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(c(
        "The Zenodo record {id} could not be retrieved.",
        i = "Check the network connection and that the record is published.",
        x = conditionMessage(e)
      ))
    }
  )
}

zenodo_from_file <- function(file) {
  parsed <- tryCatch(
    jsonlite::fromJSON(file, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort("{.path {file}} is not valid JSON: {conditionMessage(e)}")
    }
  )
  out <- zenodo_from_parsed(parsed)
  out$file <- basename(file)
  tag <- sub("_reference\\.json$", "", basename(file))
  if (grepl("_reference\\.json$", basename(file))) out$nametag <- tag
  out
}

# Normalise a Zenodo API record or a contactsurveys reference file ------------

first_text <- function(x) {
  x <- unlist(x)
  if (length(x) == 0) NA_character_ else as.character(x[[1]])
}

zenodo_from_parsed <- function(p) {
  if (!is.null(p$metadata)) {
    m <- p$metadata
    creators <- m$creators
    files <- p$files
    file_names <- vapply(files, function(f) {
      as.character(f$key %||% f$filename %||% NA_character_)
    }, character(1))
    communities <- c(
      vapply(m$communities %||% list(), function(cm) as.character(cm$id %||% NA), character(1)),
      vapply(p$communities %||% list(), function(cm) as.character(cm$id %||% NA), character(1))
    )
    license <- m$license
    if (is.list(license)) license <- license$id
    refs <- m$references
    if (is.list(refs)) refs <- vapply(refs, function(r) {
      if (is.list(r)) as.character(r$reference %||% NA) else as.character(r)
    }, character(1))
    return(list(
      kind = "record",
      title = first_text(m$title),
      creators = vapply(creators, function(cr) as.character(cr$name %||% NA), character(1)),
      affiliation = vapply(creators, function(cr) !is.null(cr$affiliation) && nzchar(cr$affiliation), logical(1)),
      orcid = vapply(creators, function(cr) !is.null(cr$orcid) && nzchar(cr$orcid), logical(1)),
      year = suppressWarnings(as.integer(substr(first_text(m$publication_date), 1, 4))),
      version = first_text(m$version),
      doi = first_text(p$doi %||% m$doi),
      references = as.character(unlist(refs)),
      description = first_text(m$description),
      type = first_text(m$resource_type$type %||% m$resource_type$id),
      communities = communities[!is.na(communities)],
      license = first_text(license),
      access = first_text(m$access_right),
      files = file_names[!is.na(file_names)],
      keywords = as.character(unlist(m$keywords))
    ))
  }
  # contactsurveys reference file: every field is an array
  note <- first_text(p$note)
  list(
    kind = "reference",
    title = first_text(p$title),
    creators = as.character(unlist(p$author)),
    year = suppressWarnings(as.integer(first_text(p$year))),
    version = if (is.na(note)) NA_character_ else trimws(sub("^[Vv]ersion\\s*", "", note)),
    doi = first_text(p$doi),
    references = as.character(unlist(p$reference))
  )
}

# The rule -------------------------------------------------------------------

rule_r36 <- function(ctx) {
  z <- ctx$zenodo
  if (is.null(z)) {
    return(NULL)
  }
  file <- z[["file"]] %||% if (identical(z$kind, "record")) "Zenodo record" else NA_character_
  out <- list()
  add <- function(variant, message, column = NA_character_) {
    out[[length(out) + 1]] <<- finding("R36", variant = variant, file = file,
                                       column = column, message = message)
  }
  record <- identical(z$kind, "record")

  # title: present, without version number or NAMETAG
  if (is.na(z$title) || !nzchar(trimws(z$title))) {
    add("title", "The title is missing.", "title")
  } else if (grepl("(^|[^A-Za-z])v[0-9]+(\\.[0-9]+)*($|[^A-Za-z0-9])|[Vv]ersion\\s*[0-9]",
                   z$title) ||
               grepl("^[A-Z0-9]+_[0-9]{4}(_[A-Z0-9]+)*$", z$title)) {
    add("title_content", paste0(
      "The title contains a version number or a NAMETAG (`", z$title,
      "`). The title names the survey, the country and the period; the ",
      "version has its own field."), "title")
  }

  # creators: persons, family name first
  if (length(z$creators) == 0) {
    add("creators", "The record has no creators.", "creators")
  } else {
    plain <- z$creators[!grepl(",", z$creators, fixed = TRUE)]
    if (length(plain) > 0) {
      add("creators_name", paste0(
        length(plain), " of ", length(z$creators), " creator(s) are not in the ",
        "form `Family name, Given names` (", collapse_names(utils::head(plain, 3)),
        if (length(plain) > 3) ", ..." else "",
        "). The citation uses the family names of persons; do not enter an ",
        "organisation as creator."), "creators")
    }
  }

  # publication year
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.na(z$year)) {
    add("year", "The publication year is missing.", "year")
  } else if (z$year < 2000 || z$year > this_year + 1) {
    add("year", paste0("The publication year ", z$year, " is not plausible."),
        "year")
  }

  # version such as v1.0
  if (is.na(z$version) || !nzchar(z$version)) {
    add("version", "The version is missing; use a scheme such as `v1.0`.",
        "version")
  } else if (!grepl("^v[0-9]+(\\.[0-9]+)*$", z$version)) {
    add("version", paste0("The version `", z$version, "` does not follow the ",
                          "scheme `v1.0`, `v1.1`, ..."), "version")
  }

  # DOI
  if (is.na(z$doi) || !nzchar(z$doi)) {
    add("doi", "The DOI is missing.", "doi")
  } else if (!grepl("^10\\.5281/zenodo\\.[0-9]+$", z$doi)) {
    add("doi", paste0("The DOI `", z$doi, "` is not a Zenodo DOI ",
                      "(`10.5281/zenodo.<number>`)."), "doi")
  }

  if (length(z$references) == 0 || all(!nzchar(z$references))) {
    add("references", paste0("No references: add the publication(s) that ",
                             "describe the survey (recommended)."),
        "references")
  }

  if (record) {
    if (!identical(tolower(z$type), "dataset")) {
      add("type", paste0("The resource type is `", z$type %||% NA,
                         "`; it must be Dataset."), "resource_type")
    }
    if (is.na(z$description) || !nzchar(trimws(z$description))) {
      add("description", paste0("The description is missing: it states the ",
                                "design, the sample, the period and the ",
                                "geographic scope."), "description")
    }
    if (!zenodo_community %in% z$communities) {
      add("community", paste0("The record is not in the Social Contact Data ",
                              "community (`", zenodo_community, "`)."),
          "communities")
    }
    if (is.na(z$license) || !nzchar(z$license)) {
      add("license", "No licence is stated.", "license")
    } else if (!identical(tolower(z$license), "cc-by-4.0")) {
      add("license_other", paste0("The licence is `", z$license, "`; CC BY ",
                                  "4.0 is recommended (any open licence is ",
                                  "accepted)."), "license")
    }
    if (!identical(tolower(z$access), "open")) {
      add("access", paste0("The access right is `", z$access %||% NA,
                           "`; the record and its files must be open, ",
                           "because they are downloaded without ",
                           "authentication."), "access_right")
    }
    if (length(z$files) == 0) {
      add("files", "The record has no files.", "files")
    } else {
      tag <- names(infer_nametags(z$files, known_suffixes(read_schema("latest"))))[1]
      if (!is.na(tag) && !is.null(tag)) {
        family <- tolower(gsub("[^A-Za-z]", "", sub(",.*$", "", z$creators[1])))
        name <- tolower(gsub("[^A-Za-z]", "", strsplit(tag, "_")[[1]][1]))
        if (length(family) == 1 && nzchar(family) && nzchar(name) &&
              !identical(family, name)) {
          add("nametag_creator", paste0(
            "The NAME of the NAMETAG (`", tag, "`) is not the family name of ",
            "the first creator (`", sub(",.*$", "", z$creators[1]), "`)."))
        }
      }
    }
    if (length(z$keywords) == 0) {
      add("keywords", paste0("No keywords (recommended, e.g. `social ",
                             "contacts`, `contact matrix`)."), "keywords")
    }
    if (length(z$creators) > 0 && !all(z$affiliation) && !any(z$orcid)) {
      add("affiliation", paste0("Affiliations and ORCID iDs are recommended ",
                                "for the creators."), "creators")
    }
  }
  bind_findings(out)
}

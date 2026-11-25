test_that("bundled schema versions are listed and readable", {
  expect_true("1.0" %in% guideline_versions())
  schema <- read_schema("latest")
  expect_equal(schema$guideline_version, "1.0")
  expect_match(attr(schema, "hash"), "^[0-9a-f]{32}$")
  expect_error(read_schema("9.9"), "Unknown schema")
})

test_that("package MAJOR.MINOR equals the newest guideline version", {
  newest <- utils::tail(guideline_versions(), 1)
  pkg <- as.character(utils::packageVersion("contactdataformat"))
  expect_equal(
    paste(strsplit(pkg, ".", fixed = TRUE)[[1]][1:2], collapse = "."),
    newest
  )
})

test_that("the rule catalogue lists classes, severities and implementation", {
  rules <- list_rules()
  expect_true(all(sprintf("R%02d", 1:40) %in% rules$rule_id))
  expect_true(all(rules$class %in% c("A", "X", "H", "M")))
  expect_true(all(rules$severity %in% c("error", "warning", "note", "manual")))
  implemented <- rules$rule_id[rules$implemented]
  ns <- asNamespace("contactdataformat")
  for (id in implemented) {
    expect_true(is.function(get0(paste0("rule_", tolower(id)), envir = ns)),
                info = id)
  }
  expect_setequal(rules$rule_id[!rules$implemented],
                  c(sprintf("R%d", 28:33), "R36", "R37"))
})

test_that("open decisions point to existing schema keys", {
  decisions <- open_decisions()
  expect_true(all(c("Q1", "Q3", "Q4", "Q6", "Q14", "Q16") %in% decisions$id))
  expect_false(anyNA(decisions$where))
})

test_that("recommended guideline choices are encoded in the schema", {
  schema <- read_schema()
  contact <- schema_column_names(schema, "contact_common")
  expect_true(all(c("cnt_id", "cnt_otherplace") %in% contact))
  expect_false(any(c("cont_id", "cnt_others", "sday_id") %in% contact))
  spec <- schema_column(schema, "contact_common", "cnt_id")
  expect_equal(unlist(spec$aliases), "cont_id")
  expect_true("part_age_exact" %in%
                schema_column_names(schema, "participant_common"))
  expect_equal(schema$files$sday$key, "part_id")
})

test_that("a modified schema file changes the outcome without R changes", {
  tables <- valid_survey()
  tables$participant$hh_id[3] <- NA
  # hh_id is never NA (guideline decision)
  expect_hit(check_survey(tables), "R08")
  # a modified schema that allows NA accepts it again
  schema <- read_schema()
  yaml_text <- readLines(attr(schema, "path"))
  idx <- grep("name: hh_id", yaml_text)[1]
  nullable <- idx + grep("nullable: false", yaml_text[idx:(idx + 3)])[1] - 1
  yaml_text[nullable] <- sub("false", "true", yaml_text[nullable])
  fk <- grep("references: hh_common.hh_id", yaml_text)[1] + 1
  yaml_text[fk] <- sub("false", "true", yaml_text[fk])
  custom <- withr::local_tempfile(fileext = ".yaml")
  writeLines(yaml_text, custom)
  report <- check_survey(tables, schema = custom)
  expect_clean(report)
})

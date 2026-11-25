write_two_datasets <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  for (tag in c("AAA_2020_BELGIUM", "BBB_2021_FRANCE")) {
    dir.create(file.path(root, tag))
    tables <- valid_survey(10)
    write_survey(tables, file.path(root, tag), tag)
    dictionary_to_xlsx(file.path(root, tag), tables$dictionary, tag)
  }
  root
}

test_that("run_check checks every dataset sub-folder and writes the reports", {
  root <- write_two_datasets()
  run <- run_check(root)
  expect_s3_class(run, "socrates_run")
  expect_setequal(run$summary$dataset, c("AAA_2020_BELGIUM", "BBB_2021_FRANCE"))
  expect_setequal(names(run$reports), run$summary$dataset)
  out <- file.path(root, "check_reports")
  expect_true(file.exists(file.path(out, "check_summary.csv")))
  expect_true(file.exists(file.path(out, "AAA_2020_BELGIUM.csv")))
  expect_true(file.exists(file.path(out, "AAA_2020_BELGIUM.md")))
  # nothing is written into a dataset folder
  expect_false(any(grepl("report", list.files(file.path(root, "AAA_2020_BELGIUM")))))
  # a second run must not treat the output folder as a dataset
  expect_equal(nrow(run_check(root)$summary), 2)
})

test_that("loose csv files next to dataset folders are not a dataset", {
  root <- write_two_datasets()
  utils::write.csv(data.frame(a = 1), file.path(root, "check_summary.csv"))
  expect_equal(nrow(run_check(root, write = FALSE)$summary), 2)
})

test_that("run_check accepts a single dataset folder", {
  root <- write_two_datasets()
  run <- run_check(file.path(root, "AAA_2020_BELGIUM"), formats = "json")
  expect_equal(run$summary$dataset, "AAA_2020_BELGIUM")
  expect_true(file.exists(file.path(root, "check_reports", "AAA_2020_BELGIUM.json")))
  expect_false(file.exists(file.path(root, "check_reports", "AAA_2020_BELGIUM.csv")))
})

test_that("run_check can skip writing and refuses an output folder inside a dataset", {
  root <- write_two_datasets()
  run <- run_check(root, write = FALSE)
  expect_null(run$output_dir)
  expect_false(dir.exists(file.path(root, "check_reports")))
  expect_error(run_check(root, output_dir = file.path(root, "AAA_2020_BELGIUM", "out")),
               "inside")
  expect_error(run_check(file.path(root, "nope")), "existing folder")
  expect_error(run_check(withr::local_tempdir()), "No dataset folders")
})

test_that("run_check passes arguments on and goes on after errors", {
  root <- write_two_datasets()
  expect_no_error(run <- run_check(root, write = FALSE, nametag = "AAA_2020_BELGIUM"))
  expect_equal(nrow(run$summary), 2)
  expect_equal(run$summary$status[run$summary$dataset == "AAA_2020_BELGIUM"], "PASS")
})

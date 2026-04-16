# test_read_agd.R
# Tests for read_agd().

library(ActiSleep)


# ---- read_agd signature and no-crash ----------------------------------------

test_that("read_agd() is exported and callable", {
  expect_true(is.function(ActiSleep::read_agd))
})

test_that("read_agd() requires a file argument", {
  expect_error(read_agd(), "argument.*missing")
})

test_that("read_agd() errors informatively on non-existent file", {
  expect_error(read_agd("nonexistent_file.agd"))
})


# ---- no global assignment ----------------------------------------------------

test_that("read_agd() does not use <<- (global assignment)", {
  src <- deparse(body(read_agd))
  expect_false(
    any(grepl("<<-", src, fixed = TRUE)),
    info = "read_agd() must not use <<- (global assignment operator)"
  )
})


# ---- return structure (when an agd fixture is present) -----------------------
# If a fixture agd file is available at tests/testthat/fixtures/sample.agd,
# run structural checks.  Otherwise skip gracefully.

test_that("read_agd() returns a list with settings and raw.data", {
  agd_path <- testthat::test_path("fixtures", "sample.agd")
  skip_if_not(file.exists(agd_path), "No sample.agd fixture found")

  result <- read_agd(agd_path)

  expect_type(result, "list")
  expect_true("settings"  %in% names(result))
  expect_true("raw.data"  %in% names(result))

  expect_s3_class(result$settings,  "data.frame")
  expect_s3_class(result$raw.data,  "data.frame")

  # date must be first column of raw.data
  expect_equal(names(result$raw.data)[1], "date")

  # date column must be POSIXct
  expect_s3_class(result$raw.data$date, "POSIXct")

  # dataTimestamp column should have been dropped
  expect_false("dataTimestamp" %in% names(result$raw.data))
})

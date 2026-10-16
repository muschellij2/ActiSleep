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


# Build a small SQLite AGD file so read_agd() is covered without shipping a
# device-specific binary fixture in the package.
test_that("read_agd() reads settings and dated activity rows", {
  agd_path <- tempfile(fileext = ".agd")
  con <- DBI::dbConnect(RSQLite::SQLite(), agd_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # AGD timestamps use 100-nanosecond ticks from 0001-01-01.
  start <- as.numeric(as.POSIXct("2020-01-01 00:00:00", tz = "GMT"))
  ticks <- (start + 719162 * 86400) * 1e7
  DBI::dbWriteTable(con, "settings", data.frame(
    settingName = c("startdatetime", "epochcount"),
    settingValue = c(as.character(ticks), "2")
  ))
  DBI::dbWriteTable(con, "data", data.frame(
    dataTimestamp = 1:2,
    axis1 = c(10L, 20L)
  ))
  result <- read_agd(agd_path, sec = 60)

  # Verify both the output shape and the timestamp/channel values.
  expect_named(result, c("settings", "raw.data"))
  expect_identical(names(result$raw.data), c("date", "axis1"))
  expect_equal(result$raw.data$axis1, c(10L, 20L))
  # Explicit formatting preserves midnight's time component in the assertion.
  expect_equal(format(result$raw.data$date, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
               c("2020-01-01 00:00:00", "2020-01-01 00:01:00"))
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

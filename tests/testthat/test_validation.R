# test_validation.R
# Input validation tests for estimate_sleep().

library(ActiSleep)

# ---- empty data --------------------------------------------------------------

test_that("empty data throws an error", {
  expect_error(
    estimate_sleep(data.frame(), activity_col = "VM"),
    "'data' is empty"
  )
})


# ---- missing columns ---------------------------------------------------------

test_that("missing 'date' column throws an error", {
  df <- data.frame(VM = 1:10)
  expect_error(
    estimate_sleep(df, activity_col = "VM"),
    "must contain a column named 'date'"
  )
})

test_that("missing activity column throws an error", {
  df <- data.frame(date = Sys.time() + seq(0, 9 * 60, by = 60))
  expect_error(
    estimate_sleep(df, activity_col = "nonexistent"),
    "not found in 'data'"
  )
})


# ---- unparseable dates -------------------------------------------------------

test_that("unparseable dates throw an error", {
  df <- data.frame(
    date = rep("not-a-date", 10),
    VM   = rep(0L, 10)
  )
  expect_error(
    estimate_sleep(df, activity_col = "VM"),
    "Could not parse the 'date' column"
  )
})


# ---- parameter ranges --------------------------------------------------------

test_that("threshold_pct outside [0,1] throws an error", {
  data("AccelData", package = "ActiSleep")
  expect_error(
    estimate_sleep(AccelData, activity_col = "VM", threshold_pct = -0.1),
    "threshold_pct"
  )
  expect_error(
    estimate_sleep(AccelData, activity_col = "VM", threshold_pct = 1.5),
    "threshold_pct"
  )
})

test_that("segments_per_hour <= 0 throws an error", {
  data("AccelData", package = "ActiSleep")
  expect_error(
    estimate_sleep(AccelData, activity_col = "VM", segments_per_hour = 0),
    "segments_per_hour"
  )
})

test_that("no_activity_cutoff outside [0,1] throws an error", {
  data("AccelData", package = "ActiSleep")
  expect_error(
    estimate_sleep(AccelData, activity_col = "VM", no_activity_cutoff = 1.2),
    "no_activity_cutoff"
  )
})

test_that("invalid cost_model string throws an error", {
  data("AccelData", package = "ActiSleep")
  expect_error(
    estimate_sleep(AccelData, activity_col = "VM", cost_model = "invalid"),
    "cost_model"
  )
})

test_that("invalid cost_model integer throws an error", {
  data("AccelData", package = "ActiSleep")
  expect_error(
    estimate_sleep(AccelData, activity_col = "VM", cost_model = 6L),
    "cost_model"
  )
})


# ---- diary validation --------------------------------------------------------

test_that("use_diary = TRUE without diary throws an error", {
  data("AccelData", package = "ActiSleep")
  expect_error(
    estimate_sleep(AccelData, activity_col = "VM", use_diary = TRUE),
    "diary.*not provided"
  )
})

test_that("diary without 'bed'/'wake' columns throws an error", {
  data("AccelData", package = "ActiSleep")
  bad_diary <- data.frame(x = Sys.time(), y = Sys.time())
  expect_error(
    estimate_sleep(AccelData, activity_col = "VM",
                   use_diary = TRUE, diary = bad_diary),
    "columns 'bed' and 'wake'"
  )
})

test_that("use_diary = FALSE with diary supplied runs without error", {
  data("AccelData",      package = "ActiSleep")
  data("SleepDiary1Day", package = "ActiSleep")
  diary <- data.frame(bed = SleepDiary1Day[, 1], wake = SleepDiary1Day[, 2])
  # Should run without error (diary is silently ignored)
  expect_no_error(
    estimate_sleep(AccelData, activity_col = "VM",
                   use_diary = FALSE, diary = diary)
  )
})


# ---- auto-format parsing -----------------------------------------------------

test_that("M/D/YYYY H:MM date format (AccelData) is parsed correctly", {
  data("AccelData", package = "ActiSleep")
  # AccelData dates are already in M/D/YYYY H:MM format — verify auto-detection
  expect_true(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", AccelData$date[1]))
  expect_no_error(
    estimate_sleep(AccelData, activity_col = "VM")
  )
})

test_that("YYYY-MM-DD HH:MM:SS date format is parsed correctly", {
  data("AccelData", package = "ActiSleep")
  accel2 <- AccelData
  # Re-parse from AccelData's native format, then format as ISO
  accel2$date <- format(
    as.POSIXct(AccelData$date, format = "%m/%d/%Y %H:%M", tz = "GMT"),
    "%Y-%m-%d %H:%M:%S"
  )
  expect_no_error(
    estimate_sleep(accel2, activity_col = "VM")
  )
})

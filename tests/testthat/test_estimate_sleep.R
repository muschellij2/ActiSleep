# test_estimate_sleep.R
# Regression tests for estimate_sleep() against saved fixtures.

library(ActiSleep)

# ---- helpers -----------------------------------------------------------------

fixture <- function(name) {
  readRDS(file.path(
    testthat::test_path("fixtures"),
    paste0(name, ".rds")
  ))
}

# round durations to nearest minute for robust comparison
round_durations <- function(df) {
  df$duration_min <- round(as.numeric(df$duration_min))
  df
}


# ---- basic -------------------------------------------------------------------

test_that("basic result matches baseline fixture", {
  data("AccelData", package = "ActiSleep")
  result <- estimate_sleep(AccelData, activity_col = "VM")

  expect_s3_class(result, "actisleep_result")
  expect_true(!is.null(result$segments))
  expect_true(!is.null(result$parameters))

  base   <- fixture("baseline_basic")
  got    <- round_durations(as.data.frame(result))
  expect  <- round_durations(as.data.frame(base))

  expect_equal(nrow(got), nrow(expect))
  expect_equal(got$sleep_onset,  expect$sleep_onset)
  expect_equal(got$sleep_offset, expect$sleep_offset)
  expect_equal(got$duration_min, expect$duration_min)
  expect_equal(got$is_sleep,     expect$is_sleep)
  expect_equal(got$diary_overlap, expect$diary_overlap)
})

test_that("basic parameters match baseline", {
  data("AccelData", package = "ActiSleep")
  result <- estimate_sleep(AccelData, activity_col = "VM")
  base   <- fixture("baseline_basic")

  p  <- result$parameters
  bp <- base$parameters
  expect_equal(p$cost_model,        bp$cost_model)
  expect_equal(p$threshold_pct,     bp$threshold_pct)
  expect_equal(p$segments_per_hour, bp$segments_per_hour)
  expect_equal(p$valid_accel,       bp$valid_accel)
  expect_equal(p$day_of_week,       bp$day_of_week)
})


# ---- non-wear ----------------------------------------------------------------

test_that("non-wear result matches baseline fixture", {
  data("AccelData", package = "ActiSleep")
  result <- estimate_sleep(AccelData, activity_col = "VM", detect_nonwear = TRUE)

  base   <- fixture("baseline_nonwear")
  got    <- round_durations(as.data.frame(result))
  expect  <- round_durations(as.data.frame(base))

  expect_equal(nrow(got), nrow(expect))
  expect_equal(got$is_sleep, expect$is_sleep)
  expect_equal(result$parameters$valid_accel, base$parameters$valid_accel)
})


# ---- diary -------------------------------------------------------------------

test_that("diary result matches baseline fixture", {
  data("AccelData",      package = "ActiSleep")
  data("SleepDiary1Day", package = "ActiSleep")

  diary <- data.frame(
    bed  = SleepDiary1Day[, 1],
    wake = SleepDiary1Day[, 2]
  )

  result <- estimate_sleep(
    AccelData,
    cost_model         = "normal",
    activity_col       = "VM",
    threshold_pct      = 0,
    detect_nonwear     = TRUE,
    segments_per_hour  = 2,
    no_activity_cutoff = 0.45,
    min_sleep_minutes  = 5,
    use_diary          = TRUE,
    diary              = diary
  )

  base   <- fixture("baseline_diary")
  got    <- round_durations(as.data.frame(result))
  expect  <- round_durations(as.data.frame(base))

  expect_equal(nrow(got), nrow(expect))
  expect_equal(got$is_sleep,     expect$is_sleep)
  expect_equal(got$diary_overlap, expect$diary_overlap)
  expect_equal(got$duration_min, expect$duration_min, tolerance = 1)
})


# ---- S3 methods --------------------------------------------------------------

test_that("print returns result invisibly", {
  data("AccelData", package = "ActiSleep")
  result <- estimate_sleep(AccelData, activity_col = "VM")
  out <- capture.output(ret <- print(result))
  expect_identical(ret, result)
  expect_true(any(grepl("ActiSleep", out)))
})

test_that("summary returns object invisibly", {
  data("AccelData", package = "ActiSleep")
  result <- estimate_sleep(AccelData, activity_col = "VM")
  out <- capture.output(ret <- summary(result))
  expect_identical(ret, result)
  expect_true(any(grepl("Sleep segments", out)))
})

test_that("as.data.frame returns the segments data frame", {
  data("AccelData", package = "ActiSleep")
  result <- estimate_sleep(AccelData, activity_col = "VM")
  df <- as.data.frame(result)
  expect_s3_class(df, "data.frame")
  expect_true("sleep_onset" %in% names(df))
  expect_true("is_sleep"    %in% names(df))
})


# ---- cost model string and integer equivalence -------------------------------

test_that("cost_model string 'poisson' matches integer 1", {
  data("AccelData", package = "ActiSleep")
  r_str <- estimate_sleep(AccelData, activity_col = "VM", cost_model = "poisson")
  r_int <- estimate_sleep(AccelData, activity_col = "VM", cost_model = 1L)
  expect_equal(as.data.frame(r_str)$duration_min,
               as.data.frame(r_int)$duration_min)
})

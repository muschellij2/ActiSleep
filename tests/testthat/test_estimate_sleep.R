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

# Confirm every requested window is passed through to the local classifier.
test_that("non-wear window is applied across parameter values", {
  data("AccelData", package = "ActiSleep")
  windows <- c(60L, 120L, 180L)
  # These expected flags were established for the default weartime settings.
  expected_valid <- c(0L, 1L, 1L)

  for (i in seq_along(windows)) {
    result <- estimate_sleep(
      AccelData,
      activity_col = "VM",
      detect_nonwear = TRUE,
      min_wear_minutes = windows[i]
    )

    expect_equal(result$parameters$valid_accel, expected_valid[i])
  }
})

# Check full masks against hand-specified expected results for the default
# zero-run rule, including coercion, empty input, and boundary run lengths.
test_that("local wear classifier handles boundary scenarios", {
  expect_identical(ActiSleep:::classify_wear_time(numeric(), 2L), integer())
  expect_identical(
    ActiSleep:::classify_wear_time(c(0, 0, 12, 0), 2L),
    c(0L, 0L, 1L, 1L)
  )
  expect_identical(
    ActiSleep:::classify_wear_time(c(4, rep(0, 5), 101, rep(0, 3), 8), 5L),
    c(1L, rep(0L, 5), 1L, rep(1L, 3), 1L)
  )
  # Counts and windows are coerced to integer before classification.
  expect_identical(
    ActiSleep:::classify_wear_time(c(0, 0.5, 0, 0, 100, 0, 0, 0), 2.9),
    c(0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L)
  )
  # With days_distinct = FALSE, a run crossing midnight remains continuous.
  expect_identical(
    ActiSleep:::classify_wear_time(rep(0, 4), 4L),
    rep(0L, 4)
  )
  # A qualifying rolling window marks its entire zero block, including the
  # first and last 59 epochs around a 60-epoch window.
  edge_block <- c(7L, rep(0L, 120L), 9L)
  expect_identical(
    ActiSleep:::classify_wear_time(edge_block, 60L),
    c(1L, rep(0L, 120L), 1L)
  )
  # A zero block shorter than the window has no qualifying window.
  expect_identical(
    ActiSleep:::classify_wear_time(c(7L, rep(0L, 59L), 9L), 60L),
    rep(1L, 61L)
  )
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

# Reuse the package's accelerometer fixture to cover each public cost-model
# branch and the retained legacy wrapper without relying on external data.
test_that("estimate_sleep and legacy wrapper run all five cost models", {
  data("AccelData", package = "ActiSleep")
  expected_models <- c(
    "poisson", "normal", "negative_binomial", "variance", "exponential"
  )

  for (model in seq_along(expected_models)) {
    current <- estimate_sleep(
      AccelData,
      cost_model = model,
      activity_col = "VM",
      segments_per_hour = 0.5
    )
    expect_identical(current$parameters$cost_model, expected_models[model])
    expect_gt(nrow(current$segments), 0L)

    legacy <- ActiSleep:::SleepEstEachDay(
      AccelData,
      f = model,
      Y_name = "VM",
      S = 0.5
    )
    expect_true(is.data.frame(legacy$summary))
    expect_true(all(c("id", "valid_accel", "Sleep") %in%
                      names(legacy$summary)))
  }
})


# Cover the small helpers directly so their accepted date and interval forms
# remain stable independently of the full estimation pipeline.
test_that("time and segment helpers handle their documented inputs", {
  timestamps <- c("2020-01-01 00:00:00", "2020-01-01 00:01:00")
  formatted <- ActiSleep:::format_time(
    data.frame(date = timestamps), col_idx = 1L
  )
  expect_s3_class(formatted$date, "POSIXct")
  expect_equal(ActiSleep:::calculate_segment_mean(1L, c(0, 2, 4), 1L, 3L), 2)

  # Both parsed POSIXct dates and supported character dates make sleep windows.
  dates <- as.POSIXct(c("2020-01-01 12:00:00", "2020-01-02 12:00:00"),
                      tz = "GMT")
  interval_posix <- ActiSleep:::get_sleep_interval(data.frame(date = dates))
  interval_text <- ActiSleep:::get_sleep_interval(
    data.frame(date = format(dates, "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  )
  expect_equal(nrow(interval_posix), 1L)
  expect_equal(interval_posix, interval_text)
  expect_error(
    ActiSleep:::get_sleep_interval(data.frame(date = c("invalid", "invalid")))
  )

  diary_wide <- data.frame(
    bed1 = "2020-01-01 22:00:00",
    wake1 = "2020-01-02 07:00:00"
  )
  diary <- ActiSleep:::get_diary_subject(diary_wide, 1L, 1L, 2L)
  expect_named(diary, c("bed", "wake"))
  expect_equal(nrow(diary), 1L)
})

test_that("sleep-segment classification merges low-activity intervals", {
  dates <- as.POSIXct("2020-01-01 22:00:00", tz = "GMT") +
    seq(0, 50, by = 10) * 60
  interval <- data.frame(
    bed = as.POSIXct("2020-01-01 22:00:00", tz = "GMT"),
    wake = as.POSIXct("2020-01-01 23:00:00", tz = "GMT")
  )
  classified <- ActiSleep:::search_sleep_segments(
    date = dates,
    Y = c(0, 0, 4, 4, 0, 0),
    idx_start = c(1L, 3L, 5L),
    idx_end = c(2L, 4L, 6L),
    no_activity_cutoff = 0.5,
    min_sleep_minutes = 5,
    sleep_interval = interval
  )

  # The adjacent zero-activity epochs merge and overlap the supplied window.
  expect_equal(nrow(classified$dat_long), 3L)
  expect_equal(nrow(classified$dat_short), 3L)
  expect_equal(classified$dat_short$is_sleep, c(1, 0, 1))
  expect_true(all(classified$dat_short$diary_overlap == 1L))
})

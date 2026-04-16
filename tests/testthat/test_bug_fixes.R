# test_bug_fixes.R
# Regression tests confirming that specific bugs from v0.2.2 remain fixed.

library(ActiSleep)


# ---- Bug 1.2: aggregate_epochs() standalone crash ---------------------------
# aggregate_dat() / aggregate_epochs() used cut(dates, ...) where `dates` was
# undefined; the fix added `dates <- datain$date`.

test_that("aggregate_epochs() works standalone without crashing", {
  n  <- 120
  df <- data.frame(
    date = as.POSIXct("2020-01-01 00:00:00") + seq(0, by = 10, length.out = n),
    VM   = sample(0:1000, n, replace = TRUE)
  )
  # Should not throw
  agg <- ActiSleep:::aggregate_epochs(df, cutnum = 1, unit = "min")
  expect_s3_class(agg, "data.frame")
  expect_true("date" %in% names(agg))
  # 120 × 10-sec epochs → ~20 one-minute rows
  expect_lte(nrow(agg), 25)
})


# ---- Bug 1.3: data.frame() column naming (no unnamed columns) ---------------
# Using `<-` inside data.frame() produced unnamed columns, breaking downstream
# `$ts_start` / `$ts_end` references.

test_that("search_sleep_segments() output has named sleep_onset / sleep_offset", {
  data("AccelData", package = "ActiSleep")
  result <- estimate_sleep(AccelData, activity_col = "VM")
  df     <- as.data.frame(result)
  expect_true("sleep_onset"  %in% names(df))
  expect_true("sleep_offset" %in% names(df))
})


# ---- Bug 1.5: select_model() Variance branch K undefined --------------------
# `1:K` referenced an undefined variable K; should have been `1:Kmax`.
# Running select_model() with model "Variance Segmentation" and BIC/AIC pens
# must not throw "object 'K' not found".

test_that("select_model() works with Variance Segmentation + BIC penalty", {
  # Use synthetic variance-changing data (Variance model requires continuous data)
  set.seed(42)
  Y <- c(rnorm(100, sd = 1), rnorm(100, sd = 10))
  seg <- ActiSleep:::Segmentor(Y, model = 4L, Kmax = 6L, compress = FALSE)
  # Should not throw "object 'K' not found" (the bug we're testing)
  expect_no_error(
    ActiSleep:::SelectModel(seg, penalty = "BIC")
  )
  expect_no_error(
    ActiSleep:::SelectModel(seg, penalty = "AIC")
  )
})

test_that("select_model() works with Poisson + BIC penalty", {
  set.seed(42)
  Y <- as.integer(c(rpois(100, 50), rpois(100, 5)))
  seg <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = 6L, compress = FALSE)
  expect_no_error(ActiSleep:::SelectModel(seg, penalty = "BIC"))
})


# ---- Bug 1.6: S4 show() model name mismatches --------------------------------
# "Variance" corrected to "Variance Segmentation";
# "Negative Binomial" corrected to "Negative binomial".

test_that("Segmentor show() works for all 5 models", {
  set.seed(42)
  n  <- 100
  # Appropriate data type for each model
  models <- list(
    list(data = as.integer(c(rpois(n %/% 2, 50), rpois(n - n %/% 2, 5))),
         model = 1L, name = "Poisson"),
    list(data = c(rnorm(n %/% 2, 100, 5), rnorm(n - n %/% 2, 5, 5)),
         model = 2L, name = "Normal"),
    list(data = as.integer(c(rnbinom(n %/% 2, mu = 20, size = 1),
                              rnbinom(n - n %/% 2, mu = 2, size = 1))),
         model = 3L, name = "Negative binomial"),
    list(data = c(rnorm(n %/% 2, sd = 1), rnorm(n - n %/% 2, sd = 10)),
         model = 4L, name = "Variance Segmentation"),
    list(data = c(rexp(n %/% 2, 1), rexp(n - n %/% 2, 0.1)),
         model = 5L, name = "Exponential")
  )
  for (m in models) {
    seg <- ActiSleep:::Segmentor(m$data, model = m$model, Kmax = 4L,
                                  compress = FALSE)
    out <- capture.output(show(seg))
    expect_true(any(grepl(m$name, out, fixed = TRUE)),
                info = paste("show() for model", m$model, "/ name", m$name))
  }
})


# ---- Bug 1.7: read_agd() global assignment removed --------------------------
# `ad_set <<- ad_set` wrote to the global environment.
# Verifiable only indirectly: read_agd() source must not contain `<<-`.

test_that("read_agd() source contains no global assignment operator <<-", {
  src <- deparse(body(ActiSleep::read_agd))
  expect_false(any(grepl("<<-", src)),
               info = "read_agd() must not use <<- (global assignment)")
})


# ---- Bug: debug print statements removed ------------------------------------
# estimate_sleep() must not print to console during normal operation.

test_that("estimate_sleep() produces no unexpected console output", {
  data("AccelData", package = "ActiSleep")
  # Assign inside capture.output to prevent auto-printing of the result
  out <- capture.output(
    result <- estimate_sleep(AccelData, activity_col = "VM")
  )
  expect_length(out, 0L)
})

# test_segmentor.R
# Tests for the PDP segmentor (S4 class + Rcpp interface).

library(ActiSleep)


# ---- helpers -----------------------------------------------------------------

make_data <- function(n = 200, seed = 42) {
  set.seed(seed)
  as.integer(c(
    rpois(n %/% 2, lambda = 50),
    rpois(n - n %/% 2, lambda = 5)
  ))
}


# ---- All 5 cost models -------------------------------------------------------

test_that("Segmentor() works with Poisson (model 1)", {
  Y   <- make_data()
  seg <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = 5L, compress = FALSE)
  expect_s4_class(seg, "Segmentor")
  expect_equal(nrow(ActiSleep:::getBreaks(seg)), 5L)
})

test_that("Segmentor() works with Normal (model 2)", {
  set.seed(1)
  Y   <- as.double(c(rnorm(100, mean = 100, sd = 5),
                     rnorm(100, mean = 5,   sd = 5)))
  seg <- ActiSleep:::Segmentor(Y, model = 2L, Kmax = 5L, compress = FALSE)
  expect_s4_class(seg, "Segmentor")
})

test_that("Segmentor() works with Negative binomial (model 3)", {
  Y   <- make_data()
  seg <- ActiSleep:::Segmentor(Y, model = 3L, Kmax = 5L, compress = FALSE)
  expect_s4_class(seg, "Segmentor")
})

test_that("Segmentor() works with Variance (model 4)", {
  set.seed(2)
  # Variance model requires continuous data with changing variance
  Y   <- as.double(c(rnorm(100, sd = 1), rnorm(100, sd = 10)))
  seg <- ActiSleep:::Segmentor(Y, model = 4L, Kmax = 4L, compress = FALSE)
  expect_s4_class(seg, "Segmentor")
})

test_that("Segmentor() works with Exponential (model 5)", {
  set.seed(3)
  # Exponential model requires positive continuous data
  Y   <- as.double(c(rexp(100, rate = 1), rexp(100, rate = 0.1)))
  seg <- ActiSleep:::Segmentor(Y, model = 5L, Kmax = 4L, compress = FALSE)
  expect_s4_class(seg, "Segmentor")
})

# Exercise every Rcpp keep=TRUE entry point and the formatted parameter table.
test_that("all kept Segmentor models retain costs and print parameters", {
  set.seed(17)
  model_inputs <- list(
    list(model = 1L, data = as.integer(rpois(100, lambda = 20))),
    list(model = 2L, data = rnorm(100, mean = 10, sd = 2)),
    list(model = 3L, data = as.integer(rpois(100, lambda = 20))),
    list(model = 4L, data = c(rnorm(50, sd = 1), rnorm(50, sd = 5))),
    list(model = 5L, data = rexp(100, rate = 0.2))
  )

  for (input in model_inputs) {
    seg <- ActiSleep:::Segmentor(input$data, model = input$model,
                                 Kmax = 4L, compress = FALSE, keep = TRUE)
    expect_s4_class(seg, "Segmentor")
    expect_true(length(ActiSleep:::getCost(seg)) > 0L)
    expect_true(length(ActiSleep:::getParameters(seg)) > 0L)
    # Segmentor stores its parameters as an S4 slot, not a list element.
    expect_true("parameters" %in% slotNames(seg))

    printed <- capture.output(print(seg))
    expect_true(any(grepl("Table of parameters", printed)))
  }
})


# ---- Accessor methods --------------------------------------------------------

test_that("getBreaks() returns integer matrix with correct dims", {
  Y    <- make_data()
  Kmax <- 6L
  seg  <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = Kmax, compress = FALSE)
  brks <- ActiSleep:::getBreaks(seg)
  expect_true(is.matrix(brks))
  expect_equal(nrow(brks), Kmax)
  expect_equal(ncol(brks), Kmax)
})

test_that("getParameters() returns matrix with correct dims", {
  Y    <- make_data()
  Kmax <- 5L
  seg  <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = Kmax, compress = FALSE)
  par  <- ActiSleep:::getParameters(seg)
  expect_true(is.matrix(par))
  expect_equal(nrow(par), Kmax)
})

test_that("getLikelihood() returns numeric vector of length Kmax", {
  Y    <- make_data()
  Kmax <- 5L
  seg  <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = Kmax, compress = FALSE)
  lik  <- ActiSleep:::getLikelihood(seg)
  expect_true(is.numeric(lik))
  expect_equal(length(lik), Kmax)
})


# ---- BestSegmentation --------------------------------------------------------

test_that("BestSegmentation() returns a list with bestCost matrix", {
  # BestSegmentation requires keep=TRUE. Use compress=FALSE so dimension
  # calculations inside BestSegmentation are straightforward (no RLE reduction).
  set.seed(42)
  # Use data without repeated values to avoid compression issues
  Y    <- as.integer(sample(1:1000, 200, replace = FALSE))
  Kmax <- 5L
  seg  <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = Kmax,
                                 compress = FALSE, keep = TRUE)
  result <- ActiSleep:::BestSegmentation(seg, K = 3L)
  expect_type(result, "list")
  expect_true("bestCost" %in% names(result))
  expect_true(is.matrix(result$bestCost))
})


# ---- SelectModel -------------------------------------------------------------

test_that("SelectModel() returns the selected number of segments K (integer)", {
  Y    <- make_data()
  Kmax <- 8L
  seg  <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = Kmax, compress = FALSE)
  K    <- ActiSleep:::SelectModel(seg, penalty = "BIC")
  expect_true(is.numeric(K) || is.integer(K))
  expect_length(K, 1L)
  expect_true(K >= 1L && K <= Kmax)
})

test_that("SelectModel() works with all penalty types for Poisson", {
  Y    <- make_data()
  seg  <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = 6L, compress = FALSE)
  for (pen in c("BIC", "AIC", "mBIC")) {
    K <- ActiSleep:::SelectModel(seg, penalty = pen)
    expect_length(K, 1L)
  }
})

test_that("SelectModel() works with Variance model (bug 1.5 regression)", {
  set.seed(42)
  Y   <- c(rnorm(100, sd = 1), rnorm(100, sd = 10))
  seg <- ActiSleep:::Segmentor(Y, model = 4L, Kmax = 6L, compress = FALSE)
  # Bug 1.5: 1:K where K was undefined — must not throw "object 'K' not found"
  for (pen in c("BIC", "AIC")) {
    K <- ActiSleep:::SelectModel(seg, penalty = pen)
    expect_length(K, 1L)
  }
})


# ---- compress = TRUE ---------------------------------------------------------

test_that("Segmentor() with compress = TRUE produces valid breaks", {
  Y   <- make_data(n = 300)
  seg <- ActiSleep:::Segmentor(Y, model = 1L, Kmax = 5L, compress = TRUE)
  expect_s4_class(seg, "Segmentor")
  brks <- ActiSleep:::getBreaks(seg)
  expect_true(is.matrix(brks))
})


# Exercise the validation and model-selection branches used by internal
# segmentation helpers, including penalties that are not used by the main API.
test_that("Segmentor() validates its model and input", {
  expect_error(ActiSleep:::Segmentor(1:10, model = 6L), "Choose model")
  expect_error(ActiSleep:::Segmentor(numeric()), "vector of data")
})

# Ensure the existing formatted print method participates in S3 dispatch.
test_that("Segmentor objects use their registered print method", {
  seg <- ActiSleep:::Segmentor(as.integer(1:30), model = 1L, Kmax = 3L,
                               compress = FALSE)
  output <- capture.output(result <- withVisible(print(seg)))
  expect_true(any(grepl("Model used for the segmentation", output)))
  # Base print methods return their input invisibly for assignment and chaining.
  expect_identical(result$value, seg)
  expect_false(result$visible)
})

test_that("SelectModel() supports model-specific penalties and criteria", {
  set.seed(11)
  samples <- list(
    poisson = list(model = 1L, data = as.integer(rpois(100, 10))),
    normal = list(model = 2L, data = rnorm(100)),
    negative_binomial = list(model = 3L, data = as.integer(rpois(100, 10))),
    exponential = list(model = 5L, data = rexp(100))
  )

  for (sample in samples) {
    # BIC, AIC, and oracle are supported for these four models.
    seg <- ActiSleep:::Segmentor(sample$data, model = sample$model,
                                 Kmax = 4L, compress = FALSE)
    for (penalty in c("BIC", "AIC", "oracle")) {
      selected <- ActiSleep:::SelectModel(seg, penalty = penalty)
      expect_length(selected, 1L)
      expect_true(selected >= 1L && selected <= 4L)
    }
  }

  variance <- ActiSleep:::Segmentor(rnorm(100), model = 4L, Kmax = 4L,
                                    compress = FALSE)
  expect_error(ActiSleep:::SelectModel(variance, penalty = "oracle"),
               "no oracle penalty")
  expect_error(ActiSleep:::SelectModel(variance, penalty = "mBIC"),
               "no mBIC")
})

test_that("BestSegmentation() validates saved costs and breakpoint counts", {
  data <- as.integer(sample(1:1000, 100, replace = FALSE))
  without_cost <- ActiSleep:::Segmentor(data, model = 1L, Kmax = 4L,
                                        compress = FALSE, keep = FALSE)
  expect_error(ActiSleep:::BestSegmentation(without_cost, K = 2L),
               "option keep=TRUE")

  with_cost <- ActiSleep:::Segmentor(data, model = 1L, Kmax = 4L,
                                     compress = FALSE, keep = TRUE)
  expect_error(ActiSleep:::BestSegmentation(with_cost, K = 1L),
               "only one segmentation")
  expect_warning(result <- ActiSleep:::BestSegmentation(with_cost, K = 5L),
                 "Kmax=K")
  expect_true(is.matrix(result$bestCost))
})

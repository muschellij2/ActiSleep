# actisleep_result.R
# S3 class for the return value of estimate_sleep().
#
# Constructor: new_actisleep_result()
# Methods:     print, summary, as.data.frame


# ---- Constructor (internal) -------------------------------------------------

#' Create an actisleep_result object
#'
#' @param segments   Data frame of sleep segment estimates.
#' @param parameters Named list of analysis parameters.
#' @return Object of class \code{actisleep_result}.
#' @keywords internal
new_actisleep_result <- function(segments, parameters) {
  structure(
    list(segments = segments, parameters = parameters),
    class = "actisleep_result"
  )
}


# ---- print ------------------------------------------------------------------

#' Print an actisleep_result
#'
#' Displays a concise, formatted summary of the sleep estimation output.
#'
#' @param x   An \code{actisleep_result} object.
#' @param ... Ignored.
#' @return \code{x} invisibly.
#' @export
print.actisleep_result <- function(x, ...) {
  p   <- x$parameters
  seg <- x$segments

  cat("ActiSleep Sleep Estimation\n")
  cat(rep("-", 40), "\n", sep = "")

  if (!is.na(p$subject_id))
    cat("  Subject ID    :", p$subject_id, "\n")

  cat("  Day of week   :", p$day_of_week,       "\n")
  cat("  Cost model    :", p$cost_model,         "\n")
  cat("  Threshold pct :", p$threshold_pct,      "\n")
  cat("  Seg/hour      :", p$segments_per_hour,  "\n")
  cat("  Valid accel   :", ifelse(p$valid_accel == 1L, "yes", "no"), "\n")

  cat(rep("-", 40), "\n", sep = "")

  n_sleep <- sum(seg$is_sleep == 1L, na.rm = TRUE)
  if (n_sleep == 0 || all(is.na(seg$sleep_onset))) {
    cat("  No sleep segments detected.\n")
  } else {
    cat(sprintf("  Sleep segments found: %d\n", n_sleep))
    sleep_rows <- seg[!is.na(seg$is_sleep) & seg$is_sleep == 1L, ]
    for (i in seq_len(nrow(sleep_rows))) {
      cat(sprintf("    [%d] %s -> %s  (%.0f min, %.0f%% zero)\n",
                  i,
                  sleep_rows$sleep_onset[i],
                  sleep_rows$sleep_offset[i],
                  as.numeric(sleep_rows$duration_min[i]),
                  100 * sleep_rows$pct_zero_activity[i]))
    }
  }
  cat(rep("-", 40), "\n", sep = "")

  invisible(x)
}


# ---- summary ----------------------------------------------------------------

#' Summarise an actisleep_result
#'
#' Prints key aggregate statistics: total sleep time, number of sleep segments,
#' and average proportion of zero-activity epochs.
#'
#' @param object An \code{actisleep_result} object.
#' @param ...    Ignored.
#' @return \code{object} invisibly.
#' @export
summary.actisleep_result <- function(object, ...) {
  seg <- object$segments

  sleep_rows <- seg[!is.na(seg$is_sleep) & seg$is_sleep == 1L, ]
  total_min  <- sum(as.numeric(sleep_rows$duration_min), na.rm = TRUE)
  n_segs     <- nrow(sleep_rows)
  mean_zero  <- mean(sleep_rows$pct_zero_activity, na.rm = TRUE)

  cat("ActiSleep Result Summary\n")
  cat(rep("-", 40), "\n", sep = "")
  cat(sprintf("  Sleep segments     : %d\n",   n_segs))
  cat(sprintf("  Total sleep time   : %.0f min (%.1f hr)\n",
              total_min, total_min / 60))
  cat(sprintf("  Mean %% zero epochs : %.1f%%\n", 100 * mean_zero))
  cat(rep("-", 40), "\n", sep = "")

  invisible(object)
}


# ---- as.data.frame ----------------------------------------------------------

#' Extract the segments data frame from an actisleep_result
#'
#' @param x   An \code{actisleep_result} object.
#' @param ... Ignored.
#' @return The \code{segments} data frame.
#' @export
as.data.frame.actisleep_result <- function(x, ...) {
  x$segments
}

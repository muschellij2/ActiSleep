# estimate_sleep.R
# Primary user-facing function for sleep estimation, plus internal helpers.
#
# Functions defined here:
#   estimate_sleep()         - main exported function
#   get_sleep_interval()     - internal: default bed/wake intervals from data dates
#   search_sleep_segments()  - internal: classify PDP segments as sleep/wake
#   calculate_segment_mean() - internal: mean of a data slice by index
#   aggregate_epochs()       - internal: aggregate sub-minute data to 1-minute
#   format_time()            - internal: coerce a character/POSIXct column to POSIXct


# ---- format_time (internal) --------------------------------------------------

#' Reformat a time column to POSIXct
#'
#' Converts one or more columns of a data frame from character or numeric
#' representations to \code{POSIXct} objects.
#'
#' @param datain Data frame containing the time column(s).
#' @param col_idx Integer vector of column indices to convert.
#' @param format Character string giving the date-time format (passed to
#'   \code{\link[base]{as.POSIXct}}).  Default \code{"\%Y-\%m-\%d \%H:\%M:\%S"}.
#'
#' @return The data frame with the specified columns converted to POSIXct.
#' @keywords internal
format_time <- function(datain,
                        col_idx,
                        format = "%Y-%m-%d %H:%M:%S")
{
  for (i in col_idx) {
    datain[, i] <- as.POSIXct(
      datain[, i],
      format = format,
      origin = "0001-01-01"
    )
  }
  return(datain)
}

# Keep the old name as an un-exported alias so existing internal calls still work
#' @keywords internal
ChangeTimeVar <- format_time


# ---- aggregate_epochs (internal) --------------------------------------------

#' Aggregate sub-minute accelerometer epochs to 1-minute intervals
#'
#' Sums all numeric columns over non-overlapping time windows of \code{cutnum}
#' \code{unit}s.  The \code{date} column is reconstructed from the cut labels.
#'
#' @param datain Data frame with a \code{date} column (POSIXct or coercible).
#' @param cutnum Numeric size of the aggregation window.  Default \code{1}.
#' @param unit Character, either \code{"min"} or \code{"sec"}.
#' @param tz Timezone string.  Default \code{"GMT"}.
#'
#' @return Data frame aggregated to the requested interval, with \code{date}
#'   as the first column.
#' @keywords internal
aggregate_epochs <- function(datain,
                             cutnum = 1,
                             unit   = c("min", "sec"),
                             tz     = "GMT")
{
  unit  <- match.arg(unit)
  dates <- datain$date
  dat   <- aggregate(
    . ~ cut(dates, paste(cutnum, unit)),
    datain[setdiff(names(datain), "date")],
    sum
  )
  names(dat)[1] <- "date"
  dat$date      <- strptime(dat$date, "%Y-%m-%d %H:%M:%S", tz = tz)
  return(dat)
}

# Keep the old name as an un-exported alias
#' @keywords internal
aggregate_dat <- aggregate_epochs


# ---- calculate_segment_mean (internal) --------------------------------------

#' Mean of a data vector between two index positions
#'
#' @param i Integer index into \code{idx_start} / \code{idx_end}.
#' @param datain Numeric vector.
#' @param idx_start Integer vector of segment start indices.
#' @param idx_end   Integer vector of segment end indices.
#'
#' @return Scalar mean of \code{datain[idx_start[i]:idx_end[i]]}.
#' @keywords internal
calculate_segment_mean <- function(i, datain, idx_start, idx_end)
{
  mean(datain[idx_start[i]:idx_end[i]])
}

# Keep old name
#' @keywords internal
cal_mean_idx <- calculate_segment_mean


# ---- get_sleep_interval (internal) ------------------------------------------

#' Generate default bed/wake intervals from accelerometer dates
#'
#' When no sleep diary is provided, this function creates one interval per
#' night using fixed clock times applied to the dates present in \code{datain}.
#'
#' @param datain Data frame with a \code{date} column (POSIXct).
#' @param bed    Character clock time for bed, e.g. \code{"22:00:00"}.
#' @param wake   Character clock time for wake, e.g. \code{"08:00:00"}.
#' @param tz     Timezone string.  Default \code{"GMT"}.
#'
#' @return Data frame with columns \code{bed} and \code{wake} (POSIXct).
#' @keywords internal
get_sleep_interval <- function(datain,
                               bed  = "22:00:00",
                               wake = "08:00:00",
                               tz   = "GMT")
{
  # Parse date column robustly — accept POSIXct or common string formats
  if (inherits(datain$date, "POSIXct") || inherits(datain$date, "POSIXlt")) {
    date_formatted <- as.POSIXct(datain$date, tz = tz)
  } else {
    date_formatted <- ymd_hms(datain$date, tz = tz, quiet = TRUE)
    if (all(is.na(date_formatted)))
      date_formatted <- as.POSIXct(datain$date, tz = tz)
  }
  if (all(is.na(date_formatted)))
    stop("Could not parse 'date' column; please ensure it is POSIXct or a recognized string.")
  date_names <- names(table(format(date_formatted, "%Y-%m-%d")))
  n_date     <- length(date_names)
  bed_char   <- paste(date_names[-n_date], bed)
  wake_char  <- paste(date_names[-1],      wake)

  bed_time  <- as.POSIXct(bed_char,  format = "%Y-%m-%d %H:%M:%S", tz = tz)
  wake_time <- as.POSIXct(wake_char, format = "%Y-%m-%d %H:%M:%S", tz = tz)

  data.frame(bed = bed_time, wake = wake_time)
}

# Keep old name
#' @keywords internal
GetEstSleepInterval_subj <- get_sleep_interval


# ---- get_diary_subject (internal) -------------------------------------------

#' Extract bed and wake times for one subject from a wide-format diary
#'
#' Converts columns from a wide-format diary data frame into a tidy
#' \code{data.frame(bed, wake)} with one row per night.
#'
#' @param datain   Data frame of diary data (one subject per row).
#' @param subj_idx Integer row index of the subject.
#' @param idx_bed  Integer vector of column indices for bed times.
#' @param idx_wake Integer vector of column indices for wake times.
#' @param tz       Timezone string.  Default \code{"GMT"}.
#'
#' @return Data frame with columns \code{bed} and \code{wake} (POSIXct).
#' @keywords internal
get_diary_subject <- function(datain,
                              subj_idx,
                              idx_bed,
                              idx_wake,
                              tz = "GMT")
{
  n_days    <- length(idx_bed)
  bed_vals  <- as.POSIXct(as.character(datain[subj_idx, idx_bed]),
                           format = "%Y-%m-%d %H:%M:%S", tz = tz)
  wake_vals <- as.POSIXct(as.character(datain[subj_idx, idx_wake]),
                           format = "%Y-%m-%d %H:%M:%S", tz = tz)
  data.frame(bed = bed_vals, wake = wake_vals)
}

# Keep old name
#' @keywords internal
GetDiary_subj <- get_diary_subject


# ---- search_sleep_segments (internal) ---------------------------------------

#' Classify PDP segments as sleep or wake
#'
#' Given the breakpoints produced by the PDP segmentor, this function
#' merges consecutive no-physical-activity (NoPA) segments, computes per-
#' segment statistics, and evaluates overlap with an external sleep interval
#' (diary or default).
#'
#' @param date      POSIXct vector of timestamps.
#' @param Y         Numeric vector of thresholded activity counts.
#' @param idx_start Integer vector of segment start indices.
#' @param idx_end   Integer vector of segment end indices.
#' @param no_activity_cutoff Proportion of zero-count epochs required to
#'   classify a segment as NoPA.  Default \code{0.7}.
#' @param min_sleep_minutes Minimum duration (minutes) for a valid sleep
#'   segment.  Default \code{30}.
#' @param sleep_interval Data frame with columns \code{bed} and \code{wake}
#'   (POSIXct) defining the expected sleep window.
#' @param tz Timezone string.  Default \code{"GMT"}.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{dat_long}{Data frame of original (un-merged) segments.}
#'     \item{dat_short}{Data frame of merged NoPA segments with overlap flag.}
#'   }
#' @keywords internal
search_sleep_segments <- function(date,
                                  Y,
                                  idx_start,
                                  idx_end,
                                  no_activity_cutoff = 0.7,
                                  min_sleep_minutes  = 30,
                                  sleep_interval,
                                  tz = "GMT")
{
  time_intervals <- difftime(date[idx_end], date[idx_start], units = "mins")

  pct_zero <- sapply(
    X = seq_along(idx_start),
    FUN = calculate_segment_mean,
    I(Y == 0),
    idx_start,
    idx_end
  )

  nopa0 <- round(pct_zero >= no_activity_cutoff)

  dat <- data.frame(
    sleep_onset  = date[idx_start],
    sleep_offset = date[idx_end],
    duration_min = time_intervals,
    pct_zero_activity = pct_zero,
    is_no_activity    = nopa0
  )

  # Merge consecutive NoPA segments
  cp_nopa0    <- rle(nopa0)
  n_cp_nopa   <- length(cp_nopa0$lengths)
  idx_end2    <- idx_end[cumsum(cp_nopa0$lengths)]
  idx_start2  <- c(1, idx_end2[-n_cp_nopa])

  t_int          <- difftime(date[idx_end2], date[idx_start2], units = "mins")
  pct_zero_nopa  <- sapply(
    X   = seq_len(n_cp_nopa),
    FUN = calculate_segment_mean,
    I(Y == 0),
    idx_start2,
    idx_end2
  )

  nopa         <- round(pct_zero_nopa >= no_activity_cutoff)
  sleep_nopa   <- round(t_int >= min_sleep_minutes) * round(pct_zero_nopa >= no_activity_cutoff)

  dat2 <- data.frame(
    sleep_onset         = date[idx_start2],
    sleep_offset        = date[idx_end2],
    duration_min        = t_int,
    pct_zero_activity   = pct_zero_nopa,
    no_activity_cutoff  = no_activity_cutoff,
    is_sleep            = sleep_nopa,
    is_no_activity      = nopa
  )

  # Compute overlap with external sleep interval
  int2        <- interval(sleep_interval$bed, sleep_interval$wake, tzone = tz)
  dat2$diary_overlap <- 0L
  for (j in seq_len(nrow(dat2))) {
    int1 <- interval(dat2[j, 1], dat2[j, 2], tzone = tz)
    dat2$diary_overlap[j] <- as.integer(!is.na(intersect(int1, int2)))
  }

  list(dat_long = dat, dat_short = dat2)
}

# Keep old name
#' @keywords internal
SearchSleepSeg <- search_sleep_segments


# ---- estimate_sleep (exported) ----------------------------------------------

#' Estimate daily sleep duration from accelerometer data
#'
#' @title Sleep duration estimation via Pruned Dynamic Programming
#'
#' @description Applies a Pruned Dynamic Programming (PDP) segmentation
#'   algorithm to wrist or hip accelerometer data to identify transitions
#'   between sleep and wakefulness.  The algorithm is described in Baek et al.
#'   (2021); this function wraps the full analysis pipeline including optional
#'   non-wear detection and diary-based validation.
#'
#' @details
#' **Algorithm overview**
#'
#' 1. A person-specific activity threshold is computed as the
#'    \code{threshold_pct}-th percentile of the activity counts.  Counts at
#'    or below this threshold are set to zero.
#' 2. The zeroed series is segmented into \eqn{K = \text{hours} \times
#'    \text{segments\_per\_hour}} pieces using the PDP changepoint algorithm
#'    with the selected cost model.
#' 3. Consecutive segments in which the proportion of zero-count epochs
#'    exceeds \code{no_activity_cutoff} are merged and labelled as potential
#'    sleep.
#' 4. Candidate sleep segments are validated against the supplied (or default)
#'    sleep window and filtered by minimum duration.
#'
#' **Cost models**
#'
#' | Value | Model | Best for |
#' |-------|-------|----------|
#' | \code{"poisson"} or \code{1} | Poisson | Non-negative integer counts |
#' | \code{"normal"} or \code{2} | Normal | Continuous, roughly symmetric data |
#' | \code{"negative_binomial"} or \code{3} | Negative binomial | Overdispersed integer counts |
#' | \code{"variance"} or \code{4} | Variance change | Constant-mean, changing variance |
#' | \code{"exponential"} or \code{5} | Exponential | Positive continuous waiting times |
#'
#' @param data Data frame of accelerometer data.  Must contain a \code{date}
#'   column (POSIXct or a character string parseable by
#'   \code{\link[lubridate]{ymd_hms}}) and a numeric activity column named
#'   by \code{activity_col}.
#' @param cost_model Cost function for the PDP algorithm.  Either a string
#'   (\code{"poisson"}, \code{"normal"}, \code{"negative_binomial"},
#'   \code{"variance"}, \code{"exponential"}) or the corresponding integer
#'   (\code{1}–\code{5}).  Default \code{"poisson"}.
#' @param subject_id Optional subject identifier stored in the result.
#' @param activity_col Name of the column containing the activity-count
#'   variable to segment (e.g. \code{"VM"} for vector magnitude).
#'   Default \code{"max_count"}.
#' @param threshold_pct Quantile (0–1) used to compute the person-specific
#'   activity threshold; counts at or below this quantile are set to zero
#'   before segmentation.  Default \code{0.4} (40th percentile).
#'   Use \code{0} to skip thresholding.
#' @param segments_per_hour Number of PDP segments allocated per hour of
#'   recording.  Higher values allow finer time resolution at the cost of
#'   computation.  Default \code{3}.
#' @param no_activity_cutoff Proportion (0–1) of zero-count epochs required
#'   within a segment to classify it as no physical activity (NoPA).
#'   Default \code{0.7}.
#' @param min_sleep_minutes Minimum duration (minutes) for a segment to
#'   qualify as sleep.  Default \code{20}.
#' @param min_wake_minutes Minimum duration (minutes) for a segment to
#'   qualify as wake.  Default \code{180}.
#' @param min_nap_minutes Sleep segments shorter than this (minutes) that do
#'   not overlap the diary window are excluded as naps.  Default \code{20}.
#' @param detect_nonwear Logical; if \code{TRUE} the
#'   \code{\link[accelerometry]{weartime}} algorithm is applied before
#'   segmentation.  Days with non-wear periods are flagged with
#'   \code{valid_accel = 0}.  Default \code{FALSE}.
#' @param min_wear_minutes Minimum number of wear minutes required when
#'   \code{detect_nonwear = TRUE}.  Default \code{120}.
#' @param use_diary Logical; if \code{TRUE} diary-based bed/wake times are
#'   used to validate sleep segments.  \code{diary} must be supplied.
#'   Default \code{FALSE}.
#' @param diary Data frame with columns \code{bed} and \code{wake} (POSIXct)
#'   giving the expected sleep window for each night.  Required when
#'   \code{use_diary = TRUE}.  One row per night.
#' @param default_bedtime  Default bed-time clock string used when
#'   \code{use_diary = FALSE}.  Default \code{"22:00:00"}.
#' @param default_waketime Default wake-time clock string used when
#'   \code{use_diary = FALSE}.  Default \code{"08:00:00"}.
#' @param tz Timezone string applied to all date-time operations.
#'   Default \code{"GMT"}.
#'
#' @return An object of class \code{actisleep_result} (a list) with two
#'   elements:
#'   \describe{
#'     \item{\code{segments}}{Data frame of estimated sleep segments.  Columns:
#'       \code{sleep_onset}, \code{sleep_offset} (character timestamps),
#'       \code{duration_min} (difftime),
#'       \code{pct_zero_activity} (proportion),
#'       \code{no_activity_cutoff},
#'       \code{is_sleep} (0/1),
#'       \code{is_no_activity} (0/1),
#'       \code{diary_overlap} (0/1),
#'       \code{diary_bedtime}, \code{diary_waketime} (POSIXct),
#'       \code{n_sleep_segments} (integer).}
#'     \item{\code{parameters}}{Named list recording the call parameters:
#'       \code{subject_id}, \code{cost_model}, \code{threshold_pct},
#'       \code{segments_per_hour}, \code{n_segments}, \code{valid_accel},
#'       \code{day_of_week}.}
#'   }
#'   Use \code{\link{as.data.frame.actisleep_result}} to extract the segments
#'   data frame directly.
#'
#' @references
#' Baek, J., Banker, M., Jansen, E. C., She, X., Peterson, K. E.,
#' Pitchford, E. A., & Song, P. X. K. (2021). An Efficient Segmentation
#' Algorithm to Estimate Sleep Duration from Actigraphy Data.
#' \emph{Statistics in Biosciences}.
#' \doi{10.1007/s12561-021-09309-3}
#'
#' @seealso \code{\link{read_agd}} for reading ActiGraph \code{.agd} files.
#'
#' @export
#'
#' @examples
#' data("AccelData")
#' # estimate_sleep() auto-detects common date formats
#' result <- estimate_sleep(AccelData, activity_col = "VM")
#' print(result)
#'
#' @examples
#' data("AccelData")
#' result <- estimate_sleep(AccelData, activity_col = "VM", detect_nonwear = TRUE)
#'
#' @examples
#' data("AccelData")
#' data("SleepDiary1Day")
#' # Build a diary data frame with bed and wake columns.
#' # estimate_sleep() auto-parses common character date-time formats.
#' diary <- data.frame(
#'   bed  = SleepDiary1Day$bed,
#'   wake = SleepDiary1Day$wake
#' )
#' result <- estimate_sleep(
#'   AccelData,
#'   cost_model         = "normal",
#'   activity_col       = "VM",
#'   threshold_pct      = 0,
#'   detect_nonwear     = TRUE,
#'   segments_per_hour  = 2,
#'   no_activity_cutoff = 0.45,
#'   min_sleep_minutes  = 5,
#'   use_diary          = TRUE,
#'   diary              = diary
#' )
#' print(result)
estimate_sleep <- function(
    data,
    cost_model         = "poisson",
    subject_id         = NA,
    activity_col       = "max_count",
    threshold_pct      = 0.4,
    segments_per_hour  = 3,
    no_activity_cutoff = 0.7,
    min_sleep_minutes  = 20,
    min_wake_minutes   = 180,
    min_nap_minutes    = 20,
    detect_nonwear     = FALSE,
    min_wear_minutes   = 120,
    use_diary          = FALSE,
    diary              = NULL,
    default_bedtime    = "22:00:00",
    default_waketime   = "08:00:00",
    tz                 = "GMT")
{
  # ---- Input validation -------------------------------------------------------

  data <- as.data.frame(data)

  if (nrow(data) == 0)
    stop("'data' is empty.")

  if (!"date" %in% names(data))
    stop("'data' must contain a column named 'date'.")

  if (!activity_col %in% names(data))
    stop(paste0("Activity column '", activity_col, "' not found in 'data'."))

  if (!is.numeric(threshold_pct) || threshold_pct < 0 || threshold_pct > 1)
    stop("'threshold_pct' must be a number between 0 and 1.")

  if (!is.numeric(segments_per_hour) || segments_per_hour <= 0)
    stop("'segments_per_hour' must be a positive number.")

  if (!is.numeric(no_activity_cutoff) || no_activity_cutoff < 0 || no_activity_cutoff > 1)
    stop("'no_activity_cutoff' must be a number between 0 and 1.")

  if (use_diary && is.null(diary))
    stop("'use_diary' is TRUE but 'diary' was not provided.")

  if (use_diary && !all(c("bed", "wake") %in% names(diary)))
    stop("'diary' must be a data frame with columns 'bed' and 'wake'.")

  # Auto-parse diary bed/wake columns if they are character strings
  if (use_diary && !is.null(diary)) {
    parse_col <- function(x, tz) {
      if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.POSIXct(x, tz = tz))
      # Try common formats
      fmts <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
                "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
                "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M")
      for (fmt in fmts) {
        candidate <- as.POSIXct(as.character(x), format = fmt, tz = tz)
        if (!all(is.na(candidate))) return(candidate)
      }
      as.POSIXct(as.character(x), tz = tz)
    }
    diary$bed  <- parse_col(diary$bed,  tz)
    diary$wake <- parse_col(diary$wake, tz)
  }

  # Resolve cost model: accept string or integer
  model_map <- c(
    poisson           = 1L,
    normal            = 2L,
    negative_binomial = 3L,
    variance          = 4L,
    exponential       = 5L
  )
  if (is.character(cost_model)) {
    cost_model_clean <- tolower(gsub("[[:space:]-]", "_", cost_model))
    if (!cost_model_clean %in% names(model_map))
      stop(paste0("'cost_model' must be one of: ",
                  paste(names(model_map), collapse = ", "), "."))
    f <- model_map[[cost_model_clean]]
  } else if (is.numeric(cost_model) || is.integer(cost_model)) {
    f <- as.integer(cost_model)
    if (!f %in% 1:5)
      stop("'cost_model' as integer must be 1 (Poisson), 2 (Normal), 3 (Negative binomial), 4 (Variance), or 5 (Exponential).")
  } else {
    stop("'cost_model' must be a character string or integer 1-5.")
  }

  # ---- Parse timestamps -------------------------------------------------------

  # Try several common formats automatically so users rarely need to pre-format.
  if (inherits(data$date, "POSIXct") || inherits(data$date, "POSIXlt")) {
    dates <- as.POSIXct(data$date, tz = tz)
  } else {
    # Ordered list of format strings to try
    formats_to_try <- c(
      "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
      "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
      "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
      "%Y%m%d%H%M%S"
    )
    dates <- NULL
    for (fmt in formats_to_try) {
      candidate <- as.POSIXct(data$date, format = fmt, tz = tz)
      if (!all(is.na(candidate))) {
        dates <- candidate
        break
      }
    }
    if (is.null(dates) || all(is.na(dates))) {
      # Last resort: lubridate's flexible parser
      dates <- ymd_hms(data$date, tz = tz, quiet = TRUE)
    }
  }
  dates <- with_tz(dates, tzone = tz)

  if (all(is.na(dates)))
    stop(paste0(
      "Could not parse the 'date' column. ",
      "Please convert it to POSIXct using as.POSIXct() or format_time() before calling estimate_sleep()."
    ))

  # Write parsed dates back into data so downstream helpers see POSIXct
  data$date <- dates

  # ---- Auto-aggregate sub-minute data ----------------------------------------

  epoch_secs <- as.numeric(difftime(dates[2], dates[1], units = "secs"))
  if (!is.na(epoch_secs) && epoch_secs < 60) {
    data  <- aggregate_epochs(data, cutnum = 1, unit = "min", tz = tz)
    dates <- as.POSIXct(data$date, tz = tz)
    dates <- with_tz(dates, tzone = tz)
  }

  # ---- Build sleep interval (diary or default) --------------------------------

  if (!use_diary) {
    sleep_interval <- get_sleep_interval(
      data,
      bed  = default_bedtime,
      wake = default_waketime,
      tz   = tz
    )
  } else {
    sleep_interval <- diary
  }

  if (nrow(sleep_interval) == 0)
    stop("No valid sleep interval could be constructed.")

  # ---- Non-wear detection -----------------------------------------------------

  valid_accel <- 1L
  if (detect_nonwear) {
    n_min   <- nrow(data)
    n_wear  <- sum(weartime(data[, activity_col], window = min_wear_minutes))
    if (n_min != n_wear)
      valid_accel <- 0L
  }

  # ---- Activity threshold and zeroing -----------------------------------------

  # Person-specific threshold at threshold_pct-th quantile
  t_threshold <- quantile(data[, activity_col], probs = threshold_pct)
  Y <- data[, activity_col] * round(data[, activity_col] > t_threshold)
  N <- length(Y)

  # ---- Number of segments K ---------------------------------------------------

  timelength <- as.numeric(difftime(dates[N], dates[1], units = "hours"))
  K          <- max(2L, as.integer(round(timelength * segments_per_hour)))

  # ---- PDP segmentation and sleep classification ------------------------------

  if (valid_accel == 1L) {
    # Scale data for Normal and Variance models to improve numerical stability
    if (f == 2L) {
      y_seg <- (Y - mean(Y)) / sd(Y)
    } else if (f == 4L) {
      v     <- c(0, diff(Y))
      y_seg <- (v - mean(v)) / sd(v)
    } else {
      y_seg <- Y
    }

    seg_out <- segmentor(data = y_seg, model = f, Kmax = K, compress = FALSE)

    # Extract breakpoints for the K-segment solution
    cpts      <- getBreaks(seg_out)[K, seq_len(K - 1)]
    idx_start <- unique(c(1L, cpts))
    idx_end   <- c(idx_start[-1], N)

    sleep_seg <- search_sleep_segments(
      date               = dates,
      Y                  = Y,
      idx_start          = idx_start,
      idx_end            = idx_end,
      no_activity_cutoff = no_activity_cutoff,
      min_sleep_minutes  = min_sleep_minutes,
      sleep_interval     = sleep_interval,
      tz                 = tz
    )

    # Separate overlapping and non-overlapping sleep candidates
    idx_sleep_overlap    <- which(
      sleep_seg$dat_short$diary_overlap == 1L &
        sleep_seg$dat_short$is_sleep == 1L
    )
    idx_sleep_nonoverlap <- which(
      sleep_seg$dat_short$diary_overlap == 0L &
        sleep_seg$dat_short$is_sleep == 1L &
        sleep_seg$dat_short$duration_min > min_nap_minutes
    )

    n_overlap    <- length(idx_sleep_overlap)
    n_nonoverlap <- length(idx_sleep_nonoverlap)

    if (n_overlap > 0) {
      seg_df <- sleep_seg$dat_short[idx_sleep_overlap, ]
    } else if (n_nonoverlap > 0) {
      seg_df     <- sleep_seg$dat_short[idx_sleep_nonoverlap, ]
      n_overlap  <- 0L
    } else {
      # No sleep found — return a single NA row
      seg_df <- data.frame(
        sleep_onset        = as.POSIXct(NA, tz = "GMT", origin = "1970-01-01"),
        sleep_offset       = as.POSIXct(NA, tz = "GMT", origin = "1970-01-01"),
        duration_min       = NA_real_,
        pct_zero_activity  = NA_real_,
        no_activity_cutoff = no_activity_cutoff,
        is_sleep           = NA_integer_,
        is_no_activity     = NA_integer_,
        diary_overlap      = NA_integer_
      )
      n_overlap <- 0L
    }

  } else {
    # Non-wear flagged — return a single NA row, K is not computed
    K <- NA_integer_
    seg_df <- data.frame(
      sleep_onset        = as.POSIXct(NA, tz = "GMT", origin = "1970-01-01"),
      sleep_offset       = as.POSIXct(NA, tz = "GMT", origin = "1970-01-01"),
      duration_min       = NA_real_,
      pct_zero_activity  = NA_real_,
      no_activity_cutoff = NA_real_,
      is_sleep           = NA_integer_,
      is_no_activity     = NA_integer_,
      diary_overlap      = NA_integer_
    )
    n_overlap <- NA_integer_
  }

  # ---- Attach diary columns and clean timestamps ------------------------------

  seg_df$diary_bedtime  <- sleep_interval$bed
  seg_df$diary_waketime <- sleep_interval$wake
  seg_df$n_sleep_segments <- n_overlap

  # Coerce POSIXct timestamps to character for portability
  seg_df$sleep_onset  <- as.character(seg_df$sleep_onset)
  seg_df$sleep_offset <- as.character(seg_df$sleep_offset)

  # Pad date-only strings with time component
  idx <- which(nchar(seg_df$sleep_onset)  == 10); seg_df$sleep_onset[idx]  <- paste(seg_df$sleep_onset[idx],  "00:00:00")
  idx <- which(nchar(seg_df$sleep_offset) == 10); seg_df$sleep_offset[idx] <- paste(seg_df$sleep_offset[idx], "00:00:00")

  # ---- Build result object ----------------------------------------------------

  params <- list(
    subject_id        = subject_id,
    cost_model        = names(model_map)[f],
    threshold_pct     = threshold_pct,
    segments_per_hour = segments_per_hour,
    n_segments        = K,
    valid_accel       = valid_accel,
    day_of_week       = as.character(wday(dates[1], label = TRUE))
  )

  new_actisleep_result(segments = seg_df, parameters = params)
}

# Keep old name as unexported alias for backward compatibility in tests
#' @keywords internal
SleepEstEachDay <- function(datain,
                             f = 1,
                             id = NA,
                             Y_name = "max_count",
                             T = 0.4,
                             nonwear_detect = FALSE,
                             wear_mins = 120,
                             S = 3,
                             NoPA_cut = 0.7,
                             sleep_mins = 20,
                             wake_mins = 180,
                             nap_mins = 20,
                             UseDiary = FALSE,
                             diary_data = NULL,
                             CommonBedTime = "22:00:00",
                             CommonWakeTime = "08:00:00",
                             tz = "GMT")
{
  # Map old diary format: if diary_data is a data frame without bed/wake names,
  # assume first two columns are bed and wake
  diary_arg <- diary_data
  if (!is.null(diary_data) && is.data.frame(diary_data)) {
    if (!all(c("bed", "wake") %in% names(diary_data))) {
      diary_arg <- data.frame(
        bed  = diary_data[, 1],
        wake = diary_data[, 2]
      )
    }
  }

  res <- estimate_sleep(
    data               = datain,
    cost_model         = f,
    subject_id         = id,
    activity_col       = Y_name,
    threshold_pct      = T,
    segments_per_hour  = S,
    no_activity_cutoff = NoPA_cut,
    min_sleep_minutes  = sleep_mins,
    min_wake_minutes   = wake_mins,
    min_nap_minutes    = nap_mins,
    detect_nonwear     = nonwear_detect,
    min_wear_minutes   = wear_mins,
    use_diary          = UseDiary,
    diary              = diary_arg,
    default_bedtime    = CommonBedTime,
    default_waketime   = CommonWakeTime,
    tz                 = tz
  )

  # Return in old format: list(summary = data.frame(...))
  seg <- res$segments
  p   <- res$parameters

  # Re-map column names to old names for internal backward-compatibility
  old_names <- c(
    sleep_onset        = "ts_start",
    sleep_offset       = "ts_end",
    duration_min       = "Intervals_mins",
    pct_zero_activity  = "PctZero",
    no_activity_cutoff = "NoPA_cut",
    is_sleep           = "Sleep",
    is_no_activity     = "NoPA",
    diary_overlap      = "overlap",
    diary_bedtime      = "SI_bed",
    diary_waketime     = "SI_wake",
    n_sleep_segments   = "n_sleep"
  )
  names(seg) <- ifelse(names(seg) %in% names(old_names),
                       old_names[names(seg)],
                       names(seg))

  summary_df <- cbind(
    data.frame(
      id          = p$subject_id,
      valid_accel = p$valid_accel,
      df          = p$n_segments,
      days        = p$day_of_week,
      stringsAsFactors = FALSE
    ),
    seg
  )

  list(summary = summary_df)
}

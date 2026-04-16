# ActiSleep

Estimate daily sleep duration from wrist or hip accelerometer data using a
**Pruned Dynamic Programming (PDP)** changepoint detection algorithm.

<!-- badges: start -->
<!-- badges: end -->

## Overview

ActiSleep applies the PDP segmentation algorithm described in
[Baek et al. (2021)](https://doi.org/10.1007/s12561-021-09309-3) to
minute-level accelerometer recordings.  The algorithm identifies transitions
between sleep and wakefulness by:

1. Computing a person-specific activity threshold (default: 40th percentile
   of counts) and zeroing counts at or below it.
2. Partitioning the zeroed series into *K* segments using PDP — an O(*nK*)
   dynamic-programming algorithm far more efficient than the naïve O(*n*²*K*)
   approach.
3. Merging consecutive low-activity segments and filtering by minimum
   duration to produce candidate sleep windows.
4. Validating candidates against an optional sleep diary or a default
   overnight window (10 pm – 8 am).

The package ships with five cost-function models (Poisson, Normal, Negative
binomial, Variance, Exponential) so the method can be tuned to different
activity-count distributions.

## Installation

```r
# CRAN (stable)
install.packages("ActiSleep")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("IrinaStatsLab/ActiSleep")
```

## Quick Start

```r
library(ActiSleep)

data("AccelData")

# Basic sleep estimation (no diary)
result <- estimate_sleep(AccelData, activity_col = "VM")
print(result)

# Extract the full segments data frame
df <- as.data.frame(result)
```

## Data

### Included datasets

| Dataset | Description |
|---------|-------------|
| `AccelData` | One day of 1-minute epoch accelerometer data for a single subject (columns: `date`, `VM`, `Steps`). |
| `SleepDiary1Day` | One-night sleep diary for a single subject (two character columns: bed time, wake time). |
| `SleepDiary1Week` | One-week sleep diary for a single subject (14 character columns alternating bed/wake). |

### AGD files

ActiGraph devices write proprietary `.agd` files (SQLite databases).
Use `read_agd()` to load them:

```r
agd  <- read_agd("subject01.agd")
accel <- agd$raw.data   # data frame with date, axis1, axis2, axis3, ...
```

### Diary data format

The `diary` argument to `estimate_sleep()` must be a `data.frame` with two
columns named `bed` and `wake` — one row per night.  The columns may be
`POSIXct` objects or character strings in any common date-time format
(`"YYYY-MM-DD HH:MM"`, `"MM/DD/YYYY HH:MM"`, etc.); `estimate_sleep()`
parses them automatically.

```r
diary <- data.frame(
  bed  = "2016-07-12 23:00",
  wake = "2016-07-13 09:30"
)
```

## Usage

### Basic usage

```r
library(ActiSleep)
data("AccelData")

result <- estimate_sleep(AccelData, activity_col = "VM")
print(result)
summary(result)
as.data.frame(result)
```

### With non-wear detection

```r
result <- estimate_sleep(
  AccelData,
  activity_col   = "VM",
  detect_nonwear = TRUE,
  min_wear_minutes = 120
)
print(result)
```

### With sleep diary

```r
data("SleepDiary1Day")
diary <- data.frame(
  bed  = SleepDiary1Day[, 1],
  wake = SleepDiary1Day[, 2]
)

result <- estimate_sleep(
  AccelData,
  activity_col       = "VM",
  cost_model         = "normal",
  threshold_pct      = 0,
  detect_nonwear     = TRUE,
  segments_per_hour  = 2,
  no_activity_cutoff = 0.45,
  min_sleep_minutes  = 5,
  use_diary          = TRUE,
  diary              = diary
)
print(result)
```

### Batch processing multiple subjects

```r
library(ActiSleep)

results <- lapply(subject_list, function(subj) {
  estimate_sleep(
    data         = subj$accel,
    subject_id   = subj$id,
    activity_col = "VM"
  )
})

# Combine all segments into one data frame
all_segments <- do.call(rbind, lapply(results, as.data.frame))
```

### Cost model guide

| `cost_model` | Distribution | Best for |
|--------------|-------------|----------|
| `"poisson"` (default) | Poisson | Non-negative integer counts (typical actigraphy) |
| `"normal"` | Normal | Continuous or approximately symmetric data |
| `"negative_binomial"` | Negative binomial | Overdispersed integer counts |
| `"variance"` | Variance change | Constant-mean series with changing variance |
| `"exponential"` | Exponential | Positive continuous inter-event times |

Integer codes 1–5 are also accepted for backward compatibility.

### Parameter tuning guide

| Parameter | Default | Effect |
|-----------|---------|--------|
| `threshold_pct` | 0.4 | Higher → more aggressive zeroing; lower → less aggressive |
| `segments_per_hour` | 3 | Higher → finer temporal resolution; lower → coarser |
| `no_activity_cutoff` | 0.7 | Proportion of zero epochs to label a segment "inactive" |
| `min_sleep_minutes` | 20 | Minimum duration to count as sleep |
| `min_nap_minutes` | 20 | Segments shorter than this outside the diary window are treated as naps |
| `default_bedtime` | `"22:00:00"` | Default overnight window start (no-diary mode) |
| `default_waketime` | `"08:00:00"` | Default overnight window end (no-diary mode) |

## Output

`estimate_sleep()` returns an `actisleep_result` object.  The three S3
methods are:

```r
print(result)          # formatted console summary
summary(result)        # aggregate statistics
as.data.frame(result)  # extract the segments data frame
```

### Segments data frame columns

| Column | Type | Description |
|--------|------|-------------|
| `sleep_onset` | character | Timestamp of segment start |
| `sleep_offset` | character | Timestamp of segment end |
| `duration_min` | difftime | Segment duration in minutes |
| `pct_zero_activity` | numeric | Proportion of zero-count epochs |
| `no_activity_cutoff` | numeric | Cutoff used for this run |
| `is_sleep` | integer | 1 if the segment is classified as sleep |
| `is_no_activity` | integer | 1 if the segment is classified as no physical activity |
| `diary_overlap` | integer | 1 if the segment overlaps the sleep diary window |
| `diary_bedtime` | POSIXct | Diary (or default) bed time for this night |
| `diary_waketime` | POSIXct | Diary (or default) wake time for this night |
| `n_sleep_segments` | integer | Number of sleep segments found this night |

## Algorithm

The Pruned Dynamic Programming (PDP) algorithm solves the penalised
segmentation problem:

> Find the *K*-segment partition of the series that minimises the total
> cost function plus a penalty proportional to the number of breakpoints.

PDP prunes the dynamic-programming recursion at each step by discarding
candidate breakpoints that cannot be optimal for any future position.
This reduces worst-case complexity from O(*n*²*K*) to O(*nK*).

See Baek et al. (2021) for full details.

## Setup

This project uses [renv](https://rstudio.github.io/renv/) for reproducible
package management.

```r
# Restore the project library from renv.lock
renv::restore()
```

## Project Structure

```
ActiSleep/
├── R/                    # R source files
│   ├── estimate_sleep.R  # Main exported function + internal helpers
│   ├── read_agd.R        # AGD file reader
│   ├── actisleep_result.R# S3 result class
│   ├── segmentor.R       # PDP segmentor (S4 class + R interface)
│   ├── segmentor_classes.R # S4 class definitions
│   └── ...
├── src/                  # C++ algorithm implementation (Rcpp)
│   ├── rcpp_segmentor.cpp# Rcpp wrappers for CallSegmentor
│   ├── CallSegmentor.*   # C++ PDP entry points
│   ├── Segmentor.*       # Core PDP algorithm
│   └── ...
├── data/                 # Included datasets (.rda)
├── man/                  # Auto-generated roxygen documentation
├── tests/testthat/       # Unit and regression tests
├── vignettes/            # Tutorial vignette
└── DESCRIPTION
```

## References

Baek, J., Banker, M., Jansen, E. C., She, X., Peterson, K. E.,
Pitchford, E. A., & Song, P. X. K. (2021). An efficient segmentation
algorithm to estimate sleep duration from actigraphy data.
*Statistics in Biosciences*.
<https://doi.org/10.1007/s12561-021-09309-3>

## License

GPL-3 © University of Michigan

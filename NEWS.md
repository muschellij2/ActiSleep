# ActiSleep 1.0.0

## CRAN submission preparation

* Reviewed package metadata and dependency declarations for CRAN submission.
* Added a generated SQLite fixture test for `read_agd()` so file parsing,
  timestamp construction, and output columns are tested without committing a
  device-specific AGD file.
* Removed `accelerometry` from runtime imports. With the existing `weartime()`
  settings (`tol = 0`, `tol_upper = 99`, `nci = FALSE`,
  `days_distinct = FALSE`), only full runs of zero-count epochs can be
  non-wear, so a base R run-length classifier now produces the same epoch
  mask. `accelerometry` remains a suggested package for reference tests.
* Corrected the `min_wear_minutes` documentation: this value sets the
  classifier's minimum zero-run window; it is not a minimum total-wear
  duration.
* Fixed `BestSegmentation()` when `K` exceeds the original `Kmax`; the
  compressed data length is now set before combining forward and reverse
  costs.
* Registered the existing `Segmentor` print method for S3 dispatch.
* Added regression tests comparing full wear masks against `weartime()` for
  several window lengths and boundary cases, as well as tests for sleep
  interval helpers, segment classification, and segmentor validation and
  model selection.

This release is a comprehensive overhaul introducing a modernised C++ interface,
a cleaner two-function API, snake_case naming throughout, an S3 result class,
and a full testthat suite.  It is a **breaking change** from v0.2.2.

## Breaking changes

* `SleepEstEachDay()` is replaced by `estimate_sleep()` with renamed parameters
  (see *API changes* below).  An unexported `SleepEstEachDay()` backward-
  compatibility shim is provided for internal tests only.
* The `format_time()` / `ChangeTimeVar()` pre-processing step is no longer
  needed; `estimate_sleep()` auto-detects and parses common date-time formats.
* The `tibble`-based `read_agd()` return value is now a plain `data.frame`.
* Dependencies `dplyr`, `tibble`, `stringr`, and `lazyeval` have been removed.

## New features

* **`estimate_sleep()`** — primary exported function.  Improvements over the
  old `SleepEstEachDay()`:
  - Accepts `cost_model` as a string (`"poisson"`, `"normal"`,
    `"negative_binomial"`, `"variance"`, `"exponential"`) or integer (1–5).
  - Auto-detects and parses `date` column formats
    (`YYYY-MM-DD HH:MM:SS`, `MM/DD/YYYY HH:MM`, etc.) — no separate
    `format_time()` call required.
  - Auto-aggregates sub-minute epoch data to 1-minute intervals.
  - Auto-parses diary `bed` / `wake` columns from character strings.
  - Returns an `actisleep_result` S3 object with `print()`, `summary()`,
    and `as.data.frame()` methods.
  - Full input validation with informative error messages.
* **`actisleep_result` S3 class** — structured return object with:
  - `print.actisleep_result()` — formatted per-segment console output.
  - `summary.actisleep_result()` — aggregate statistics (total sleep,
    segment count, mean zero-epoch proportion).
  - `as.data.frame.actisleep_result()` — extract the segments data frame.
* **Rcpp C++ interface** — replaced the fragile `.C()` / `extern "C"` /
  `init.cpp` pattern with `[[Rcpp::export]]` wrappers.  Symbol registration
  is now handled automatically; the manual `src/init.cpp` and `src/PrunedSeg.cc`
  shims have been removed.

## API changes

| v0.2.2 parameter | v1.0.0 parameter |
|-----------------|-----------------|
| `datain` | `data` |
| `f` | `cost_model` |
| `id` | `subject_id` |
| `Y_name` | `activity_col` |
| `T` | `threshold_pct` |
| `S` | `segments_per_hour` |
| `NoPA_cut` | `no_activity_cutoff` |
| `sleep_mins` | `min_sleep_minutes` |
| `wake_mins` | `min_wake_minutes` |
| `wear_mins` | `min_wear_minutes` |
| `nap_mins` | `min_nap_minutes` |
| `UseDiary` | `use_diary` |
| `diary_data` | `diary` |
| `CommonBedTime` | `default_bedtime` |
| `CommonWakeTime` | `default_waketime` |
| `nonwear_detect` | `detect_nonwear` |

## Output column renames (`segments` data frame)

| v0.2.2 column | v1.0.0 column |
|--------------|--------------|
| `ts_start` | `sleep_onset` |
| `ts_end` | `sleep_offset` |
| `Intervals_mins` | `duration_min` |
| `PctZero` | `pct_zero_activity` |
| `NoPA_cut` | `no_activity_cutoff` |
| `Sleep` | `is_sleep` |
| `NoPA` | `is_no_activity` |
| `overlap` | `diary_overlap` |
| `SI_bed` | `diary_bedtime` |
| `SI_wake` | `diary_waketime` |
| `n_sleep` | `n_sleep_segments` |

## Bug fixes

* **`init.cpp` symbol registration** — all 10 C++ function declarations were
  commented out, causing a "symbol not found in flat namespace" crash.  Fixed
  (Phase 1) and subsequently replaced by Rcpp auto-registration (Phase 2).
* **`aggregate_epochs()` standalone crash** — `cut(dates, ...)` referenced an
  undefined variable `dates`; changed to `cut(datain$date, ...)`.
* **Unnamed `data.frame()` columns** — `ts_start <- as.POSIXct(...)` inside
  `data.frame()` produced unnamed columns; changed `<-` to `=`.
* **`select_model()` Variance branch** — `1:K` referenced an undefined `K`;
  changed to `1:Kmax`.  Same fix applied to the Poisson BIC/AIC branch.
* **S4 `show()` model name mismatches** — `"Variance"` corrected to
  `"Variance Segmentation"` and `"Negative Binomial"` to `"Negative binomial"`.
* **`read_agd()` global assignment** — `ad_set <<- ad_set` wrote to the global
  environment (CRAN policy violation); removed.
* **`get_sleep_interval()` overnight window** — the old code grouped timestamps
  by full datetime string, producing incorrect bed/wake anchor dates.  Fixed by
  grouping on calendar date (`format(date_formatted, "%Y-%m-%d")`), which
  correctly constructs the overnight window (e.g. 2016-07-12 22:00 –
  2016-07-13 08:00).  This is a **correctness improvement**: the new code
  returns 3 sleep segments for the included `AccelData` example (37, 142, and
  314 min), excluding two post-wake segments (29 and 25 min) that the old code
  incorrectly included due to date-parsing failures.
* **Removed debug print statements** — `print(head(dates))`,
  `print("Flag A")`, `print("Flag B")`, and `print(sleep_tab)` deleted.

## Internal changes

* R source files renamed to snake_case (`estimate_sleep.R`, `segmentor.R`,
  `segmentor_classes.R`, etc.).
* All internal functions renamed to snake_case with old-name aliases retained
  for internal callers.
* `R/Global.R` deleted (dplyr global variable declarations no longer needed).
* `R/SegmentorInstall.r` replaced by `R/zzz.R` with a proper `.onAttach()`
  startup message.
* `src/Makevars` added (`CXX_STD = CXX11`).
* `src/RcppExports.cpp` / `R/RcppExports.R` auto-generated by
  `Rcpp::compileAttributes()`.

---

# ActiSleep 0.2.2

Initial CRAN release.

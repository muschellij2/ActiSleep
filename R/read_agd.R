#' @title Read AGD file data
#'
#' @description Read an ActiGraph .agd file (SQLite format) and return
#'   device settings and raw accelerometer data.
#'
#' @param file Path to an AGD data file.
#' @param tz Timezone string, default \code{"GMT"}.
#' @param sec Epoch length in seconds used to construct timestamps, default 10.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{settings}{Data frame of device settings metadata.}
#'     \item{raw.data}{Data frame with columns \code{date} and all
#'       accelerometer channels (axis1, axis2, axis3, steps, lux, etc.).}
#'   }
#' @export
#'
read_agd <- function(file, tz = "GMT", sec = 10)
{
  con <- dbConnect(SQLite(), file)
  on.exit(dbDisconnect(con), add = TRUE)

  ad_set <- dbReadTable(con, "settings")
  raw_start <- ad_set$settingValue[ad_set$settingName == "startdatetime"]
  origen <- as.POSIXlt(
    (as.numeric(raw_start) / 1e+07),
    origin = "0001-01-01 00:00:00",
    tz = "GMT")
  origen <- as.POSIXct(as.character(origen), tz = tz)
  longitud <- ad_set$settingValue[ad_set$settingName == "epochcount"]
  longitud <- as.numeric(longitud)
  base <- dbReadTable(con, "data")
  date <- origen + seq(from = 0, by = sec, length.out = longitud)
  base$date <- date
  # Move date to first column; drop internal dataTimestamp column
  keep_cols <- c("date", setdiff(names(base), c("date", "dataTimestamp")))
  base <- base[, keep_cols]
  ad_set <- data.frame(ad_set)
  list(settings = ad_set, raw.data = base)
}

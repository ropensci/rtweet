#' round_time
#'
#' Aggregates POSIXct object
#'
#' @param x Vector of class POSIXct.
#' @param interval Amount, in seconds, of aggregated window of time.
#' @param center Logical indicating whether to center datetime value at interval
#'   midpoint.
#' @export
round_time <- function(x, interval = 60, center = TRUE) {
  stopifnot(inherits(x, "POSIXct"))
  ## round off to lowest value
  rounded <- floor(as.numeric(x) / interval) * interval
  if (center) {
    ## center so value is interval mid-point
    rounded <- rounded + round(interval * .5, 0)
  }
  ## return to date-time
  as.POSIXct(rounded, origin = "1970-01-01")
}

#' ts_filter
#'
#' Converts text-level observations to time aggregated frequency data
#'   frame with [optional] filtered dummy variable(s).
#'
#' @param rt Tweets or users data frame. Technically, this argument
#'   will accept any recursive object (i.e., list or data frame)
#'   containing a named date-time (POSIXt) element or column. By
#'   default, \code{ts_plot} assumes the date-time variable is
#'   labeled "created_at", which is the default date-time label used
#'   in tweets data. However, this function should work with any
#'   data source, assuming it meets the (a) POSIXt class requirement
#'   and (b) the date-time variable is given the appropriate name
#'   (if not "created_at" then a label specified with the
#'   \code{dtname} argument).
#' @param by Unit of time, e.g., \code{secs, days, weeks,
#'   months, years} by which to aggregate observations. By default,
#'   \code{ts_plot} tries to aggregate time by "days", but for some
#'   high-frequency data sets that only span a matter of minutes or
#'   hours, this is likely to either produce an error or a truly
#'   disappointing plot. In these cases, users are encouraged to
#'   explore smaller units of time. Conversely, high-frequency and
#'   long [in duration] data sets may be difficult to read given the
#'   default unit of time. In these cases, users should try larger
#'   units of time, e.g., "weeks" or "months". This parameter will
#'   also accept numeric quantifiers in addition to units of time.
#'   By default, for example, the provided unit of time is
#'   assumed to specify whole (1) units of time. It is posible to
#'   tweak this unit by specifying the number (or fraction) of time
#'   units, e.g., \code{by = "2 weeks"}, \code{by = "30 secs"},
#'   \code{by = ".333 days"}.
#' @param dtname Name of date-time (POSIXt) column (if data frame)
#'   or element (if list). Defaults to "created_at", the default
#'   label supplied as a timestamp variable for tweets data. This
#'   function is exportable to non-Twitter data, assuming the
#'   intended data object includes a date-time variable with the
#'   same label that's supplied to the \code{dtname} parameter.
#' @param txt Name of distinguishing variable in data frame or list
#'   to which filter is applied. Defaults to text.
#' @param filter Vector of regular expressions with which to
#'   filter data (creating multiple time series).
#' @param key Optional provide pretty labels for filters.
#'   Defaults to actual filters.
#' @param na.omit Logical indicating whether to omit rows with
#'   missing (NA) values for the dtname variable. Defaults to TRUE.
#'   If FALSE and data contains missing values for the date-time
#'   variable, an error will be returned to the user.
#' @param trim Logical indicating whether to trim extreme intervals,
#'   which often capture artificially lower frequencies. Defaults to
#'   FALSE.
#' @export
ts_filter <- function(rt, by = "days",
                      dtname = "created_at",
                      txt = "text",
                      filter = NULL,
                      key = NULL,
                      na.omit = TRUE,
                      trim = FALSE) {
  warning("ts_filter is deprecated. use ts_data instead", call. = FALSE)
  .ts_filter(rt = rt, by = by, dtname = dtname,
             txt = txt, filter = filter, key = key,
             na.omit = na.omit, trim = trim)
}

.ts_filter <- function(rt, by,
                       dtname,
                       txt,
                       filter,
                       key,
                       na.omit,
                       trim) {

  ## handle missing data
  if (is.data.frame(rt)) {
    rt <- rt[!is.na(rt[[dtname]]), ]
  } else if (is.list(rt)) {
    notmissing <- !is.na(rt[[dtname]])
    rt[c(dtname, txt)] <- lapply(
      rt[c(dtname, txt)], function(x) x[[notmissing]])
  }

  ## make sure date variable is of class POSIXt
  if (!any(grepl("POSIX", class(rt[[dtname]])))) {
    rt[[dtname]] <- as.POSIXct(rt[[dtname]]) %>%
      tryCatch(error = return(NULL))
    if (is.null(rt[[dtname]])) {
      stop(paste0("the ", dtname,
                  " variable must be of class POSIXt"),
           call. = FALSE)
    }
  }

  ## if no filter provided
  if (any(is.null(filter), identical(filter, ""))) {
    ## if no filters then simple
    dat <- sollts(rt[[dtname]], by = by)
    dat$filter <- ""
    if (trim) {
      dat <- dat[-c(1, nrow(dat)), ]
    }
  } else {
    ## flag filtered obs
    f.rows <- lapply(
      filter, function(x)
        grepl(x, rt[[txt]], ignore.case = TRUE))
    f.rows[filter == ""] <- rep(TRUE, NROW(rt))
    ## count obs for each filter
    lens <- vapply(f.rows, sum, double(1))
    ## if nil use vector of unique datetimes instead
    if (any(lens == 0)) {
      f.rows[lens == 0] <- sum(lens == 0) %>%
        seq_len %>%
        lapply(function(x)
          which(!duplicated(rt[[dtname]])))
    }
    ## iterate thru each filter for timeseries data
    lstdat <- lapply(f.rows, function(x)
      sollts(rt[[dtname]][unlist(x)],
             by = by,
             fdt = rt[[dtname]]))
    ## hollow out (set freqs to 0) any nil filters
    if (any(lens == 0)) {
      for (i in seq_len(sum(lens == 0))) {
        lstdat[lens == 0][[i]]$freq <- 0
      }
    }
    ## if no pretty label provided then use filter
    if (is.null(key)) {
      key <- filter
      ## if complex expression include up to 1st bar
      key <- gsub("[|].*", "", key)
    }
    ## add variable name for each filter
    for (i in seq_along(lstdat)) {
      lstdat[[i]]$filter <- key[i]
      if (trim) {
        lstdat[[i]] <- lstdat[[i]][-c(1, nrow(lstdat)), ]
      }
    }
    ## collapse into tidy data frame
    dat <- do.call("rbind", lstdat)
  }
  if (requireNamespace("tibble", quietly = TRUE)) {
    dat <- tryCatch(tibble::as_tibble(dat),
                    error = function(e) return(dat))
  }
  dat
}

sollts <- function(dt, by = "days", fdt = NULL) {
  ## convert time unit to double
  if (grepl("month", by)) .unit <- 2592000
  if (grepl("week", by)) .unit <- 604800
  if (grepl("day", by)) .unit <- 86400
  if (grepl("hour", by)) .unit <- 3600
  if (grepl("min", by)) .unit <- 60
  if (grepl("sec", by)) .unit <- 1
  ## parse out numeric multiplier
  x <- gsub("[^[:digit:]|.]", "", by) %>%
    as.double()
  ## if not multiplier, set to 1
  if (any(is.na(x), identical(x, ""))) x <- 1
  ## calculate desired unit of time
  .unit <- .unit * x
  ## aggregate by desired unit
  rdt <- as.POSIXct(
    round(as.double(dt) / .unit, 0) * .unit,
    origin = "1970-01-01")
  ## observation for each unit of time
  if (!is.null(fdt)) {
    ## if nested in potentially wider range
    fdt <- as.POSIXct(
      round(as.double(fdt) / .unit, 0) * .unit,
      origin = "1970-01-01")
    time <- seq(min(fdt), max(fdt), .unit)
  } else {
    ## if approp date range contained within data
    time <- seq(min(rdt, na.rm = TRUE),
                max(rdt, na.rm = TRUE), .unit)
  }
  ## create freq table
  tab <- as.data.frame(table(rdt), stringsAsFactors = FALSE)
  ## set class and var names
  tab <- data.frame(
    time = as.POSIXct(tab$rdt),
    freq = tab$Freq)
  ## merge with all observations
  tab <- merge(data.frame(time = time), tab, all = TRUE)
  ## replace NAs with 0s
  tab$freq[is.na(tab$freq)] <- 0
  tab
}

#' @importFrom grDevices hcl
rt_cols <- function(n, lighter = FALSE) {
  if (n < 4) {
    if (lighter) {
      cols <- c("#4a7aee", "#dd5a5a", "#33bb33")
    } else {
      cols <- c("#003366", "#bb2222", "#008800")
    }
    return(sample(cols, n))
  }
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 58, c = 100)[1:n]
}


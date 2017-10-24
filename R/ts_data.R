
#' ts data
#'
#' Returns time series-like data frame.
#'
#' @param data Data frame or grouped data frame.
#' @param by Desired interval of time expressed as numeral plus secs, mins,
#'   hours, days, weeks, months, years. If a numeric is provided, the value
#'   is assumed to be in seconds.
#' @param group Name of grouping variable to construct multiple time series,
#'   which still returns a data frame but it includes the group variable as
#'   a named column.
#' @param \dots Passed along to trim_time. Most likely used to specify timezone.
#' @return Data frame with time, n, and grouping column if applicable.
#' @export
ts_data <- function(data, by = "days", group = NULL, ...) {
  do.call("ts_data_", list(data = data, by = by, group = group, ...))
}


ts_data_ <- function(data, by = "days", group = NULL, ...) {
  if (inherits(data, "grouped_df")) {
    indices <- attr(data, "indices")
    group_vars <- paste0("g", seq_along(indices))
    data <- lapply(indices, function(x) data[x, ])
    for (i in seq_along(data)) {
      if (nrow(data[[i]]) > 0L) {
        data[[i]]$group_var <- group_vars[i]
      }
    }
    data <- do.call("rbind", data)
    group <- "group_var"
  }
  if (!is.null(group)) {
    groupfun(data, group, aggregate_time, by, ...)
  } else {
    aggregate_time(data, by, ...)
  }
}



#' apply functions to data frame by group
#'
#' @param data Data frame.
#' @param group Name of grouping variable.
#' @param f Function to be applied to data frame for each group.
#' @param \dots Args passed along to function.
#' @noRd
groupfun <- function(data, group, f, ...) {
  data <- lapply(
    unique(data[[group]]),
    function(x) {
      dg <- f(data[data[[group]] == x, ], ...)
      dg[[group]] <- x
      dg
    })
  tibble::as_tibble(do.call("rbind", data))
}

groupsfun <- function(data, groups, f, ...) {
  data <- lapply(groups, function(x) groupfun(data, x, f, ...))
  for (i in seq_along(data)) {
    nacols <- vapply(groups, function(x) has_name_(data[[i]], x), FUN.VALUE = logical(1))
    if (sum(nacols) > 0L) {
      for (j in seq_along(nacols)) {
        data[[i]][[j]] <- NA_character_
      }
    }
  }
  tibble::as_tibble(do.call("rbind", data))
}


#' aggregate (count) tweets by time
#'
#' @param dt A date-time vector of class POSIXct. To be consistent with older versions,
#'   if a data frame is provided, this function looks for a column named "created_at".
#' @param by Length of time interval expressed in years, months, weeks, days, hours,
#'   minutes, or seconds. Can include numeric specifier with unit, e.g., 2.5 months.
#'   If a numeric is provided, it assumes you have specified the desired interval of
#'   time interval in seconds.
#' @param trim Logical indicating whether to round off the unit of time, e.g., days at
#'   0 hours, minutes at 0 secds, etc.
#' @param \dots Args passed to trim_time. Most likely this is used to specify time zone.
#' @return Data frame of time and n (like a frequency table).
#' @noRd
aggregate_time <- function(dt, by = "days", trim = TRUE, ...) {
  if (all(is.data.frame(dt), isTRUE("created_at" %in% names(dt)))) {
    dt <- dt[["created_at"]]
  }
  stopifnot(inherits(dt, "POSIXct"), is.atomic(by))
  dt <- sort(na_omit(dt))
  .unit <- parse_unit(by)

  dt <- round_time(dt, by)
  dt <- trim_time(dt, by, ...)
  dtb <- table(dt)

  df <- data.frame(
    time = unique(dt),
    n = as.integer(dtb)
  )

  df2 <- data.frame(
    time = unique(trim_time(seq(dt[1], dt[length(dt)], .unit), by)),
    n = 0L
  )

  if (any(!df2$time %in% df$time)) {
    df <- rbind(df, df2[!df2$time %in% df$time, ])
  }
  df <- df[order(df$time), ]
  row.names(df) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    df <- tibble::as_tibble(df)
  }
  df
}



trim_time <- function(dt, by, tz = "UTC", ...) {
  if (grepl("year", by)) {
    as.POSIXct(paste0(substr(dt, 1, 5), "01-01 00:00:00"), tz = tz, ...)
  } else if (grepl("month", by)) {
    as.POSIXct(paste0(substr(dt, 1, 8),    "01 00:00:00"), tz = tz, ...)
  } else if (grepl("week|day", by)) {
    as.POSIXct(paste0(substr(dt, 1, 12),      "00:00:00"), tz = tz, ...)
  } else if (grepl("hour", by)) {
    as.POSIXct(paste0(substr(dt, 1, 15),         "00:00"), tz = tz, ...)
  } else if (grepl("min", by)) {
    as.POSIXct(paste0(substr(dt, 1, 18),            "00"), tz = tz, ...)
  } else {
    dt
  }
}

parse_unit <- function(by) {
  stopifnot(is.atomic(by))
  if (is.numeric(by)) {
    n <- by
  } else if (grepl("year", by)) {
    n <- 60 * 60 * 24 * 365
  } else if (grepl("month", by)) {
    n <- 60 * 60 * 24 * 30
  } else if (grepl("week", by)) {
    n <- 60 * 60 * 24 * 7
  } else if (grepl("day", by)) {
    n <- 60 * 60 * 24
  } else if (grepl("hour", by)) {
    n <- 60 * 60
  } else if (grepl("min", by)) {
    n <- 60
  } else if (grepl("sec", by)) {
    n <- 1
  } else {
    stop("must express time interval in secs, mins, hours, days, weeks, months, or years",
         call. = FALSE)
  }
  x <- as.double(gsub("[^[:digit:]|\\.]", "", by))
  if (any(is.na(x), identical(x, ""))) {
    x <- 1
  }
  n * x
}


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
  ## parse interval
  interval <- parse_unit(interval)
  ## round off to lowest value
  rounded <- floor(as.numeric(x) / interval) * interval
  if (center) {
    ## center so value is interval mid-point
    rounded <- rounded + round(interval * .5, 0)
  }
  ## return to date-time
  as.POSIXct(rounded, origin = "1970-01-01")
}


#' trim_ts
#'
#' @param x Time series Twitter data.
#' @param group Name of grouping variable. Default (NULL) will use
#'   variable of grouped data frames or look for variables labelled
#'   "group" or "filter". Set this to FALSE to override these defaults.
#' @return Trimmed data frame
#' @noRd
trim_ts <- function(x, group = NULL) {
  if (all(is.null(group), inherits(x, "grouped_df"))) {
    group <- attr(x, "vars")
  } else if (all(is.null(group), "group" %in% names(x))) {
    group <- "group"
  } else if (all(is.null(group), "filter" %in% names(x))) {
    group <- "filter"
  }
  if (any(is.null(group), identical(group, FALSE))) {
    n <- nrow(x)
  } else {
    n <- sum(x[[group]] == x[[group]][1])
  }
  nope <- c(seq(1, nrow(x), n), seq(n, nrow(x), n))
  x <- x[-nope, ]
  row.names(x) <- NULL
  x
}

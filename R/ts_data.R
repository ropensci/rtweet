

#' ts plot
#'
#' Returns time series-like data frame.
#'
#' @param data Data frame or grouped data frame.
#' @param by Desired interval of time expressed as numeral plus secs,
#'   mins, hours, days, weeks, months, years. If a numeric is
#'   provided, the value is assumed to be in seconds.
#' @param ... For info on all possible arguments see
#'   \code{\link{ts_plot.default}}.
#' @return Data frame with time, n, and grouping column if applicable.
#' @importFrom graphics legend
#' @export
ts_plot <- function(data, by, ...) {
  UseMethod("ts_plot")
}


#' ts_plot.default
#'
#' Returns time series-like data frame.
#'
#' @param data Data frame or grouped data frame.
#' @param by Desired interval of time expressed as numeral plus secs,
#'   mins, hours, days, weeks, months, years. If a numeric is
#'   provided, the value is assumed to be in seconds.
#' @param group Name of grouping variable to construct multiple time
#'   series, which still returns a data frame but it includes the
#'   group variable as a named column.
#' @param \dots Passed along to trim_time. Most likely used to specify
#'   timezone.
#' @return Data frame with time, n, and grouping column if applicable.
#' @importFrom graphics legend
#' @export
ts_plot.default <- function(data, by = "days", group = NULL, ...) {
  data <- ts_data(data, by, group = group, ...)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    if (ncol(data) == 3) {
      ggplot2::ggplot(data,
                      ggplot2::aes_string(x = "time", y = "n",
                                          colour = names(data)[3])) +
        ggplot2::geom_line(...)
    } else {
      ggplot2::ggplot(data, ggplot2::aes_string(x = "time", y = "n")) +
        ggplot2::geom_line(...)
    }
  } else {
    if (ncol(data) == 3) {
      cols <- ggcols(length(unique(data[[3]])))
      with(data, plot(time, n, col = cols, type = "n"))
      invisible(lapply(
        seq_along(unique(data[[3]])), function(i)
          with(data[data[[names(data)[3]]] == unique(data[[3]])[i], ],
               lines(time, n, col = cols[i]))
      ))
      legend("topleft", unique(data[[3]]), bty = "n", inset = 0,
             col = cols, lwd = rep(1, length(unique(data[[3]]))))
    } else {
      with(data, plot(time, n, ..., type = "l"))
    }
  }
}


ggcols <- function (n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
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
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(do.call("rbind", data))
  } else {
    do.call("rbind", data)
  }
}

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
ts_data <- function(data, by, ...) {
  UseMethod("ts_data")
}

ts_data <- function(data, by = "days", group = NULL, ...) {
  if (inherits(data, "grouped_df")) {
    group <- names(attr(data, "labels"))
    data <- data.frame(data)
  }
  if (!is.null(group)) {
    groupfun(data, group, aggregate_time, by, ...)
  } else {
    aggregate_time(data, by, ...)
  }
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
aggregate_time <- function (dt, by = "days", trim = TRUE, ...) {
  if (all(is.data.frame(dt), isTRUE("created_at" %in% names(dt)))) {
    dt <- dt[["created_at"]]
  }
  stopifnot(inherits(dt, "POSIXct"), is.atomic(by))
  dt <- sort(na_omit(dt))
  .unit <- parse_unit(by)

  dt <- round_time(dt, .unit)
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


#' Plots tweets data as a time series-like data object.
#'
#' Creates a ggplot2 plot of the frequency of tweets over a specified
#' interval of time.
#'
#' @param data Data frame or grouped data frame.
#' @param by Desired interval of time expressed as numeral plus one of
#'   "secs", "mins", "hours", "days", "weeks", "months", or
#'   "years". If a numeric is provided, the value is assumed to be in
#'   seconds.
#' @param trim The number of observations to drop off the beginning
#'   and end of the time series.
#' @param tz Time zone to be used, defaults to "UTC" (Twitter default)
#' @param ... Other arguments passed to
#'   \code{\link[ggplot2]{geom_line}}.
#' @return If
#'   \href{https://cran.r-project.org/package=ggplot2}{ggplot2} is
#'   installed then a \code{\link[ggplot2]{ggplot}} plot object.
#' @examples
#'
#' \dontrun{
#'
#' ## search for tweets containing "rstats"
#' rt <- search_tweets("rstats", n = 10000)
#'
#' ## plot frequency in 1 min intervals
#' ts_plot(rt, "mins")
#'
#' ## plot multiple time series--retweets vs non-retweets
#' rt %>%
#'   dplyr::group_by(is_retweet) %>%
#'   ts_plot("hours")
#'
#' ## compare account activity for some important US political figures
#' tmls <- get_timeline(
#'   c("SenSchumer", "SenGillibrand", "realDonaldTrump"),
#'   n = 3000
#' )
#'
#' ## examine all twitter activity using weekly intervals
#' ts_plot(tmls, "weeks")
#'
#' ## group by screen name and plot each time series
#' ts_plot(dplyr::group_by(tmls, screen_name), "weeks")
#'
#' ## group by screen name and is_retweet
#' tmls %>%
#'   dplyr::group_by(tmls, screen_name, is_retweet) %>%
#'   ts_plot("months")
#'
#' }
#' @family ts_data
#' @export
ts_plot <- function(data, by = "days", trim = 0L, tz ="UTC", ...) {
  do.call("ts_plot_", list(data = data, by = by, trim = trim, tz = tz, ...))
}


#' @importFrom graphics legend
ts_plot_ <- function(data, by = "days", trim = 0L, tz ="UTC", ...) {
  data <- ts_data(data, by, trim, tz)
  try_require("ggplot2")
  if (ncol(data) == 3L) {
    ggplot2::ggplot(
      data, ggplot2::aes_string(
        x = "time", y = "n", colour = names(data)[3])
    ) +
    ggplot2::geom_line(...)
  } else if (ncol(data) == 4L) {
    ggplot2::ggplot(
      data, ggplot2::aes_string(
        x = "time", y = "n", colour = names(data)[3], linetype = names(data)[4])
    ) +
    ggplot2::geom_line(...)
  } else {
    ggplot2::ggplot(
      data, ggplot2::aes_string(x = "time", y = "n")) +
      ggplot2::geom_line(...)
  }
}


#' Converts tweets data into time series-like data object.
#'
#' Returns data containing the frequency of tweets over a specified
#' interval of time.
#'
#' @param data Data frame or grouped data frame.
#' @param by Desired interval of time expressed as numeral plus one of
#'   "secs", "mins", "hours", "days", "weeks", "months", or
#'   "years". If a numeric is provided, the value is assumed to be in
#'   seconds.
#' @param trim Number of observations to trim off the front and end of
#'   each time series
#' @param tz Time zone to be used, defaults to "UTC" (Twitter default)
#' @return Data frame with time, n, and grouping column if applicable.
#' @examples
#'
#' \dontrun{
#'
#' ## handles of women senators
#' sens <- c("SenatorBaldwin", "SenGillibrand", "PattyMurray", "SenatorHeitkamp")
#'
#' ## get timelines for each
#' sens <- get_timeline(sens, n = 3200)
#'
#' ## get single time series for tweets
#' ts_data(sens)
#'
#' ## using weekly intervals
#' ts_data(sens, "weeks")
#'
#' ## group by screen name and then use weekly intervals
#' sens %>%
#'   dplyr::group_by(screen_name) %>%
#'   ts_plot("weeks")
#'
#' }
#'
#' @export
ts_data <- function(data, by = "days", trim = 0L, tz ="UTC") {
  args <- list(data = data, by = by, trim = trim, tz = tz)
  do.call("ts_data_", args)
}

ts_data_ <- function(data, by = "days", trim = 0L, tz = "UTC") {
  stopifnot(is.data.frame(data), is.atomic(by))
  if (has_name_(data, "created_at")) {
    dtvar <- "created_at"
  } else {
    dtvar <- vapply(data, inherits, "POSIXct", FUN.VALUE = logical(1))
    if (sum(dtvar) == 0L) stop("no datetime (POSIXct) var found", call. = FALSE)
    dtvar <- names(data)[which(dtvar)[1]]
  }
  ## drop NAs and sort data
  data <- data[!is.na(data[[dtvar]]), ]
  data <- data[order(data[[dtvar]]), ]
  ## reformat time var
  .unit <- parse_unit(by)
  ## adjust to desired tz
  data[[dtvar]] <- as.POSIXct(format(data[[dtvar]], tz = "UTC"), tz = tz)
  data[[dtvar]] <- round_time(data[[dtvar]], by, tz)
  ## get unique values of time in series
  dtm <- unique(
    seq(data[[dtvar]][1], data[[dtvar]][length(data[[dtvar]])], .unit)
  )
  ## if grouped df (up to 2 groups)
  if (inherits(data, "grouped_df") &&
      ("groups" %in% names(attributes(data)) ||
          "labels" %in% names(attributes(data)))) {
    if (!"groups" %in% names(attributes(data)) &&
        "labels" %in% names(attributes(data))) {
      groups <- names(attr(data, "labels"))
    } else {
      groups <- names(attr(data, "groups"))
      groups <- groups[!groups %in% ".rows"]
    }
    if (length(groups) > 1L) {
      group2 <- groups[2]
    } else {
      group2 <- NULL
    }
    group1 <- groups[1]
    lv1 <- unique(data[[group1]])
    df1 <- as.POSIXct(character(), tz = tz)
    df2 <- integer()
    df3 <- list()
    if (!is.null(group2)) {
      lv2 <- unique(data[[group2]])
      df4 <- list()
      ## count expressions for each row for output time series-like data
      for (i in seq_along(dtm)) {
        for (j in seq_along(lv1)) {
          for (k in seq_along(lv2)) {
            df1[length(df1) + 1L] <- dtm[i]
            df2[length(df2) + 1L] <- sum(
              data[[dtvar]] == dtm[i] &
                data[[group1]] == lv1[j] &
                data[[group2]] == lv2[k],
              na.rm = TRUE
            )
            df3[[length(df3) + 1L]] <- lv1[j]
            df4[[length(df4) + 1L]] <- lv2[k]
          }
        }
      }
      df <- data.frame(
        time = df1,
        n = df2,
        g1 = unlist(df3),
        g2 = unlist(df4),
        stringsAsFactors = FALSE
      )
      names(df)[3:4] <- groups[1:2]
    } else {
      ## count expressions for each row for output time series-like data
      for (i in seq_along(dtm)) {
        for (j in seq_along(lv1)) {
          df1[length(df1) + 1L] <- dtm[i]
          df2[length(df2) + 1L] <- sum(
            data[[dtvar]] == dtm[i] &
              data[[group1]] == lv1[j],
              na.rm = TRUE
          )
          df3[[length(df3) + 1L]] <- lv1[j]
        }
      }
      df <- data.frame(
        time = df1,
        n = df2,
        g1 = unlist(df3),
        stringsAsFactors = FALSE
      )
      names(df)[3] <- group1
    }
  } else {
    df <- data.frame(
      time = dtm,
      n = vapply(dtm, function(x) sum(data[[dtvar]] == x), FUN.VALUE = integer(1)),
      stringsAsFactors = FALSE
    )
  }
  df <- tibble::as_tibble(df)
  if (trim > 0L) {
    df <- trim_ts(df, trim)
  }
  df
}

parse_unit <- function(by) {
  stopifnot(is.atomic(by))
  if (is.numeric(by)) {
    return(by)
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


#' A generic function for rounding date and time values
#'
#' @param x A vector of class POSIX or Date.
#' @param n Unit to round to. Defaults to mins. Numeric values treated
#'   as seconds. Otherwise this should be one of "mins", "hours", "days",
#'   "weeks", "months", "years" (plural optional).
#' @param tz Time zone to be used, defaults to "UTC" (Twitter default)
#' @return If POSIXct then POSIX. If date then Date.
#' @examples
#'
#' ## class posixct
#' round_time(Sys.time(), "12 hours")
#'
#' ## class date
#' unique(round_time(seq(Sys.Date(), Sys.Date() + 100, "1 day"), "weeks"))
#' 
#' @export
round_time <- function(x, n, tz) UseMethod("round_time")

#' @export
round_time.POSIXt <- function(x, n = "mins", tz = "UTC") {
  n <- parse_to_secs(n)
  #as.POSIXct(hms::hms(as.numeric(x) %/% n * n), tz = tz)
  hms(as.numeric(x) %/% n * n, tz = tz)
}


hms <- function(secs = NULL, tz = "UTC") {
  if (is.null(secs)) {
    secs <- numeric()
  }
  structure(secs, tzone = tz,
    class = c("POSIXct", "POSIXt"))
}


#' @export
round_time.Date <- function(x, n = "months", tz = "UTC") {
  x <- as.POSIXct(format(x, tz = "UTC"), tz = tz)
  as.Date(round_time(x, n, tz = tz))
}


round_time2 <- function(x, interval = 60, center = TRUE, tz = "UTC") {
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
  as.POSIXct(rounded, tz = tz, origin = "1970-01-01")
}


trim_ts <- function(data, trim = 1L) {
  if (ncol(data) > 2L) {
    g <- unique(data[[3]])
    g <- lapply(g, function(x) trim_ots(data[data[[3]] == x, ], trim, trim))
    g <- do.call("rbind", g)
    if (ncol(data) == 4L) {
      g2 <- unique(data[[4]])
      g2 <- lapply(g2, function(x) trim_ots(data[data[[4]] == x, ], trim, trim))
      g2 <- do.call("rbind", g2)
      g <- rbind(g, g2)
    }
    g
  } else {
    trim_ots(data, trim, trim)
  }
}


trim_ots <- function(x, f = 1L, l = 1L) {
  x <- x[order(x[[1]]), ]
  f <- seq_len(f)
  l <- nrow(x) - seq_len(l) + 1L
  if ((length(l) + length(f)) >= nrow(x)) {
    return(x)
  }
  x[-c(f, l), ]
}


parse_to_secs <- function(x) {
  if (is.numeric(x)) {
    n <- x
  } else if (grepl("year", x)) {
    n <- 60 * 60 * 24 * 365
  } else if (grepl("month", x)) {
    n <- 60 * 60 * 24 * 30
  } else if (grepl("week", x)) {
    n <- 60 * 60 * 24 * 7
  } else if (grepl("day", x)) {
    n <- 60 * 60 * 24
  } else if (grepl("hour", x)) {
    n <- 60 * 60
  } else if (grepl("min", x)) {
    n <- 60
  } else if (grepl("sec", x)) {
    n <- 1
  } else {
    stop("must express time interval in secs, mins, hours, days, weeks, months, or years",
      call. = FALSE)
  }
  x <- as.double(gsub("[^[:digit:]|\\.]", "", x))
  if (any(is.na(x), identical(x, ""))) {
    x <- 1
  }
  n * x
}

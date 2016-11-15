#' ts_plot
#'
#' @param rt Tweets data frame
#' @param by Unit of time, e.g., \code{secs, days, weeks,
#'   months, years}
#' @param txt Name of text variable in data frame which
#'   filter is applied to.
#' @param filter Vector of regular expressions with which to
#'   filter data (creating multiple time series)
#' @param exclude Vector of regular expressions with which to
#'   distinguish data.
#' @param key Labels for filters. Defaults to actual filters.
#' @param cols Colors for filters
#' @param leg.x Location for plot text
#' @param leg.y Location for plot text
#' @param \dots Other arguments passed to plot function.
#'
#' @examples
#' \dontrun{
#' # stream tweets mentioning presidential debates hashtag
#' #   for 5 minutes
#' x <- stream_tweets(q = "debates2016", timeout = (60 * 5))
#' tsplot(x)
#' }
#' @importFrom graphics plot axis grid lines par rect text
#' @importFrom grDevices hcl
#' @importFrom stats runif sd
#' @export
ts_plot <- function(rt,
                    by = "days",
                    txt = "text",
                    filter = NULL,
                    exclude = NULL,
                    key = NULL,
                    cols = NULL,
                    leg.x = NULL,
                    leg.y = NULL,
                    ...) {
  if (is.null(filter)) {
    filter <- ""
  }
  if (is.null(key)) key <- filter
  df <- lapply(seq_along(filter), function(i)
    make_tsdat(
      key_filter(rt, filter[i], txt = txt),
      by = by, key[i]))
  ylim <- range(unlist(lapply(df,
    function(x) x[["freq"]])), na.rm = TRUE)
  xlim <- range(unlist(lapply(df,
    function(x) x[["time"]])), na.rm = TRUE)
  if (any(grepl("sec|min", by))) {
    xlim <- as.POSIXct(xlim, origin = "1970-01-01")
  } else if (any(grepl("hour", by))) {
    xlim <- as.POSIXct(xlim, origin = "1970-01-01")
  } else {
    xlim <- as.POSIXct(xlim, origin = "1970-01-01")
    xlim <- as.Date(xlim)
  }
  op <- par(no.readonly = TRUE)
  par(bg = "#fdfdfd", bty = "n", tcl = NA, las = 1)
  grid.minor <- rep(0, 49)
  grid.minor[(0:24 * 2) + 1] <- 1
  grid.major <- rep(3, 6)
  plot(NA, xlim = xlim, ylim = ylim,
    xlab = "Time", ylab = "Freq",
    axes = FALSE, ...,
    panel.first = list(rect(par("usr")[1], par("usr")[3],
        par("usr")[2], par("usr")[4],
        col = "#f5f5f5", border = NA),
    panel.last = grid(4, 4, lty = grid.major, lwd = .75),
    grid(8, 8, lty = grid.minor, lwd = .5)))
  yaxis <- signif(seq(ylim[1], ylim[2], length.out = 4), 2)
  axis(2, at = round(yaxis, 0),
    lwd = 2, col = "#666666",
    lwd.ticks = 2, col.ticks = "#444444")
  if (any(grepl("sec|min", by))) {
    axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
      labels = format(as.POSIXct(seq(xlim[1], xlim[2],
        length.out = 4)), "%l:%M%p"),
      lwd = 2, col = "#666666",
      lwd.ticks = 2, col.ticks = "#444444")
  } else if (any(grepl("hour", by))) {
    axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
      labels = format(as.POSIXct(seq(xlim[1], xlim[2],
        length.out = 4)), "%a"),
      lwd = 2, col = "#666666",
      lwd.ticks = 2, col.ticks = "#444444")
  } else if (any(grepl("day|week", by))) {
    axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
      labels = format(as.POSIXct(seq(xlim[1], xlim[2],
        length.out = 4)), "%b %d"),
      lwd = 2, col = "#666666",
      lwd.ticks = 2, col.ticks = "#444444")
    df <- lapply(df, function(x) {
      x[["time"]] <- as.Date(x[["time"]])
      return(x)
    })
  } else {
    axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
      labels = format(as.POSIXct(seq(xlim[1], xlim[2],
        length.out = 4)), "%b %Y"),
      lwd = 2, col = "#666666",
      lwd.ticks = 2, col.ticks = "#444444")
  }

  if (is.null(cols)) {
    cols <- gg_cols(length(filter))
    cols <- sample(cols)
  }

  if (all(is.null(leg.x), is.null(leg.y))) {
    rnum <- runif(1, .01, .99)
    leg.x <- x[round(NROW(x) * rnum, 0), 1]
    rnum <- runif(1, .1, .9)
    leg.y <- x[round(NROW(x) * rnum, 0), 2]
    leg.y <- (leg.y + 5) * runif(1, .7, 1.1)
    if (leg.y < ylim[1]) leg.y <- ylim[1]
    if (leg.y > ylim[2]) leg.y <- ylim[2]
  } else {
    leg.x <- as.POSIXct(x[round(NROW(x) * leg.x, 0), 1])
  }
  for (i in seq_along(df)) {
    lo <- mean(df[[i]]$freq, na.rm = TRUE) +
      (sd(df[[i]]$freq, na.rm = TRUE) * .2)
    if (any(
      df[[i]]$freq[1] < lo,
      df[[i]]$freq[NROW(df[[i]])] < lo)) {
      df[[i]] <- df[[i]][(length(filter) + 1):(NROW(
        df[[i]]) - 1 - length(filter)), ]
    }
    x <- df[[i]][, 1:2]
    lines(x, lwd = 2.5, col = paste0(cols[i], "cc"))


    leg.text <- unique(as.character(df[[i]][, 3]))
    text(leg.x[i], leg.y[i], labels = leg.text,
      col = paste0(cols[i], "ff"), cex = 1.25)
  }
  par(op)
  invisible(do.call("rbind", df))
}

key_filter <- function(x, filter = NULL, txt = "text", exclude = NULL) {
  if (!all(is.data.frame(x), txt %in% names(x))) {
    stop("must include tweets data frame", call. = FALSE)
  }
  if ("created_at" %in% names(x)) {
    o <- x[["created_at"]]
  } else {
    o <- x[, grep("date", x, value = TRUE)[1]]

  }
  x <- x[[txt]]

  if (is.null(filter)) return(x)

  ## select rows to keep for recursive x
  if (is.recursive(x)) {
    if (is.null(exclude)) {
      x <- vapply(x, function(i)
        any(tolower(filter) %in% tolower(i)),
        logical(1), USE.NAMES = FALSE)
    } else {
      x <- vapply(x, function(i)
        all(any(tolower(filter) %in% tolower(i)),
          any(!tolower(exclude) %in% tolower(i))),
        logical(1), USE.NAMES = FALSE)
    }
  } else {
    ## rows to keep for simple vector
    if (is.null(exclude)) {
      x <- vapply(x, function(i)
        any(grepl(
          paste0(tolower(filter), collapse = "|"),
          i, ignore.case = TRUE)),
        logical(1), USE.NAMES = FALSE)
    } else {
      x <- vapply(x, function(i)
        all(any(
          grepl(paste0(tolower(filter), collapse = "|"),
            i, ignore.case = TRUE)),
          any(!grepl(paste0(tolower(exclude), collapse = "|"),
            i, ignore.case = TRUE))),
        logical(1), USE.NAMES = FALSE)
    }
  }
  o[x]
}

#' @importFrom stats aggregate
make_tsdat <- function(x, by, key) {
  x <- aggregate(x, list(day = cut(x, by)), NROW)
  names(x) <- c("time", "freq")
  x$time <- as.POSIXct(x$time)
  dat <- data.frame(time = seq.POSIXt(as.POSIXct(min(x$time)),
    as.POSIXct(max(x$time)), by),
    freq = 0)
  dat <- dat[!dat$time %in% x$time, ]
  dat <- rbind(dat, x)
  dat <- dat[order(dat$time), ]
  if (!is.null(key)) {
    dat$key <- key
  } else {
    dat$key <- "all"
  }
  dat
}

addplot <- function(..., add = FALSE) {
  if (add) {
    lines(...)
  } else {
    plot(...)
  }
}


gg_cols <- function(n) {
  if (n < 4) {
    cols <- c("#003366", "#bb2222", "#992299", "#008800")
    return(sample(cols, n))
  }
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


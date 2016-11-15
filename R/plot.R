#' ts_plot
#'
#' Plots frequency of tweets as time series or, if multiple
#'   filters (text-based criteria used to subset data) are
#'   specified, multiple time series.
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
#' @param lab.cex Size of filter labels
#' @param lwd Width of filter lines
#' @param \dots Arguments passed to plot function, e.g.,
#'   \code{main = "#rstats tweets"},
#'   \code{xlab = "Date"},
#'   \code{ylab = "Tweets"},
#'   \code{main.cex = 1}.
#'
#'
#' @examples
#' \dontrun{
#' ## stream tweets mentioning beibs and tswift for 10 mins
#' rt <- rtweet::stream_tweets(
#'     q = "justinbieber,taylorswift13",
#'     timeout = (60 * 60 * 10))
#'
#' ## split mentions into distinct elements
#' mentions <- strsplit(rt$mentions_screen_name, ",")
#'
#' ## sorted table of mentions
#' mentions <- sort(table(unlist(mentions)),
#'     decreasing = TRUE)
#'
#' ## exclude biebs and tswift
#' mentions <- grep("justinbieber|taylorswift13", names(mentions),
#'     invert = TRUE, value = TRUE)
#'
#' ## store next most pop in obj
#' thirdpop <- mentions[1]
#'
#' ##plot with mentions as filters
#' ts.df <- ts_plot(rt, by = "mins", filter = c(
#'     "justinbieber", "taylorswift", thirdpop),
#'     main = "Biebs vs Tswift")
#'
#' ## preview returned data frame
#' head(ts.df)

#'
#' }
#' @importFrom graphics plot axis grid lines par rect text
#' @importFrom grDevices hcl
#' @importFrom stats runif sd aggregate
#' @export
ts_plot <- function(rt, by = "days", txt = "text", filter = NULL,
  exclude = NULL, key = NULL, cols = NULL, leg.x = NULL,
  leg.y = NULL, lab.cex = NULL, lwd = NULL, ...) {

  if (is.null(filter)) {
    filter <- ""
  }
  if (is.null(key)) {
    key <- filter
  }
  if (identical(filter, "")) {
    fd <- list(sollts(rt[["created_at"]], by = by))
  } else {
    fd <- lapply(lapply(filter, grep, rt[[txt]]),
      function(x) rt[["created_at"]][x])
    fd <- lapply(fd, sollts, by = by)
  }

  for (i in seq_along(fd)) {
    names(fd[[i]]) <- c("time", "freq")
    fd[[i]]$time <- as.POSIXct(fd[[i]]$time)
    fd[[i]]$filter <- key[i]
  }

  df <- do.call("rbind", fd)

  op <- par(no.readonly = TRUE)
  par(bg = "#fdfdfd", bty = "n", tcl = NA, las = 1,
    mar = c(5, 4, 4, 3))
  grid.minor <- rep(0, 49)
  grid.minor[(0:24 * 2) + 1] <- 1
  grid.major <- rep(3, 6)
  xlim <- range(df$time)
  ylim <- range(df$freq)
  yaxis <- signif(seq(ylim[1], ylim[2], length.out = 4), 2)
  if (any(yaxis %% 1 > 0)) {
    yaxis <- signif(seq(ylim[1], ylim[2], length.out = 4), 1)
  }
  if (any(grepl("POSIX", class(xlim)))) {
    xaxis <- seq.POSIXt(xlim[1], xlim[2], length.out = 4)
  } else {
    xaxis <- seq.POSIXt(xlim[1], xlim[2], length.out = 4)
  }
  plot(df$time, df$freq, ylim = yaxis[c(1, 4)], type = "p",
    cex = 0, col = NA,
    xlab = "Time", ylab = "Freq", axes = FALSE, ...,
    panel.first = list(rect(par("usr")[1], par("usr")[3],
      par("usr")[2], par("usr")[4],
      col = "#f5f5f5", border = NA),
      panel.last = grid(4, 4, lty = grid.major, lwd = .75),
      grid(8, 8, lty = grid.minor, lwd = .75)))

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

  if (is.null(lab.cex)) lab.cex <- 1.1
  if (is.null(lwd)) lwd <- 2.5

  for (i in seq_along(filter)) {
    dff <- df[df$filter == key[i], ]
    x <- dff[, 1:2]

    lines(x, lwd = lwd, col = paste0(cols[i], "dd"))

    if (is.null(leg.x)) {
      pct.x <- runif(1, .01, .99)
      legx <- xlim[1] + (pct.x * diff(xlim))
    } else {
      legx <- xlim[1] + (leg.x * diff(xlim))
    }
    if (is.null(leg.y)) {
      pct.y <- runif(1, -1, 1)
      row.y <- round(pct.x * NROW(x), 0)
      legy <- x$freq[row.y]
      if (isTRUE(legy < 2)) {
        legy <- runif(1, 1, (diff(ylim)/3))
      } else {
        rnum <- runif(1, 3, 8)
        legy <- legy + runif(1, -(diff(ylim)/rnum), (diff(ylim)/rnum))
        if (isTRUE(legy < 2)) {
          legy <- runif(1, 1, (diff(ylim)/3))
        }
      }
    } else {
      legy <- ylim[1] + (leg.y * diff(ylim))
    }

    leg.text <- unique(as.character(dff[, 3]))
    text(legx, legy, labels = leg.text,
      col = paste0(cols[i], "ff"), cex = lab.cex)
  }
  par(op)
  invisible(df)
}

sollts <- function(x, by = "days") {
  if (grepl("month", by)) unit <- 2592000
  if (grepl("week", by)) unit <- 604800
  if (grepl("day", by)) unit <- 86400
  if (grepl("hour", by)) unit <- 3600
  if (grepl("min", by)) unit <- 60
  if (grepl("sec", by)) unit <- 1

  mf <- as.numeric(gsub("[^[:digit:]]", "", by))
  if (any(is.na(mf), identical(mf, ""))) mf <- 1
  unit <- mf * unit
  ca <- as.POSIXct(
    ceiling((as.numeric(x) / unit)) * unit,
    origin = "1970-01-01")
  as.data.frame(table(ca))
}

gg_cols <- function(n) {
  if (n < 4) {
    cols <- c("#003366", "#bb2222", "#008800")
    return(sample(cols, n))
  }
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


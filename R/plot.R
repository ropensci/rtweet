#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

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
#' @param xlim Limits for x axis, defaults for range of observed
#'   time.
#' @param ylim Limits for y axis, defaults to range plus a little
#'   bit of padding.
#' @param legend Logical indicating wether to include a plot
#'   legend. Defaults to true.
#' @param leg.x Location for plot text
#' @param leg.y Location for plot text
#' @param lab.cex Size of filter labels
#' @param lwd Width of filter lines
#' @param plot Logical indicating whether to draw plot.
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
ts_plot <- function(rt, by = "days",
                    txt = "text",
                    filter = NULL,
                    exclude = NULL,
                    key = NULL,
                    cols = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    legend = TRUE,
                    leg.x = NULL,
                    leg.y = NULL,
                    lab.cex = NULL,
                    lwd = NULL,
                    plot = TRUE, ...) {

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

    if (!plot) return(invisible(df))

    op <- par(no.readonly = TRUE)
    par(bg = "#f0f0f0cc", bty = "n",
        tcl = -.125, las = 1,
        mar = c(5, 4, 4, 8.5), cex = .8)
    grid.minor <- rep(0, 49)
    grid.minor[(0:24 * 2) + 1] <- 1
    grid.major <- rep(3, 6)

    ## x axis
    if (is.null(xlim)) {
        xlim <- range(df$time, na.rm = TRUE)
    }
    xaxis <- seq.POSIXt(xlim[1], xlim[2], length.out = 4)
    ## y axis
    if (is.null(ylim)) {
        pads <- ceiling(sd(df$freq, na.rm = TRUE) / 5)
        ylim <- c(min(df$freq, na.rm = TRUE) - pads,
                  max(df$freq, na.rm = TRUE) + pads)
        if (ylim[1] <= 0) {
            ylim[1] <- 0
        } else {
            ylim[1] <- ylim[1] - (ylim[2] %% 5)
        }
        ylim[2] <- ylim[2] + 5 - (ylim[2] %% 5)
    }
    yaxis <- seq(ylim[1], ylim[2], length.out = 5)

    plot(df$time, df$freq,
         ylim = ylim,
         type = "p",
         cex = 0,
         col = NA,
         xlab = "Time",
         ylab = "Freq",
         axes = FALSE, ...,
         panel.first = list(
             rect(par("usr")[1], par("usr")[3],
                  par("usr")[2], par("usr")[4],
                  col = "#fcfcfc", border = "#f2f2f2",
                  lwd = .75)),
         panel.last = list(
             grid(4, 4, lty = grid.major, lwd = .75),
             grid(8, 8, lty = grid.minor, lwd = .75)))
    om <- par("mai")
    par(mar = c(0,0,0,0))
    points(runif(500000, xlim[1], xlim[2]),
           runif(500000, ylim[1], ylim[2]),
           col = "#ffffffaa", pch = 16, cex = .1)
    par(mai = om)
    par(mar = c(5, 4, 4, 8.5))
    axis(2, at = round(yaxis, 0),
         lwd = 0, col = "#666666",
         lwd.ticks = 2, col.ticks = "#444444")

    if (abs(diff(as.numeric(xlim))) > 10000000) {
        axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
             labels = format(as.POSIXct(seq(
                 xlim[1], xlim[2],
                 length.out = 4)), "%b %Y"),
             lwd = 0, col = "#666666",
             lwd.ticks = 2, col.ticks = "#444444")
    } else if (any(grepl("sec|min", by))) {
        axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
             labels = format(
                 as.POSIXct(seq(
                     xlim[1], xlim[2],
                     length.out = 4)), "%l:%M%p"),
             lwd = 0, col = "#666666",
             lwd.ticks = 2, col.ticks = "#444444")
    } else if (any(grepl("hour", by))) {
        axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
             labels = format(as.POSIXct(seq(
                 xlim[1], xlim[2],
                 length.out = 4)), "%a"),
             lwd = 0, col = "#666666",
             lwd.ticks = 2, col.ticks = "#444444")
    } else if (any(grepl("day|week", by))) {
        axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
             labels = format(as.POSIXct(seq(
                 xlim[1], xlim[2],
                 length.out = 4)), "%b %d"),
             lwd = 0, col = "#666666",
             lwd.ticks = 2, col.ticks = "#444444")
    } else {
        axis(1, at = seq(xlim[1], xlim[2], length.out = 4),
             labels = format(as.POSIXct(seq(
                 xlim[1], xlim[2],
                 length.out = 4)), "%b %Y"),
             lwd = 0, col = "#666666",
             lwd.ticks = 2, col.ticks = "#444444")
    }

    if (is.null(cols)) {
        cols <- gg_cols(length(filter))
        cols <- sample(cols)
    } else if (identical(cols, "transparent")) {
        cols <- "transparent"
    }

    if (is.null(lab.cex)) lab.cex <- 1.1
    if (is.null(lwd)) lwd <- 2

    for (i in seq_along(filter)) {
        dff <- df[df$filter == key[i], ]
        x <- dff[, 1:2]

        lines(x, lwd = lwd, col = cols[i])

        if (!legend) {
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
                    legy <- legy +
                        runif(1, -(diff(ylim)/rnum), (
                            diff(ylim)/rnum))
                    if (isTRUE(legy < 2)) {
                        legy <- runif(1, 1, (diff(ylim)/3))
                    }
                }
            } else {
                legy <- ylim[1] + (leg.y * diff(ylim))
            }

            leg.text <- unique(as.character(dff[, 3]))
            text(legx, legy, labels = leg.text,
                 col = cols[i], cex = lab.cex)
        }
    }
    if (legend) {
        par(xpd = TRUE)
        legend("right", key,
               bty = "o",
               lty = rep(1, length(key)),
               lwd = lwd,
               col = cols,
               inset = -.245,
               cex = .9,
               pt.lwd = 1.2,
               box.lwd = .55,
               box.col = "#f2f2f2",
               bg = "#fcfcfc")
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
        ceiling(as.numeric(x) / unit) * unit +
        sample(c(unit * -.5, unit * .5), 1),
        origin = "1970-01-01")
    as.data.frame(table(ca), stringsAsFactors = FALSE)
}

#' ggplot colors
#'
#' Returns ggplot2 html colors.
#'
#' @param n Number of colors to return. If fewer than 4,
#'   the function randomly samples 3 better colors than
#'   what's normally produced ny the ggplot internal
#'   mechanism.
#' @return Character vector of html colors.
#' @export
gg_cols <- function(n) {
    if (n < 4) {
        cols <- c("#003366", "#bb2222", "#008800")
        return(sample(cols, n))
    }
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

#' basemap
#'
#' Plots world map
#'
#' @param mapdf Mapping data frame defaults to high resolution
#'    rworldmap.
#' @param xlim Maximimum plot values along x-axis. Defaults to
#'    \code{xlim = c(-145, 145)} with a max range of -180 to
#'    180 (degree longitude)
#' @param ylim Maximimum plot values along y-axis. Defaults to
#'    \code{ylim = c(-50, 70)} with a max range of -90 to
#'    90 (degrees latitude)
#' @param ocean.col Fill color to apply to ocean area to a
#'    light gray color.
#' @param land.col Fill color to apply to land area defaults to
#'    white.
#' @param border.lwd Width of country borders defaults to .025
#' @return Plot of world map.
#' @importFrom rworldmap getMap
#' @importFrom graphics plot
#' @importFrom maps map
#' @export
basemap <- function(mapdf = NULL,
                    xlim = c(-179, 179),
                    ylim = c(-55, 80),
                    ocean.col = "#e0e0eb99",
                    land.col = "#ffffff",
                    border.lwd = .1) {

    if (is.null(mapdf)) {
        mapdf <- rworldmap::getMap(resolution = "coarse")
    }
    par(mar = c(0, 0, 0, 0))
    maps::map(mapdf, lwd = border.lwd, col = land.col,
              bg = ocean.col, fill = TRUE,
              xlim = xlim, ylim = ylim)
}

latlong <- function(x) {
    data.frame(
        lat = rowMeans(
            x$coordinates[, 1:4], na.rm = TRUE),
        long = rowMeans(
            x$coordinates[, 5:8], na.rm = TRUE))
}
go.long <- function(data)
    rowMeans(data$coordinates[, 1:4], na.rm = TRUE)
go.lat <- function(data)
    rowMeans(data$coordinates[, 5:8], na.rm = TRUE)

#' longlat
#'
#' Add long and lat variables to data frame
#'
#' @param data Twitter data
#' @param by Unit of time
#' @export
longlat <- function(data, by = NULL) {
    if (!is.null(by)) {
        data <- ts_plot(data, by, plot = FALSE)
    } else {
        data$long <- go.long(data)
        data$lat <- go.lat(data)
    }
    data
}

#' alphacolor
#'
#' Returns colors at alpha level a
#'
#' @param cols Colors vector
#' @param a Alpha level numeric ranging from 0 to 1
#' @importFrom grDevices col2rgb rgb
#' @export
alphacolor <- function(cols, a = .99) {
    cols <- t(col2rgb(cols, alpha = TRUE)) / 255
    rgb(cols, alpha = a)
}

#' rtdata
#'
#' Initializes rt plotting sequence
#'
#' @param data Data frame generated via rtweet function.
#' @param by Unit of time for aggregating tweet frequency for
#'   time series plot. Defaults to NULL. To plot as time series
#'   by argument is required (e.g., \code{by = "days"}.
#' @param \dots Args passed to aes and other functions.
#' @export
rtdata <- function(data, by = NULL, ...) {
    with(longlat(data, by), rtaes(x = long, y = lat, ...))
}

getcols <- function (n = 1, l = 65, c = 100) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = l, c = c)[1:n]
}

#' @importFrom grDevices col2rgb
is.color <- function(x) {
    if (all(grepl("^#", x))) return(TRUE)
    x <- tryCatch(col2rgb(x),
                  error = function(e) return(NULL))
    if (!is.null(x)) return(TRUE)
    FALSE
}


#' rtaes
#'
#' Sets aesthetics
#'
#' @param x Variable x either longitudinal coordinates or
#'    time
#' @param y Variable y either latitude coordinates or
#'    freq
#' @param color Variable used to determine color
#' @param alpha Alpha level used to set transparency
#' @param size Size of plot units
#' @param shape Shape of plot units
#' @export
rtaes <- function(x, y,
                  color = NULL,
                  alpha = NULL,
                  size = NULL,
                  shape = NULL) {

    if (is.null(alpha)) alpha <- .99
    if (is.null(size)) size <- .4
    if (is.null(shape)) shape <- 16L

    if (!is.null(color)) {
        if (is.color(color)) {
            colors <- color
        } else {
            if (is.factor(color)) {
                cols <- length(levels(color))
                cols <- sample(getcols(cols), cols)
                colors <- cols[match(color, levels(color))]
            } else {
                cols <- length(unique(color))
                cols <- sample(getcols(cols), cols)
                colors <- cols[match(color, unique(color))]
            }
        }
    } else {
        colors <- "#0066aa"
    }
    colors <- alphacolor(colors, alpha)

    data.frame(x = x, y = y,
               color = colors,
               size = size,
               shape = shape,
               stringsAsFactors = FALSE)
}

#' rtpoint
#'
#' Plots map coordinate points
#'
#' @param dat Data piped from rtaes function
#' @param noise Logical indicating whether to apply minor
#'    jitter function to points.
#' @importFrom graphics points
#' @export
rtpoint <- function(dat, noise = FALSE) {
    if (noise) {
        stdv <- sd(as.double(dat[["x"]]),
                   na.rm = TRUE) / 1000
        dat[["x"]] <- runif(NROW(dat), -stdv * 2,
                            stdv * 2) + dat[["x"]]
        dat[["y"]] <- runif(NROW(dat), -stdv * 2,
                            stdv * 2) + dat[["y"]]
    }
    points(dat[["x"]],
           dat[["y"]],
           col = dat[["color"]],
           cex = dat[["size"]],
           pch = dat[["shape"]])
    invisible(dat)
}


#' rtline
#'
#' Plots time series line
#'
#' @param dat Data piped from rtaes function
#' @param new Logical indicating whether to revert to base r plot
#'   defaults to false.
#' @param \dots Args passed to base plot.
#' @export
rtline <- function(dat, new = FALSE, ...) {
    names(dat)[1:2] <- c("time", "freq")
    dat$time <- as.numeric(dat$time)
    dat$freq <- as.numeric(dat$freq)
    if (new) {
        with(dat, plot(time, freq, type = "l", lty = lty, ...))
    } else {
        with(dat, lines(time, freq, ...))
    }
    invisible(dat)
}

#' rtmap
#'
#' Initiates and plots new world map
#'
#' @param dat Piped data
#' @param \dots Args passed along to plot function.
#' @export
rtmap <- function(dat, ...) {
    basemap(...)
    invisible(dat)
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' ts_plot
#'
#' Plots frequency of tweets as time series or, if multiple
#'   filters (text-based criteria used to subset data) are
#'   specified, multiple time series.
#'
#' @param rt Tweets or users data frame
#' @param by Unit of time, e.g., \code{secs, days, weeks,
#'   months, years}
#' @param dtname Name of date-time (POSIXt) object. Defaults
#'   to created_at.
#' @param txt Name of text (chr) variable in data frame which
#'   filter is applied to. Defaults to text.
#' @param filter Vector of regular expressions with which to
#'   filter data (creating multiple time series).
#' @param key Optional provide pretty labels for filters.
#'   Defaults to actual filters.
#' @param lwd Width of time series line(s). Defaults to 1.5
#' @param cols Colors for filters. Leave NULl for default color
#'   scheme.
#' @param theme Character string specifyng whether and which
#'   plot theme should be used; options include "lighter",
#'   "darker", and "nerdy"
#' @param main Optional, text for plot title.
#' @param xlab Optional, text for x-axis title, defaults to
#'   "Time".
#' @param ylab Optional, text for y-axis title, defaults to
#'   "Number of Tweets"
#' @param box Logical indicating whether to draw box around
#'   plot area. Defaults to true.
#' @param axes Logical indicating whether to draw axes. Defaults
#'   to true.
#' @param legend.title Provide title for legend ro ignore to
#'   leave blank (default)
#' @param ticks Numeric specifying width of tick marks. Defaults
#'   to zero. If you'd like tick marks, try setting this value
#'   to 1.25.
#' @param cex Global cex setting defaults to .90
#' @param cex.main Size of plot title (if plot title provided
#'   via \code{main = "title"} argument).
#' @param cex.sub Size of subtitles
#' @param cex.lab Size of axis labels and legend text
#' @param cex.axis Size of other axis text.
#' @param mai Margins in inches.
#' @param plot Logical indicating whether to draw plot.
#' @param \dots Arguments passed to plot (and par) function
#'
#' @examples
#' \dontrun{
#' ## stream tweets mentioning trump for 30 mins
#' rt <- rtweet::stream_tweets(
#'     q = "realdonaldtrump",
#'     timeout = (60 * 60 * 30))
#'
#' ## plot tweet data aggregated by minute (default)
#' ts_plot(rt, by = "mins")
#'
#' ## use a different time increment, line width, and theme
#' ts_plot(rt, by = "30 secs", lwd = .75, theme = "inverse")
#'
#' ## filter data using regular expressions and plot
#' ## each corresponding time series
#' ts_plot(rt, by = "mins",
#'         theme = "gray",
#'         main = "Partisanship in tweets mentioning Trump",
#'         filter = c("democrat|liberal|libs",
#'                    "republican|conservativ|gop"),
#'         key = c("Democrats", "Republicans"))
#'
#' ## ts_plot also silently returns data frame
#' rt.ts <- ts_plot(rt, by = "mins")
#'
#' ## printing should yield around 30 rows (give or take)
#' rt.ts
#'
#' ## the returned data frame is tidy with three columns
#' ## Column 1 - time Date-time obj of [median] time intervals
#' ## Column 2 - freq Integer (class double) frequency counts
#' ## Column 3 - filter Keys of different time series filters
#' rt.ts <- ts_plot(rt, by = "mins",
#'                  filter = c("democrat|liberal|libs",
#'                             "republican|conservativ|gop"),
#'                  key = c("Democrats", "Republicans"))
#'
#' ## this makes it easy to pass the data along to ggplot
#' ## but if anyone asks you, you prefer the awesome rtweet
#' ## plot themes over the ggplot2 themes!
#' library(ggplot2)
#' rt.ts %>%
#'     ggplot(aes(x = time, y = freq, color = filter)) +
#'     geom_line()
#' }
#' @importFrom graphics plot axis axis.POSIXct grid lines par
#'   rect title strwidth legend
#' @importFrom stats quantile
#' @export
ts_plot <- function(rt, by = "days",
                    dtname = "created_at",
                    txt = "text",
                    filter = NULL,
                    key = NULL,
                    lwd = 1.5,
                    cols = NULL,
                    theme = "light",
                    main = NULL,
                    xlab = "Time",
                    ylab = "Number of Tweets",
                    box = TRUE,
                    axes = TRUE,
                    legend.title = NULL,
                    ticks = 0,
                    cex = .90,
                    cex.main = 1.25,
                    cex.sub = .9,
                    cex.lab = .9,
                    cex.axis = .7,
                    mai = c(.515, .575, .15, .3),
                    plot = TRUE,
                    ...) {

    ## if there are filters (subsets)
    if (!is.null(filter)) {
        ## flag filtered obs
        f.rows <- lapply(
            filter, grepl, rt[[txt]], ignore.case = TRUE)
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
        ## if no pretty label provided use filter
        if (is.null(key)) key <- filter
        ## add variable name for each filter
        for (i in seq_along(lstdat)) {
            lstdat[[i]]$filter <- key[i]
        }
        ## collapse into tidy data frame
        dat <- do.call("rbind", lstdat)
    } else {
        ## if no filters then simple
        dat <- sollts(rt[[dtname]], by = by)
        dat$filter <- ""
    }
    if (!plot) return(dat)
    ## store current aesthetics
    op <- par(no.readonly = TRUE)
    ## restore those values on exit
    on.exit(par(op))

    ## estimate width of right (legend side) margin
    ## if no filter then no legend/smaller margin
    if (is.null(filter)) {
        mai[4] <- .3
    } else {
        ## select biggest filter
        legmarg <- key[which.max(nchar(key))]
        mai[4] <- strwidth(
            legmarg, units = "inches") + .475
    }
    ## top margin depends on whether main (title)
    if (!is.null(main)) {
        mai[3] <- .4
    } else {
        mai[3] <- .15
    }
    theme.bg <- "#ffffff"
    if (theme %in% c("reverse", "inverse", 2)) {
        theme.bg <- "#f5f5f5"
    } else if (theme %in% c("minimal", "simple", 6)) {
        theme.bg <- "#ffffff"
    } else if (theme %in% c("gray", "grey", 5)) {
        theme.bg <- "#f0f0f0"
    }
    ## set aesthetics
    par(tcl = -.125,
        cex = cex,
        cex.axis = cex.axis,
        cex.lab = cex.lab,
        cex.main = cex.main,
        cex.sub = cex.sub,
        lend = "butt",
        las = 1,
        bg = theme.bg,
        mgp = c(2, .2, 0),
        mai = mai)
    ## init plot to get parameters and set scale
    with(dat, plot(time, freq, type = "l",
                   lwd = 0,
                   axes = FALSE,
                   main = main,
                   xlab = "",
                   ylab = "",
                   ...))
    title(ylab = ylab, mgp = c(2.1, .25, 0))
    title(xlab = xlab, mgp = c(1.5, .25, 0))

    ## draw axes
    if (axes) {
        x.ticks <- seq(
            par("xaxp")[1], par("xaxp")[2],
            length.out = par("xaxp")[3] + 1)
        x.labs <- as.POSIXct(x.ticks, origin = "1970-01-01")
        axis.POSIXct(1, at = x.labs,
             lwd = 0, lwd.ticks = ticks)
        axis(2, at = NULL,
             lwd = 0, lwd.ticks = ticks)
    }

    ## construct background grid aesthetic
    if (theme %in% c("light", "lighter", 1)) {
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#f9f9f9", lwd = 0, border = NA)
        ## blend grid lines
        grid(col = "#fcfcfc", lwd = 6, lty = 1)
        grid(col = "#ffffff", lwd = 3, lty = 1)
        grid(col = "#999999", lwd = .5, lty = 2)
        ## draw box
        if (box) {
            box(lwd = 1.5, col = "#666666")
        }
    } else if (theme %in% c("inverse", "reverse", 2)) {
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#ffffff", lwd = 0, border = NA)
        ## blend grid lines
        grid(col = "#f0f0f0", lwd = 4, lty = 1)
        grid(col = "#999999", lwd = .5, lty = 2)
        ## draw box
        if (box) {
            box(lwd = 1.5, col = "#666666")
        }
    } else if (theme %in% c("dark", "darker", 3)) {
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#f0f0f0", lwd = 0, border = NA)
        ## blend grid lines to white
        grid(col = "#e9e9e9", lwd = 5, lty = 1)
        grid(col = "#c5c5c5", lwd = 2.5, lty = 1)
        grid(col = "#666666", lwd = .5, lty = 2)
        ## draw box
        if (box) {
            box(lwd = 1.5, col = "#666666")
        }
    } else if (theme %in% c("nerdy", "nerd", "nerdier", 4)) {
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#f5f5f5", lwd = .75,
             density = 15, angle = 90, border = NA)
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#f5f5f5", lwd = .75,
             density = 15, angle = 0, border = NA)
        grid(col = "#00336655", lwd = .75, lty = 1)
        ## draw box
        if (box) {
            box(lwd = 1.5, col = "#99002266")
        }
    } else if (theme %in% c("gray", "grey", 5)) {
        ## blend grid lines to white
        grid(col = "#dadada", lwd = 3, lty = 1)
        grid(col = "#666666", lwd = .5, lty = 5)
        ## draw box
        if (box) {
            box(lwd = 1.5, col = "#666666")
        }
    } else if (theme %in% c("minimal", "simple", 6)) {
        ## blend grid lines to white
        grid(col = "#666666", lwd = .3, lty = 2)
        ## draw box
        if (box) {
            box(lwd = 1.5, col = "#333333")
        }
    }

    ## draw lines and add legend
    if (!is.null(filter)) {
        ## if colors undefined
        if (is.null(cols)) {
            if (length(filter) < 4) {
                cols <- c("#880000cc", "#003366cc", "#008800cc")
                cols <- cols[seq_along(filter)]
            } else {
                ## includes random sample so jumpy colors
                cols <- gg_cols(length(filter))
            }
        } else {
            ## if colors provided check them
            if (length(cols) < length(filter)) {
                stop(paste0(
                    "insufficient number of colors. try ",
                    "picking ", length(filter), " colors :)."))
            } else {
                cols <- cols[seq_along(filter)]
            }
        }
        ## if no key then use filter expression(s)
        if (is.null(key)) key <- filter
        ## iterate over xy lines
        seq_along(lstdat) %>%
            lapply(function(i)
                with(lstdat[[i]],
                     lines(time, freq, lwd = lwd, col = cols[i])))
        ## calculate x legend postion given par
        ## y legend position looks too short at median so
        ## use .575 quartile instead
        legend(
            par("usr")[2] + (par("usr")[2] - par("usr")[1]) *
            (1-par("plt")[2]) / 16,
            quantile(c(par("usr")[3], par("usr")[4]), .575),
            key,
            lwd = rep(((5/3) * lwd), length(filter)),
            col = cols,
            x.intersp = .5,
            title = legend.title,
            lty = rep(1, length(filter)),
            seg.len = 1,
            xpd = TRUE,
            bty = "n",
            cex = cex.lab)
    } else {
        if (is.null(cols)) cols <- "#222222"
        with(dat, lines(time, freq, lwd = lwd, col = cols))
    }

    ## return aggregated data
    invisible(dat)
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
        time <- seq(min(rdt), max(rdt), .unit)
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

#' gg_cols
#'
#' returns ggplot2-like colors
#' @param n number of desired colors
#' @keywords internal
#' @noRd
#' @importFrom grDevices hcl
#' @export
gg_cols <- function(n) {
    if (n < 4) {
        cols <- c("#003366", "#bb2222", "#008800")
        return(sample(cols, n))
    }
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 60, c = 100)[1:n]
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

#' @importFrom grDevices hcl
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
#' @importFrom stats sd runif
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

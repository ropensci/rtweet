#' ts_plot
#'
#' Plots frequency of tweets as time series or, if multiple
#'   filters (text-based criteria used to subset data) are
#'   specified, multiple time series.
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
#' @param na.omit Logical indicating whether to omit rows with
#'   missing (NA) values for the dtname variable. Defaults to TRUE.
#'   If FALSE and data contains missing values for the date-time
#'   variable, an error will be returned to the user.
#' @param filter Vector of regular expressions with which to
#'   filter data (creating multiple time series).
#' @param key Optional provide pretty labels for filters.
#'   Defaults to actual filters.
#' @param trim Logical indicating whether to trim extreme intervals,
#'   which often capture artificially lower frequencies. Defaults to
#'   FALSE.
#' @param lwd Width of time series line(s). Defaults to 1.5
#' @param linetype Logical indicating whether lines should be
#'   distinguished by line type.
#' @param cols Colors for filters. Leave NULl for default color
#'   scheme.
#' @param theme Either integer (0-8) or character string specifyng the
#'   plot theme. Options include "light", "inverse", "dark", "nerd",
#'   "gray", "spacegray", "minimal", and "apa" (my attempt at making an
#'   APA-consistent graphic).
#' @param main Optional, title of the plot. By default, the title is
#'   printed on top of the plot and it is left-justified (ggplot2 style).
#'   To alter justification, see \code{adj}.
#' @param subtitle Optional, text for plot subtitle. Inherits
#'   justification method from \code{main}.
#' @param adj Logical indicating whether to left justify main
#'   plot title. Defaults to TRUE. To more exactly specify hornizontal
#'   location of the title, provide a numeric value between 0 (left) and
#'   1 (right).
#' @param xlab Optional, text for x-axis title, defaults to
#'   "Time".
#' @param ylab Optional, text for y-axis title, defaults to
#'   "Freq"
#' @param box Logical indicating whether to draw box around
#'   plot area. Defaults to false.
#' @param axes Logical indicating whether to draw axes. Defaults
#'   to true. Users may set this to FALSE and supply their own axes
#'   using the base graphics axis function.
#' @param legend.title Provide title for legend or ignore to leave
#'   blank (default).
#' @param ticks Numeric specifying width of tick marks. Defaults
#'   to zero. If you'd like tick marks, try setting this value
#'   to 1.25.
#' @param cex Global cex setting defaults to 1.0.
#' @param cex.main Size of plot title (if plot title provided
#'   via \code{main = "title"} argument).
#' @param cex.sub Size of subtitles
#' @param cex.lab Size of axis labels
#' @param cex.axis Size of axis text
#' @param cex.legend Size of legend text
#' @param mar Margins in number of lines.
#' @param font.main Font style of main title if provided. Default is
#'   to 1, which means (non-bold) normal font, overriding R's bold
#'   default, which I think is a little to aggressive. If you disagree
#'   with me, you can make the title bold by setting this value to 2.
#' @param xtime Format date-time labels in x-axis. Accepts any format
#'   string via \code{strptime}, e.g., \code{xtime = "\%F \%H:\%S"}.
#' @param plot Deprecated. Use \code{ts_filter} to create
#'   time series-like data frame.
#' @param \dots Arguments passed to base graphics plot function.
#'
#' @examples
#' \dontrun{
#' ## stream tweets mentioning trump for 30 mins
#' rt <- stream_tweets(
#'     q = "realdonaldtrump",
#'     timeout = (60 * 60 * 30))
#'
#' ## plot tweet data aggregated by minute (default)
#' ts_plot(rt, by = "mins")
#'
#' ## use a different time increment, line width, and theme
#' ts_plot(rt, by = "30 secs", lwd = .75, theme = "inverse")
#'
#' ## filter data using regular expressions and
#' ## plot each corresponding time series
#' ts_plot(rt, by = "mins",
#'         theme = "gray",
#'         main = "Partisanship in tweets about Trump",
#'         filter = c("democrat|liberal|libs",
#'                    "republican|conservativ|gop"),
#'         key = c("Democrats", "Republicans"))
#'
#' ## ts_plot also accepts data frames created via ts_filter
#' rt.ts <- ts_filter(
#'     rt, "mins",
#'     filter = c("democrat|liberal|libs",
#'                "republican|conservativ|gop"),
#'     key = c("Democrats", "Republicans"))
#' ## printing should yield around 30 rows (give or take)
#' ## since stream was 30 mins and aggregated by minute
#' rt.ts
#'
#' ## Pass data frame created by ts_filter to ts_plot
#' ts_plot(rt.ts, theme = "spacegray")
#'
#' ## the returned data frame from ts_filter also fits the
#' ## tidyverse and includes three columns
#' ## Column 1 - time Date-time obj of [median] time intervals
#' ## Column 2 - freq Integer (class double) frequency counts
#' ## Column 3 - filter Keys of different time series filters
#'
#' ## This makes it easy to pass the data along to ggplot
#' ## but my themes are cooler anyway so why bother?
#' ## library(ggplot2)
#' ## rt.ts `%>%`
#' ##     ggplot(aes(x = time, y = freq, color = filter)) +
#' ##     geom_line()
#' }
#' @importFrom graphics plot axis axis.POSIXct grid lines par
#'   rect title strwidth legend mtext
#' @importFrom stats quantile
#' @export
ts_plot <- function(rt, by = "days",
                    dtname = "created_at",
                    txt = "text",
                    na.omit = TRUE,
                    filter = NULL,
                    key = NULL,
                    trim = FALSE,
                    lwd = 1.5,
                    linetype = FALSE,
                    cols = NULL,
                    theme = "light",
                    main = NULL,
                    subtitle = NULL,
                    adj = TRUE,
                    xlab = "Time",
                    ylab = "Freq",
                    box = FALSE,
                    axes = TRUE,
                    legend.title = NULL,
                    ticks = 0,
                    cex = 1,
                    cex.main,
                    cex.sub,
                    cex.lab,
                    cex.axis,
                    cex.legend,
                    mar,
                    font.main = 1,
                    xtime = NULL,
                    plot = TRUE,
                    ...) {

    ## check if correct data arg is supplied
    if (missing(rt)) {
        stop(paste0("must provide data frame or named list ",
                    "containing a date-time variable"),
             call. = FALSE)
    } else {
        stopifnot(is.recursive(rt))
    }
    if (all(c("time", "freq", "filter") %in% names(rt))) {
        dat <- rt
        filter <- unique(dat$filter)
        if (identical(filter, "")) {
            if (is.null(key)) key <- filter
            lstdat <- list(dat)
        } else {
            if (is.null(key)) key <- filter
            lstdat <- lapply(
                key, function(i) subset(dat, filter == i))
        }
    } else {
        if (is.null(key)) key <- filter
        dat <- ts_filter(rt, by = by,
                         dtname = dtname,
                         txt = txt,
                         filter = filter,
                         key = key,
                         na.omit = na.omit,
                         trim = trim)
        lstdat <- lapply(
            key, function(i) subset(dat, filter == i))
    }
    ## if plot is false return ts data object
    if (!plot) return(dat)
    ## adjust left margin for larger frequencies
    if (max(dat[["freq"]], na.rm = TRUE) > 1000000) {
        mar.default <- c(2.425, 3.675, 0.750, 1.500)
    } else if (max(dat[["freq"]], na.rm = TRUE) > 100000) {
        mar.default <- c(2.425, 3.375, 0.750, 1.500)
    } else if (max(dat[["freq"]], na.rm = TRUE) > 10000) {
        mar.default <- c(2.425, 3.175, 0.750, 1.500)
    } else if (max(dat[["freq"]], na.rm = TRUE) > 1000) {
        mar.default <- c(2.425, 2.875, 0.750, 1.500)
    } else {
        mar.default <- c(2.425, 2.575, 0.750, 1.500)
    }
    if (missing(mar)) {
        mar <- mar.default
    }
    ## store current aesthetics
    op <- par(no.readonly = TRUE)
    oop <- options()
    options(scipen = 6)
    ## restore those values on exit
    on.exit(par(op))
    on.exit(options(oop), add = TRUE)

    ## sort out cex values
    if (missing(cex.main)) cex.main <- cex * 1.225
    if (missing(cex.sub)) cex.sub <- cex * 0.875
    if (missing(cex.lab)) cex.lab <- cex * 0.9
    if (missing(cex.axis)) cex.axis <- cex * 0.80
    if (missing(cex.legend)) cex.legend <- cex * 0.80

    ## if default mar provided then...
    if (identical(mar, mar.default)) {
        ## estimate width of right (legend side) margin
        ## if no filter then no legend/smaller margin
        if (is.null(filter)) {
            mar[4] <- 1.3
        } else {
            ## select biggest filter
            legmarg <- max(nchar(key), na.rm = TRUE)
            if (any(!is.numeric(legmarg),
                    isTRUE(legmarg < 3))) legmarg <- 3
            ## adjust accordingly
            mar[4] <- (legmarg + 10) * .29 * (1.25 * cex.legend)
        }
        ## top margin depends on whether main (title)
        if (!is.null(main)) {
            mar[3] <- 1.8
        } else {
            mar[3] <- .6
        }
        if (!is.null(subtitle)) mar[3] <- mar[3] + cex.sub * 1.1
    }

    ## base plot background
    if (theme %in% c("reverse", "inverse", 2)) {
        theme.bg <- "#f3f3f3"
    } else if (theme %in% c("gray", "grey", 5)) {
        theme.bg <- "#f0f0f0"
    } else if (theme %in% c("spacegray", "spacegray", 6)) {
        theme.bg <- "#414A4C"
        par(col.main = "white", col.lab = "white",
            col.axis = "white", col.sub = "white",
            col = "white")
        if (all(is.null(cols), is.null(filter))) cols <- "white"
    } else if (theme %in% c("apa", "APA", 8)) {
        theme.bg <- "#ffffff"
        ticks <- 1.5
        linetype <- TRUE
        if (is.null(filter)) {
            cols <- "black"
        } else {
            cols <- rep("black", length(filter))
        }
    } else if (theme %in% c("nerdy", "nerdier", "nerd", 4)) {
        if (all(is.null(cols), is.null(filter))) cols <- "#990022bb"
        theme.bg <- "#ffffff"
    } else {
        theme.bg <- "#ffffff"
    }
    ## set aesthetics
    par(tcl = -.125,
        cex = cex,
        font.main = font.main,
        cex.axis = cex.axis,
        cex.lab = cex.lab,
        cex.main = cex.main,
        cex.sub = cex.sub,
        lend = "butt",
        las = 1,
        bg = theme.bg,
        mgp = c(2, .2, 0),
        mar = mar)
    ## convert adj format if logical
    if (is.logical(adj)) {
        if (adj) {
            adj <- 0.0
        } else {
            adj <- 0.5
        }
    }
    if (!is.null(main)) {
        main.hold <- ""
    } else {
        main.hold <- NULL
    }
    ## init plot to get parameters and set scale
    with(dat, plot(time, freq,
                   type = "l",
                   lwd = 0,
                   axes = FALSE,
                   main = main.hold,
                   xlab = "",
                   ylab = "",
                   adj = adj,
                   ...))
    ## add xlab and ylab
    ## ylab location is nonlinear function of margin +
    ## and a linear function of cex
    mgp1 <- log10(mar.default[2]^2) * mar.default[2] * .5 + cex * .5
    title(ylab = ylab, mgp = c(mgp1, .25, 0))
    ## xlab is easier bc lower mar is constant
    title(xlab = xlab, mgp = c(1.4 * cex, .25, 0))
    ## add main title title
    if (!is.null(main)) {
        ## increase top margin for better fit
        tmp.mar <- par("mar")
        tmp.mar[2] <- tmp.mar[2] - .05
        if (tmp.mar[2] < 0) tmp.mar[2] <- 0
        if (!is.null(subtitle)) {
            tmp.mar[3] <- tmp.mar[3] - .67
        }
        if (tmp.mar[3] < 0) tmp.mar[3] <- 0
        par(mar = tmp.mar)
        ## add main title text
        title(main = main, mgp = c(2.7, .25, 0),
              adj = adj, cex = cex.main)
        ## add subtitle
        if (!is.null(subtitle)) {
            tmp.mar[3] <- tmp.mar[3] + .33
            par(mar = tmp.mar)
            mtext(subtitle,
                  adj = adj + .00001 * cex,
                  cex = cex.sub,
                  line = 0)
        }
        par(mar = mar)
    }
    ## draw axes
    if (axes) {
        x.ticks <- seq(
            par("xaxp")[1], par("xaxp")[2],
            length.out = par("xaxp")[3] + 1)
        x.labs <- as.POSIXct(x.ticks, origin = "1970-01-01")
        if (!is.null(xtime)) {
            axis.POSIXct(1, at = x.labs, format = xtime, hadj = 0,
                         lwd = 0, lwd.ticks = ticks)
        } else {
            axis.POSIXct(1, at = x.labs,
                         lwd = 0, lwd.ticks = ticks)
        }
        y.ticks <- seq(
            par("yaxp")[1], par("yaxp")[2],
            length.out = par("yaxp")[3] + 1)
        y.labs <- prettyNum(y.ticks, big.mark = ",",
                            preserve.width = "none")
        axis(2, at = y.ticks, y.labs,
             lwd = 0, lwd.ticks = ticks)
    }

    ## construct background grid aesthetic
    if (theme %in% c("light", "lighter", 1)) {
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#f5f5f5", border = NA)
        ## blend grid lines
        grid(col = "#f9f9f9", lwd = 5 * cex, lty = 1)
        grid(col = "#fcfcfc", lwd = 3 * cex, lty = 1)
        grid(col = "#ffffff", lwd = 1.5 * cex, lty = 1)
        grid(col = "#333333", lwd = .1 * cex, lty = 3)
        ## draw box
        if (box) {
            box(lwd = .75 * cex, col = "#333333")
        }
    } else if (theme %in% c("inverse", "reverse", 2)) {
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#ffffff", border = NA)
        ## blend grid lines
        grid(col = "#f3f3f3", lwd = .25  * cex, lty = 1)
        grid(col = "#333333", lwd = .1  * cex, lty = 3)
        ## draw box
        if (box) {
            box(lwd = 1.3 * cex, col = "#666666")
        }
    } else if (theme %in% c("dark", "darker", 3)) {
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#f3f3f3", border = NA)
        ## blend grid lines
        grid(col = "#333333", lwd = .1  * cex, lty = 1)
        ## draw box
        if (box) {
            box(lwd = 1.3 * cex, col = "#666666")
        }
    } else if (theme %in% c("nerdy", "nerd", "nerdier", 4)) {
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#eaeaea", lwd = .5 * cex,
             density = 15, angle = 90, border = NA)
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#eaeaea", lwd = .5 * cex,
             density = 15, angle = 0, border = NA)
        grid(col = "#0033aa4f", lwd = .75 * cex, lty = 1)
        ## draw box
        if (box) {
            box(lwd = 1.3 * cex, col = "#99002266")
        }
    } else if (theme %in% c("gray", "grey", 5)) {
        ## blend grid lines to white
        grid(col = "#333333", lwd = .1 * cex, lty = 1)
        ## draw box
        if (box) {
            box(lwd = 1.3 * cex, col = "#666666")
        }
    } else if (theme %in% c("spacegray", "spacegrey", 6)) {
        ## grid lines
        grid(col = "#f5f5f5", lwd = .4 * cex, lty = 3)
        ## draw box
        if (box) {
            box(lwd = .5 * cex, col = "#f5f5f5")
        }
    } else if (theme %in% c("minimal", "simple", 7)) {
        ## grid lines
        grid(col = "#666666", lwd = .3 * cex, lty = 2)
        ## draw box
        if (box) {
            box(lwd = 1.5 * cex, col = "#333333")
        }
    } else if (theme %in% c("apa", "APA", 8)) {
        ## draw box
        box(lwd = 1.5 * cex, col = "black")
        lwd <- lwd * cex
    }

    ## draw lines and add legend
    if (!is.null(filter)) {
        ## if colors undefined
        if (is.null(cols)) {
            ## includes random sample so jumpy colors
            if (theme %in% c("spacegray", "spacegrey", 6)) {
                lighter <- TRUE
            } else {
                lighter <- FALSE
            }
            cols <- rt_cols(length(filter), lighter = lighter)
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
        if (linetype) {
            ## iterate over xy lines
            seq_along(lstdat) %>%
                lapply(function(i)
                    with(lstdat[[i]],
                         lines(time, freq, lwd = lwd,
                               lty = as.double(i),
                               col = cols[[i]])))
        } else {
            ## iterate over xy lines
            seq_along(lstdat) %>%
                lapply(function(i)
                    with(lstdat[[i]],
                         lines(time, freq, lwd = lwd,
                               col = cols[i])))
        }
        xwide <- max(nchar(key), na.rm = TRUE)
        if (any(!is.numeric(xwide), isTRUE(xwide < 10))) xwide <- 10
        xwide <- par("usr")[2] + ((par("usr")[2] - par("usr")[1]) *
                  (.875 - par("plt")[2]) / xwide)
        if (!is.null(main)) {
            ytall <- quantile(c(par("usr")[3], par("usr")[4]),
                              .52 + .02 * length(filter) * cex)
        } else {
            ytall <- quantile(c(par("usr")[3], par("usr")[4]),
                              .48 + .02 * length(filter) * cex)
        }
        ## calculate x legend postion given par
        ## y legend position looks too short at median so
        ## use .575 quartile instead
        if (any(linetype, theme %in% c("apa", 8))) {
            legend(
                xwide, ytall, key,
                lwd = lwd * .9,
                col = cols,
                x.intersp = .5,
                title = legend.title,
                lty = seq_len(length(filter)),
                seg.len = 1.5,
                xpd = TRUE,
                bty = "n",
                cex = cex.legend)
        } else {
            legend(
                xwide, ytall, key,
                lwd = rep((1.1 * lwd), length(filter)),
                col = cols,
                x.intersp = .5,
                title = legend.title,
                lty = rep(1, length(filter)),
                seg.len = 1.5,
                xpd = TRUE,
                bty = "n",
                cex = cex.legend)
        }
    } else {
        if (is.null(cols)) cols <- "black"
        with(dat, lines(time, freq, lwd = lwd, col = cols))
    }

    ## return aggregated data
    invisible(dat)
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


#' mutate_coords
#'
#' Initializes rt plotting sequence
#'
#' @param data Data frame generated via rtweet function.
#' @param \dots Args passed to points.
#' @aliases mutate_coords
#' @export
mutate_coords <- function(data, ...) {
    with(mutate.coords(data), rtpoint(long, lat, ...))
}


#' ts plot
#'
#' Returns time series-like data frame.
#'
#' @param data Data frame or grouped data frame.
#' @param by Desired interval of time expressed as numeral plus secs,
#'   mins, hours, days, weeks, months, years. If a numeric is
#'   provided, the value is assumed to be in seconds.
#' @param group Name of grouping variable for use in plotting multiple
#'   time series. This is automatically configured if data is a
#'   grouped_df (dplyr's group_by function).
#' @param ... Other args passed to ggplot geom_line or, if ggplot2 is
#'   not installed, then to lines plotting function.
#' @return Either a ggplot plot object or an invisibly returned time
#'   series-like data frame.
#' @examples
#' \dontrun{
#' rt <- search_tweets("rstats", n = 10000)
#' ts_plot(rt, "10 mins")
#'
#' rt %>%
#'   dplyr::group_by(is_retweet) %>%
#'   ts_plot("hours")
#'
#' ## compare account activity for some important national political figures
#' tmls <- get_timeline(c("SenSchumer", "SenGillibrand", "realDonaldTrump"), n = 3000)
#' ## examine all twitter activity
#' ts_plot(tmls, "months")
#' ## group by screen name
#' ts_plot(dplyr::group_by(tmls, screen_name), "months") + theme_mwk()
#' ## group by screen name and is_retweet
#' ts_plot(dplyr::group_by(tmls, screen_name, is_retweet), "months") + theme_minimal()
#' }
#' @export
ts_plot <- function(data, by = "days", group = NULL, ...) {
  do.call("ts_plot_", list(data = data, by = by, group = group, ...))
}


#' @importFrom graphics legend
ts_plot_ <- function(data, by = "days", group = NULL, ...) {
  data <- ts_data(data, by, group = group, ...)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    if (ncol(data) == 3) {
      ggplot2::ggplot(
        data, ggplot2::aes_string(
          x = "time", y = "n", colour = names(data)[3])
      ) +
      ggplot2::geom_line(...)
    } else {
      ggplot2::ggplot(
        data, ggplot2::aes_string(x = "time", y = "n")) +
        ggplot2::geom_line(...)
    }
  } else {
    if (ncol(data) == 3) {
      cols <- ggcols(length(unique(data[[3]])))
      with(data, plot(time, n, col = cols, type = "n"))
      invisible(lapply(
        seq_along(unique(data[[3]])), function(i)
          with(data[data[[names(data)[3]]] == unique(data[[3]])[i], ],
               lines(time, n, col = cols[i], ...))
      ))
      legend("topleft", unique(data[[3]]), bty = "n", inset = 0,
             col = cols, lwd = rep(1, length(unique(data[[3]]))))
    } else {
      with(data, plot(time, n, ..., type = "l"))
      invisible(data)
    }
  }
}

#' @importFrom grDevices hcl
ggcols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Plots tweets data as a time series-like data object.
#'
#' Creates a ggplot2 plot of the frequency of tweets over a specified
#' interval of time.
#'
#' @param data Data frame or grouped data frame.
#' @param by Desired interval of time expressed as numeral plus secs,
#'   mins, hours, days, weeks, months, years. If a numeric is
#'   provided, the value is assumed to be in seconds.
#' @param trim The number of observatons to drop off the beginning and
#'   end of the time series.
#' @param ... Other args passed to ggplot geom_line or, if ggplot2 is
#'   not installed, then to lines plotting function.
#' @return If ggplot2 is installed then a ggplot2 plot
#' @examples
#' \dontrun{
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
#' ts_plot(dplyr::group_by(tmls, screen_name, is_retweet), "months")
#' }
#' @family ts_data
#' @export
ts_plot <- function(data, by = "days", trim = 0L, ...) {
  do.call("ts_plot_", list(data = data, by = by, trim = trim, ...))
}


#' @importFrom graphics legend
ts_plot_ <- function(data, by = "days", trim = 0L, ...) {
  data <- ts_data(data, by, trim)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
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
  } else {
    stop("please install ggplot2 to use ts_plot", call. = FALSE)
  }
}

#' @importFrom grDevices hcl
ggcols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

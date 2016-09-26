#' ts_plot
#'
#' @param x Tweets data frame
#' @param by Unit of time, e.g., \code{secs, days, weeks,
#'   months, years}
#' @param \dots Other arguments passed to plot function.
#'
#' @examples
#' \dontrun{
#' # stream tweets mentioning presidential debates hashtag
#' #   for 5 minutes
#' x <- stream_tweets(q = "debates2016", timeout = (60 * 5))
#' tsplot(x)
#' }
#' @importFrom graphics plot
#' @export
ts_plot <- function(x, by = "days", ...) {
	cut <- cut.POSIXt(x$created_at, breaks = by)
	x <- as.data.frame(table(cut))
	Time <- as.POSIXct(x$cut)
	Freq <- x$Freq
	plot(data.frame(Time, Freq), type = "l", ...)
}

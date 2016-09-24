#' tsplot
#'
#' @param x Tweets data frame
#' @param by Unit of time, e.g., \code{secs, days, weeks,
#'   months, years}
#' @param \dots Other arguments passed to plot function.
#'
#' @examples
#' \dontrun{
#' x <- stream_tweets()
#' tsplot(x)
#' }
#' @importFrom graphics plot
#' @export
tsplot <- function(x, by = "days", ...) {
	cut <- cut.POSIXt(x$created_at, breaks = by)
	x <- as.data.frame(table(cut))
	Time <- as.POSIXct(x$cut)
	Freq <- x$Freq
	plot(data.frame(Time, Freq), type = "l", ...)
}

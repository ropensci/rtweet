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
  if (is.atomic(x)) {
    dtm <- c("Date", "POSIXct", "POSIXt")
    if (!any(dtm %in% class(x))) {
      stop("ts_plot requires date/datetime object", call. = FALSE)
    }
  } else if (all(is.recursive(x), "created_at" %xy% x)) {
    x <- format_date(x[["created_at"]])
  }
	cut <- cut.POSIXt(x, breaks = by)
	if (length(unique(cut)) < 3) {
	  cut <- cut.POSIXt(x, breaks = "hours")
	  if (length(unique(cut)) < 3) {
	    cut <- cut.POSIXt(x, breaks = "secs")
	  }
	} else if (length(unique(cut)) > 300) {
	  cut <- cut.POSIXt(x, breaks = "3 days")
	  if (length(unique(cut)) < 300) {
	    cut <- cut.POSIXt(x, breaks = "weeks")
	  }
	}
	x <- as.data.frame(table(cut))
	Time <- as.POSIXct(x$cut)
	Freq <- x$Freq
	plot(data.frame(Time, Freq), type = "l", ...)
}

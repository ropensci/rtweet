#' get_trends
#'
#' @description Returns Twitter trends
#'
#' @param woeid Character, WOEID is a Yahoo! Where On
#'   Earth ID.
#' @param exclude Logical, indicating whether or not to exclude
#'   hashtags
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable tokens.
#'
#' @return Trend data for a given location.
#' @export
get_trends <- function(woeid, exclude = FALSE, token = NULL) {

	query <- "trends/place"

	stopifnot(is.atomic(woeid))

	token <- check_token(token, query)

	params <- list(
		id = woeid,
		exclude = exclude)

	url <- make_url(
		query = query,
		param = params)

	gt <- TWIT(get = TRUE, url, token)

	gt <- from_js(gt)

	if (parse) gt <- parse_trends(gt)

	gt
}

#' parse_trends
#'
#' @description Returns tibble data frame of trends data.
#'
#' @param x Nexted list fromJSON of trends data.
#'
#' @importFrom dplyr data_frame tbl_df bind_cols
#' @export
parse_trends <- function(x) {
	trends <- tbl_df(x$trends[[1]])
	rows <- nrow(trends)
	bind_cols(trends, data_frame(
		as_of = format_trend_date(rep(x$as_of, rows)),
		created_at = format_trend_date(rep(x$created_at, rows)),
		place = rep(x$locations[[1]]$name, rows),
		woeid = rep(x$locations[[1]]$woeid, rows)))
}


format_trend_date <- function(x, date = FALSE) {
	x <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ",
		tz = Sys.timezone())
	if (date) {
		x <- as.Date(x)
	}
	x
}

#' trends_available
#'
#' @description Returns Twitter trends based on requested WOEID.
#'
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable tokens.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
#'
#' @return Data frame with WOEIDs. WOEID is a Yahoo! Where On
#'   Earth ID.
#' @export
trends_available <- function(token = NULL, parse = TRUE) {

	query <- "trends/available"

	token <- check_token(token, query)

	url <- make_url(query = query,
		param = NULL)

	trd <- TWIT(get = TRUE, url, token)

	trd <- from_js(trd)

	if (parse) trd <- parse_trends_available(trd)

	trd
}

#' @importFrom dplyr bind_cols tbl_df
#' @export
parse_trends_available <- function(x) {
	bind_cols(tbl_df(x[names(x) != "placeType"]),
		tbl_df(x[["placeType"]]))
}

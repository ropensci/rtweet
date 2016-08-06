#' stream_tweets
#'
#' @description Returns public statuses via one of three methods
#'   described below. By default, this function deciphers which
#'   method is when processing the \code{stream} argument.
#'
#'   1. Filtering via a search-like query (up to 400 keywords)
#'   2. Tracking via vector of user ids (up to 5000 user_ids)
#'   3. Location via geo coordinates (1-360 degree location boxes)
#'
#' @param stream Character vector with desired phrases and keywords
#'   used to filter tweets, a comma separated list of desired
#'   user IDs to track, or a set of bounding boxes to track.
#' @param timeout Numeric specifying amount of time, in seconds,
#'   to leave connection open while streaming/capturing tweets.
#'   By default, this is set at 30 seconds.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param file_name Character with name of file. By default, this
#'   generates random file name and parses tweets.
#' @seealso \url{https://stream.twitter.com/1.1/statuses/filter.json}
#' @examples
#' \dontrun{
#' # stream tweets mentioning Hillary Clinton for 60 seconds
#' hrc <- stream_tweets(stream = "hillaryclinton", timeout = 60)
#' hrc
#'
#' # stream tweets mentioning Donald Trump for 60 seconds
#' djt <- search_tweets(q = "realdonaldtrump", timeout = 60)
#' djt
#' }
#'
#' @return Tweets data returned as a tibble data_frame
#' @import jsonlite
#' @export
stream_tweets <- function(stream, timeout = 30, token = NULL,
                          file_name = NULL) {

  if (is.null(token)) {
    token <- get_tokens()
    token <- fetch_tokens(token, "friends/ids")
  }

  if (missing(stream)) stop("Must include stream search call.")

  params <- stream_params(stream)

  url <- make_url(
    restapi = FALSE,
    "statuses/filter",
    param = params)

  if (is.null(file_name)) file_name <- tempfile(fileext = ".json")

  if (grepl(".json", file_name)) file_name <- paste0(file_name, ".json")

  if (!file.exists(file_name)) file.create(file_name)

  message(paste0("Streaming tweets for ", timeout, " seconds..."))

  TWIT(
    get = FALSE, url,
    config = token,
    timeout = timeout,
    filename = file_name)

  resp <- stream_in(
    file(file_name),
    verbose = FALSE)

  message(paste0("Collected ", nrow(resp), " tweets!"))

  if (is.null(file_name)) file.remove(file_name)

  if (nrow(resp) > 0) resp <- tweets_df(resp)

  resp
}

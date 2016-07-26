#' stream_tweets
#'
#' @description Returns public statuses that match one or more
#'   filter predicates. Multiple parameters may be specified which
#'   allows most clients to use a single connection to the Streaming
#'   API. Both GET and POST requests are supported, but GET requests
#'   with too many parameters may cause the request to be rejected
#'   for excessive URL length. Use a POST request to avoid
#'   long URLs.
#'   The track, follow, and locations fields should be considered
#'   to be combined with an OR operator. track=foo&follow=1234
#'   returns Tweets matching “foo” OR created by user 1234.
#'   The default access level allows up to 400 track
#'   keywords, 5,000 follow userids and 25 0.1-360 degree
#'   location boxes. If you need elevated access to the
#'   Streaming API, you can contact Gnip.
#'
#' @seealso \url{https://stream.twitter.com/1.1/statuses/filter.json}
#' @return json object
#' @param stream either follower A comma separated list of
#'   user IDs, indicating the users to return statuses for in
#'   the stream. See follow for more information. track
#'   Keywords to track. Phrases of keywords are specified by
#'   a comma-separated list. See track for more information.
#'   Or locations Specifies a set of bounding boxes to track.
#'   See locations for more information.
#' @param timeout numeric time in seconds to leave connection
#'   open while streaming/capturing tweets
#' @param token OAuth token (1.0 or 2.0)
#' @param delimited optional Specifies whether messages
#'   should be length-delimited. See delimited for more
#'   information.
#' @param stall_warnings optional Specifies whether stall
#'   warnings should be delivered. See stall_warnings for
#'   more information.
#' @param file_name character name of file. By defaut, this
#'   generates random file name and parses tweets.
#' @import jsonlite
#' @export
stream_tweets <- function(stream, timeout = 20, token = NULL,
                          delimited = FALSE, stall_warnings = FALSE,
                          file_name = NULL) {

  if (missing(stream)) stop("Must include stream search call.")

  stream <- unlist(trimws(unlist(strsplit(stream, ","))))

  if (!all(suppressWarnings(is.na(as.numeric(stream))))) {
    if (all(is.integer(as.integer(stream)))) {
      api_type <- "follow"
    } else {
      api_type <- "locations"
    }
  } else {
    api_type <- "track"
  }

  if (is.null(file_name)) file_name <- tempfile(fileext = ".json")

  if (is.null(token)) {
    token <- get_tokens()
    token <- fetch_tokens(token, "statuses/filter")
  }

  params <- list(api_type = stream)
  names(params) <- api_type

  url <- make_url(
    restapi = FALSE,
    "statuses/filter",
    param = params)

  file.create(file_name)

  TWIT(
    get = FALSE, url,
    config = token,
    timeout = timeout,
    filename = file_name)

  resp <- stream_in(file(file_name))

  if (is.null(file_name)) file.remove(file_name)

  parse_status(resp)
}

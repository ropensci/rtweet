#' search_tweets
#'
#' @description Returns a collection of relevant Tweets matching a
#'   specified query. Please note that Twitter’s search service and,
#'   by extension, the Search API is not meant to be an exhaustive
#'   source of Tweets. Not all Tweets will be indexed or made
#'   available via the search interface. In API v1.1, the response
#'   format of the Search API has been improved to return Tweet
#'   objects more similar to the objects you’ll find across the
#'   REST API and platform. However, perspectival attributes (fields
#'   that pertain to the perspective of the authenticating user) are
#'   not currently supported on this endpoint. To learn how to use
#'   Twitter Search effectively, consult our guide to Using the
#'   Twitter Search API. See Working with Timelines to learn best
#'   practices for navigating results by since_id and max_id.
#'
#' @param q required, A UTF-8, URL-encoded search query of 500
#'   characters maximum, including operators. Queries may
#'   additionally be limited by complexity.
#' @param geocode optional, Returns tweets by users located within a
#'   given radius of the given latitude/longitude. The location is
#'   preferentially taking from the Geotagging API, but will fall
#'   back to their Twitter profile. The parameter value is specified
#'   by “latitude,longitude,radius”, where radius units must be
#'   specified as either “mi” (miles) or “km” (kilometers). Note
#'   that you cannot use the near operator via the API to geocode
#'   arbitrary locations; however you can use this geocode parameter
#'   to search near geocodes directly. A maximum of 1,000 distinct
#'   “sub-regions” will be considered when using the radius modifier.
#' @param lang optional, Restricts tweets to the given language,
#'   givenby an ISO 639-1 code. Language detection is best-effort.
#' @param locale optional, Specify the language of the query you are
#'   sending (only ja is currently effective). This is intended for
#'   language-specific consumers and the default should work in the
#'   majority of cases.
#' @param result_type optional, Specifies what type of search results
#'   you would prefer to receive. The current default is “mixed.”
#'   Valid values include \code{'mixed'} to include both popular and
#'   real time results in the response, \code{'recent'} to return
#'   only the most recent results in the response, and
#'   \code{'popular'} to return only the most popular results in
#'   the response.
#' @param count optional, The number of tweets to return per page,
#'   up to a maximum of 100. Defaults to 15. This was formerly the
#'   “rpp” parameter in the old Search API.
#' @param until optional, Returns tweets created before the given
#'   date. Date should be formatted as YYYY-MM-DD. Keep in mind that
#'   the search index has a 7-day limit. In other words, no tweets
#'   will be found for a date older than one week.
#'   Example Values: \code{'2015-07-19'}.
#' @param since_id optional, Returns results with an ID greater than
#'   (that is, more recent than) the specified ID. There are limits
#'   to the number of Tweets which can be accessed through the API.
#'   If the limit of Tweets has occured since the since_id, the
#'   since_id will be forced to the oldest ID available.
#' @param max_id optional, Returns results with an ID less than
#'   (that is, older than) or equal to the specified ID.
#' @param include_entities optional, The entities node will be
#'   disincluded when set to false.
#' @param callback optional, If supplied, the response will use the
#'   JSONP format with a callback of the given name. The usefulness
#'   of this parameter is somewhat diminished by the requirement of
#'   authentication for requests to this endpoint.
#' @details The GET search/tweets is part of the Twitter REST API
#'   1.1 and is rate limited similarly to other v1.1 methods. See
#'   REST API Rate Limiting in v1.1 for information on that model.
#'   At this time, users represented by access tokens can make 180
#'   requests/queries per 15 minutes. Using application-only auth,
#'   an application can make 450 queries/requests per 15 minutes on
#'   its own behalf without a user context.
#' @param token OAuth token (1.0 or 2.0)
#' @seealso \url{https://api.twitter.com/1.1/search/tweets.json}
#' @return json object
#' @details dplyr
#' @export
search_tweets <- function(q, count = 100, result_type = "mixed",
                          token = NULL, ...) {

  if (is.null(token)) {
    token <- get_tokens()
    token <- fetch_tokens(token, "search/tweets")
  }

  params <- list(
    q = q,
    result_type = result_type,
    count = "100",
    cursor = "-1",
    ...)

  url <- make_url(restapi = TRUE, "search/tweets", param = params)

  tw_df <- dplyr::data_frame()
  nrows <- 0

  while (nrows < count) {
    qresp <- TWIT(get = TRUE, url, config = token)
    qresp <- from_js(qresp)

    qresp$statuses <- parse_status(qresp$statuses)

    tw_df <- dplyr::bind_rows(tw_df, qresp$statuses)
    tw_df <- tw_df[!is.na(tw_df$status_id), ]
    nrows <- nrow(tw_df)

    if (length(qresp$search_metadata$next_results) == 0) break
    if (qresp$search_metadata$next_results == 0) break

    cursor_url <- sub("[?]", "", qresp$search_metadata$next_results)
    url <- make_url(restapi = TRUE, "search/tweets", cursor_url)
  }

  tw_df
}

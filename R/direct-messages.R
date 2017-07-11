#' GET direct_messages¶
#'
#' Returns the 20 most recent direct messages sent to the authenticating
#'   user. Includes detailed information about the sender and recipient user.
#'   You can request up to 200 direct messages per call, and only the most
#'   recent 200 DMs will be available using this endpoint. Important: This
#'   method requires an access token with RWD (read, write & direct
#'   message) permissions.
#' @param since_id optional Returns results with an ID greater than (that is, more recent than) the
#'   specified ID. There are limits to the number of Tweets which can be accessed
#'   through the API. If the limit of Tweets has occured since the since_id, the
#'   since_id will be forced to the oldest ID available.
#' @param max_id optional Returns results with an ID less than (that is, older than) or equal to the
#'   specified ID.
#' @param count optional Specifies the number of direct messages to try and retrieve, up to a maximum of
#'   200. The value of count is best thought of as a limit to the number of Tweets
#'   to return because suspended or deleted content is removed after the count has
#'   been applied.
#' @return data
my_direct_messages <- function(since_id = NULL,
                               max_id = NULL,
                               count = 200,
                               token = NULL) {
  query <- "direct_messages"
  token <- check_token(token, query)
  params <- list(include_entities = FALSE)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  from_js(r)
}

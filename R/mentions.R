#' Get mentions for the authenticating user.
#'
#' Returns data on up to 200 of the most recent mentions (Tweets
#' containing a users's screen_name) of the authenticating user.
#'
#' @param n Specifies the number of Tweets to try and retrieve, up to
#'   a maximum of 200 (the default). The value of count is best
#'   thought of as a limit to the number of tweets to return because
#'   suspended or deleted content is removed after the count has been
#'   applied.
#' @param since_id Returns results with an ID greater than (that is,
#'   more recent than) the specified ID. There are limits to the
#'   number of Tweets which can be accessed through the API. If the
#'   limit of Tweets has occurred since the since_id, the since_id
#'   will be forced to the oldest ID available.
#' @param max_id Character, returns results with an ID less than (that is,
#'   older than) or equal to `max_id`.
#' @param parse Logical indicating whether to convert the response
#'   object into an R list. Defaults to TRUE.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @param ... Other arguments passed as parameters in composed API
#'   query.
#' @return Tibble of mentions data.
#' @details The timeline returned is the equivalent of the one seen
#'   when you view your mentions on twitter.com. This method can only
#'   return up to 800 tweets.
#' @family tweets
#' @examples
#'
#' \dontrun{
#'
#' ## get most recent 200 mentions of authenticating user
#' mymentions <- get_mentions()
#'
#' ## view data
#' mymentions
#'
#' }
#'
#' @seealso
#'   \url{https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-mentions_timeline}
#' @export
get_mentions <- function(n = 200,
                         since_id = NULL,
                         max_id = NULL,
                         parse = TRUE,
                         token = NULL,
                         ...) {
  args <- list(
    n = n,
    since_id = since_id,
    max_id = max_id,
    parse = parse,
    token = token,
    ...
  )
  do.call("get_mentions_", args)
}

get_mentions_ <- function(n = 100,
                          since_id = NULL,
                          max_id = NULL,
                          parse = TRUE,
                          token = NULL,
                          ...) {
  query <- "statuses/mentions_timeline"
  params <- list(
    count = n,
    since_id = since_id,
    max_id = max_id,
    ...
  )
  token <- check_token(token)
  message("Getting mentions for ", token_home_user(token))
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  if (parse) {
    r <- from_js(r)
    r <- as_mentions(r)
    r <- as.data.frame(r)
    if (has_name_(r, "created_at")) {
      r$created_at <- format_date(r$created_at)
    }
  }
  r
}

token_home_user <- function(token) {
  if (is.list(token)) {
    lapply(token, go_get_var, "credentials", "screen_name")
  } else {
    go_get_var(token, "credentials", "screen_name")
  }
}



as_mentions <- function(x) {
  structure(x, class = "mentions")
}

as.data.frame.mentions <- function(x) {
  out <- tibble::as_tibble(
    wrangle_into_clean_data(x, "status"))
  if (has_name_(x, "user")) {
    users <- tibble::as_tibble(
      wrangle_into_clean_data(x, "user"))
    attr(out, "users") <- users
  }
  out
}

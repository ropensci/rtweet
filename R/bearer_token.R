#' Bearer token
#'
#' @description 
#' Create a bearer token for application only (user-free) authentication.
#' A bearer token performs actions on behalf of the app, instead of behalf
#' of an individual user. This is useful because it provides more generous 
#' rate limits for some endpoints. For example, the rate limit for the 
#' standard search API is 18,000 tweets per fifteen minutes. With a bearer 
#' token, the rate limit rises to 45,000 tweets per fifteen minutes. See
#' <https://developer.twitter.com/en/docs/basics/rate-limits.html> for details.
#' 
#' Note that you should call this function once, save the result to variable,
#' then pass that object to the `token` argument of rtweet functions.
#' 
#' There is little point creating a bearer token with the default app because
#' the app rate limits are shared across all rtweet users. Instead, you'll 
#' need to create a custom app and token following the advice in 
#' [create_token()] and `vignette("auth")`.
#'
#' @param token A token created by [create_token()]
#' @return A bearer token
#' @examples
#' \dontrun{
#' token <- bearer_token(my_token)
#' ## use bearer token to search for >18k tweets (up to 45k) w/o hitting rate limit
#' verified_user_tweets <- search_tweets("filter:verified", n = 30000, token = token)
#'
#' ## get followers (user == app)
#' ### - USER (normal) token – rate limit 15req/15min
#' cnn_flw <- get_followers("cnn", n = 75000)
#' ### - APP (bearer) token – rate limit 15req/15min
#' cnn_flw <- get_followers("cnn", n = 75000, token = token)
#'
#' ## get timelines (user < app)
#' ### - USER (normal) token – rate limit 900req/15min
#' cnn_flw_data <- get_timelines(cnn_flw$user_id[1:900])
#' ### - APP (bearer) token – rate limit 1500req/15min
#' cnn_flw_data <- get_timelines(cnn_flw$user_id[1:1500], token = token)
#'
#' ## lookup statuses (user > app)
#' ### - USER (normal) token – rate limit 900req/15min
#' cnn_flw_data2 <- lookup_tweets(cnn_flw_data$status_id[1:90000])
#' ### - APP (bearer) token – rate limit 300req/15min
#' cnn_flw_data2 <- lookup_tweets(cnn_flw_data$status_id[1:30000], token = token)
#'
#' }
#' @export
bearer_token <- function(key, secret) {
  token <- create_bearer_token(key, secret)
  r <- httr::add_headers(Authorization = paste0("Bearer ", token))
  structure(r, bearer = token, class = c("rtweet_bearer", "request"))
}

#' @export
print.rtweet_bearer <- function(x, ...) {
  # Make it hard to accidentally reveal token
  cat("<bearer token>\n")
  invisible(x)
}

create_bearer_token <- function(key, secret) {
  r <- httr::POST(
    "https://api.twitter.com/oauth2/token",
    httr::add_headers(Authorization = paste0("Basic ", encode_keys(key, secret))),
    body = list(grant_type = "client_credentials")
  )
  check_status(r)
  
  json <- from_js(r)
  json$access_token
}

#' Invalidate bearer token
#'
#' @param token A bearer token
#' @keywords internal
#' @export
invalidate_bearer <- function(bearer, token) {
  if (!inherits(bearer, "rtweet_bearer")) {
    abort("`bearer` must be a bearer token")
  }
  token <- check_token(token)
  
  r <- httr::with_verbose(httr::POST("https://api.twitter.com/oauth2/invalidate_token",
    token,
    query = list(access_token = attr(bearer, "bearer"))
  ))
  check_status(r)
  r
}

# Helpers -----------------------------------------------------------------

encode_keys <- function(key, secret) {
  openssl::base64_encode(paste0(key, ":", secret))
}

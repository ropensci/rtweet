

encode_keys <- function(key, secret) {
  try_require("openssl")
  openssl::base64_encode(paste0(key, ":", secret))
}

create_bearer_token <- function(token = NULL) {
  ## Step 1: Encode consumer key and secret

  ## Get app keys from token
  if (is.null(token)) {
    token <- get_token()
  }
  ## get app keys
  app_keys <- encode_keys(get_app_key(token), get_app_secret(token))

  ## Step 2: Obtain a bearer token
  r <- httr::POST("https://api.twitter.com/oauth2/token",
    httr::add_headers(Authorization = paste0("Basic ", app_keys)),
    body = list(grant_type = "client_credentials"))
  httr::stop_for_status(r)
  bearer <- httr::content(r, encoding = "UTF-8")
  bearer_env <- new.env()
  assign(".bearer_env", bearer_env, envir = .state)
  assign("bearer", bearer, envir = bearer_env)
  invisible()
}

#' Bearer token
#'
#' Convert default token into bearer token for application only (user-free)
#' authentication method
#'
#' @param token Oauth token created via \code{\link{create_token}}. See details
#'   for more information on valid tokens.
#' @return A bearer token
#' @details \code{bearer_token()} will only work on valid tokens generated from
#'   a user-created Twitter app (requires a Twitter developer account; see
#'   \code{\link{create_token}} for more information). Unlike the default token
#'   returned by \code{create_token}, bearer tokens operate without any
#'   knowledge/information about the user context–meaning, bearer token requests
#'   cannot engage in user actions (e.g., posting tweets, reading DMs), and the
#'   information returned by Twitter will not include user-specific variables
#'   (e.g., if the user is following a certain account). The upside to this
#'   authentication method is that it can afford users with more generous rate
#'   limits. For example, the rate limit for the standard search API is 18,000
#'   tweets per fifteen minutes. With a bearer token, the rate limit is 45,000
#'   tweets per fifteen minutes. However, this is not true for all endpoints.
#'   For a breakdown/comparison of rate limits, see
#'   \url{https://developer.twitter.com/en/docs/basics/rate-limits.html}.
#'
#' @examples
#' \dontrun{
#' ## use bearer token to search for >18k tweets (up to 45k) w/o hitting rate limit
#' verified_user_tweets <- search_tweets("filter:verified", n = 30000, token = bearer_token())
#'
#' ## get followers (user == app)
#' ### - USER (normal) token – rate limit 15req/15min
#' cnn_flw <- get_followers("cnn", n = 75000)
#' ### - APP (bearer) token – rate limit 15req/15min
#' cnn_flw <- get_followers("cnn", n = 75000, token = bearer_token())
#'
#' ## get timelines (user < app)
#' ### - USER (normal) token – rate limit 900req/15min
#' cnn_flw_data <- get_timelines(cnn_flw$user_id[1:900])
#' ### - APP (bearer) token – rate limit 1500req/15min
#' cnn_flw_data <- get_timelines(cnn_flw$user_id[1:1500], token = bearer_token())
#'
#' ## lookup statuses (user > app)
#' ### - USER (normal) token – rate limit 900req/15min
#' cnn_flw_data2 <- lookup_tweets(cnn_flw_data$status_id[1:90000])
#' ### - APP (bearer) token – rate limit 300req/15min
#' cnn_flw_data2 <- lookup_tweets(cnn_flw_data$status_id[1:30000], token = bearer_token())
#'
#' }
#' @export
bearer_token <- function(token = NULL) {
  bearer <- get_bearer(token)
  if (is.null(bearer)) {
    stop("couldn't find bearer token")
  }
  r <- httr::add_headers(Authorization = paste0("Bearer ", bearer$access_token))
  structure(r, bearer = bearer, class = c("bearer", "list"))
}


get_bearer <- function(token = NULL) {
  ## create if not already
  if (!exists(".bearer_env", envir = .state)) {
    create_bearer_token(token)
  }
  ## retrieve
  bearer_env <- get(".bearer_env", envir = .state)
  bearer <- tryCatch(get("bearer", envir = bearer_env), error = function(e) NULL)
  if (is.null(bearer)) {
    stop("couldn't find bearer token")
  }
  bearer
}

get_app_key <- function(token) {
  token$app$key
}

get_app_secret <- function(token) {
  token$app$secret
}

get_oauth_key <- function(token) {
  token$credentials$oauth_token
}

get_oauth_secret <- function(token) {
  token$credentials$oauth_token_secret
}

print.bearer <- function(bearer) {
  cat(paste0("Bearer token: ", attr(bearer, "bearer")$access_token), fill = TRUE)
}

#' Invalidate bearer token
#'
#' @param token Oauth token created via \code{\link{create_token}}.
#' @keywords internal
#' @export
invalidate_bearer <- function(token = NULL) {
  if (is.null(token)) {
    token <- get_token()
  }
  bearer <- bearer_token(token)
  bearer_token <- attr(bearer, "bearer")
  ## Get app keys from token
  app_keys <- encode_keys(get_app_key(token), get_app_secret(token))
  r <- httr::POST("https://api.twitter.com/oauth2/invalidate_token",
    httr::add_headers(Authorization = paste0("Basic ", app_keys)),
    body = list(access_token = bearer_token$access_token),
    encode = "form")
  httr::warn_for_status(r)
  r
}

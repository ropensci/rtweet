#' Bearer token
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `bearer_token()` has been deprecated because it used a rather indirect
#' method of generating a bearer token. Instead, use [rtweet_app()], copying
#' in the bearer token directly from the
#' [Twitter developer portal](https://developer.twitter.com/en/portal/projects-and-apps).
#' See `vignette("auth", package = "rtweet")` for full details.
#' @keywords internal
#' @export
bearer_token <- function(token = NULL) {
  lifecycle::deprecate_stop("1.0.0", "bearer_token()", "rtweet_app()")
}

#' Invalidate bearer token
#'
#' Invalidate the bearer token automatically if you know the API key and API secret.
#'
#' @note Not tested!
#' @inheritParams rtweet_user
#' @param token Expert use only. Use this to invalidate a specific bearer token
#' created with [rtweet_app()]. If `NULL` the default authentication mechanism is invalidated.
#' @references <https://developer.twitter.com/en/docs/authentication/api-reference/invalidate_bearer_token>
#' <https://developer.twitter.com/en/docs/authentication/api-reference/invalidate_access_token>
#' @keywords internal
#' @export
invalidate_bearer <- function(api_key, api_secret, client = NULL, token = NULL) {

  if (is.null(client)) {
    client <- client_as(client)
    api_key <- client["id"]
    api_secret <- client["secret"]
  } else {
    stopifnot(is_string(api_key), is_string(api_secret))
  }
  if (is.null(client) && missing(api_key)) {
    api_key <- ask_pass("API key")
  }
  if (is.null(client) &&  missing(api_secret)) {
    api_key <- ask_pass("API secret")
  }
  token <- check_token_v2(token)
  req1 <- httr2::request("https://api.twitter.com/oauth2/invalidate_token")
  req2 <- httr2::req_url_query(req1, access_token = token)
  req3 <- httr2::req_method(req2, "POST")
  req4 <- httr2::req_auth_basic(req3, api_key, api_secret)
  resp <- httr2::req_perform(req4)
  resp
}

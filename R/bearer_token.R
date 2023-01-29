#' Bearer token
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `bearer_token()` has been deprecated because it used a rather indirect
#' method of generating a bearer token. Instead, use [rtweet_app()], copying
#' in the bearer token directly from the
#' [Twitter developer portal](https://developer.twitter.com/en/portal/projects-and-apps).
#' See `vignette("auth")` for full details.
#' @keywords internal
#' @export
bearer_token <- function(token = NULL) {
  lifecycle::deprecate_stop("1.0.0", "bearer_token()", "rtweet_app()")
}

#' Invalidate bearer token
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Invalidation of the bearer token is no longer the responsibility of rtweet.
#' This is something you should instead perform in the [Twitter developer
#' portal](https://developer.twitter.com/en/portal/projects-and-apps).
#' @inheritParams rtweet_user
#' @references <https://developer.twitter.com/en/docs/authentication/api-reference/invalidate_bearer_token>
#' <https://developer.twitter.com/en/docs/authentication/api-reference/invalidate_access_token>
#' @keywords internal
#' @export
invalidate_bearer <- function(api_key, api_secret, token = NULL) {
  lifecycle::deprecate_stop("1.0.0", "invalidate_bearer()")

  token <- auth_get(token)

  if (missing(api_key)) {
    api_key <- ask_pass("API key")
  }
  if (missing(api_secret)) {
    api_key <- ask_pass("API secret")
  }
  httr2::request("https://api.twitter.com/oauth2/invalidate_token") |>
    httr2::req_url_query(access_token = token) |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(api_key, api_secret_key) |>
    httr2::req_perform()
}

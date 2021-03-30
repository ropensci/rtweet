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
#' 
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
#' 
#' @keywords internal
#' @export
invalidate_bearer <- function(token = NULL) {
  lifecycle::deprecate_stop("1.0.0", "invalidate_bearer()")
}

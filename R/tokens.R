#' Fetch Twitter OAuth token
#' 
#' @description 
#' `r lifecycle::badge("deprecated")`
#' Please use [auth_get()] instead.
#'
#' @keywords internal
#' @family tokens
#' @export
get_token <- function() {
  lifecycle::deprecate_warn("1.0.0", "get_token()", "auth_get()")
  auth_get()
}

#' @export
#' @rdname get_token
get_tokens <- function() {
  lifecycle::deprecate_warn("1.0.0", "get_tokens()", "auth_get()")
  auth_get()
}

#' Create custom Twitter OAuth token
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' By default, `create_token()` does three things: it creates an authentication 
#' "token", sets it as the default token for the current session, and save it
#' to disk to make it the default for future sessions.
#' 
#' These three components have now been split up into three separate pieces:
#' use [auth_user()]/[auth_app()]/[auth_bot()] to create the token,
#' `auth_use()` to make it the default for this session, and `auth_save()` to
#' use it in future sessions.
#' 
#' See `vignette("auth")` for more details.
#' 
#' @param app Name of user created Twitter application
#' @param consumer_key,consumer_secret App API key and secret.
#' @param access_token,access_secret Access token and secret.
#' @param set_renv Should the token be cached? 
#' @seealso
#'   <https://developer.twitter.com/en/docs/basics/authentication/overview/oauth>
#'
#' @return Twitter OAuth token(s) (Token1.0).
#' @keywords internal
#' @family tokens
#' @export
create_token <- function(app = "mytwitterapp",
                         consumer_key = NULL,
                         consumer_secret = NULL,
                         access_token = NULL,
                         access_secret = NULL,
                         set_renv = TRUE) {

  if (!is.null(access_token) && !is.null(access_secret)) {
    lifecycle::deprecate_warn("1.0.0", "create_token()", "auth_bot()")
    token <- auth_bot(consumer_key, consumer_secret, access_token, access_secret)
  } else {
    lifecycle::deprecate_warn("1.0.0", "create_token()", "auth_user()")
    token <- auth_user(consumer_key, consumer_secret)
  }
  
  if (set_renv) {
    lifecycle::deprecate_warn("1.0.0", "create_token(set_renv)", "auth_save()")
    auth_use(token)
    auth_save(token, "default")
  }
  
  token
}

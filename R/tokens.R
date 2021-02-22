check_token <- function(token = NULL) {
  token <- token %||% get_token()
  
  if (inherits(token, "bearer") || inherits(token, "Token1.0")) {
    return(token)
  }
  
  stop("`token` is not a valid access token", call. = FALSE)
}

#' Fetch Twitter authorization token
#' 
#' @description 
#' Call function used to fetch and load Twitter OAuth tokens.
#' Since Twitter application key should be stored privately, users should save
#' the path to token(s) as an environment variable. This allows Tokens
#' to be instantly [re]loaded in future sessions. See the tokens
#' vignette i.e.,`vignettes("auth", "rtweet")` for instructions on 
#' obtaining and using access tokens.
#'
#' @return Twitter OAuth token(s) (Token1.0).
#' @examples
#' \dontrun{
#' token <- get_token()
#' token
#' }
#' @family tokens
#' @export
get_token <- function() {
  if (is.null(.state$token)) {
    .state$token <<- load_cached_token() %||% default_token() %||% no_token()
  }

  .state$token
}

#' @export
#' @rdname get_token
get_tokens <- function() {
  # TODO: deprecate this function since there's only ever one token
  get_token()
}

load_cached_token <- function() {
  path <- token_cache_path()
  if (!file.exists(path)) {
    return()
  }
  
  message("Reading token from ", path)
  readRDS(path)
}
save_cached_token <- function(token) {
  path <- token_cache_path()
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  
  .state$token <- token # also save to memory cache
  message("Saving token to ", path)
  saveRDS(token, path)
}
delete_cached_token <- function() {
  unlink(token_cache_path())
}
token_cache_path <- function() {
  file.path(rappdirs::user_cache_dir("rtweet", "R"), "auth.rds")
}

default_token <- function() {
  # Requires user interaction
  if (!interactive()) {
    return(NULL)
  }
  
  key <- rawToChar(openssl::rsa_decrypt(sysdat$DYKcJfBkgMnGveI[[2]], sysdat$DYKcJfBkgMnGveI[[1]]))
  secret <- rawToChar(openssl::rsa_decrypt(sysdat$MRsnZtaKXqGYHju[[2]], sysdat$MRsnZtaKXqGYHju[[1]]))
  create_token("rstats2twitter", key, secret)
}

no_token <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    skip("Auth not available")
  } else {
    stop("Could not authenticate", call. = FALSE)
  }
}

#' Create custom Twitter authorization token
#'
#' @description Sends request to generate OAuth 1.0 tokens. Twitter
#'   also allows users to create user-only (OAuth 2.0) access token.
#'   Unlike the 1.0 tokens, OAuth 2.0 tokens are not at all centered
#'   on a host user. Which means these tokens cannot be used to send
#'   information (follow requests, Twitter statuses, etc.).  If you
#'   have no interest in those capabilities, then 2.0 OAuth tokens do
#'   offer some higher rate limits. At the current time, the
#'   difference given the functions in this package is trivial, so I
#'   have yet to verified OAuth 2.0 token method.  Consequently, I
#'   encourage you to use 1.0 tokens.
#' @param app Name of user created Twitter application
#' @param consumer_key Application API key
#' @param consumer_secret Application API secret User-owned
#'   application must have \code{Read and write} access level and
#'   \code{Callback URL} of \code{http://127.0.0.1:1410}.
#' @param access_token Access token as supplied by Twitter (apps.twitter.com)
#' @param access_secret Access secret as supplied by Twitter (apps.twitter.com)
#' @param set_renv Should the token be cached? 
#' @seealso
#'   \url{https://developer.twitter.com/en/docs/basics/authentication/overview/oauth}
#'
#' @return Twitter OAuth token(s) (Token1.0).
#' @importFrom httr oauth_app oauth1.0_token oauth_endpoints
#' @family tokens
#' @export
create_token <- function(app = "mytwitterapp",
                         consumer_key,
                         consumer_secret,
                         access_token = NULL,
                         access_secret = NULL,
                         set_renv = TRUE) {

  stopifnot(is.character(app))
  check_key(consumer_key, "`consumer_key`")
  check_key(consumer_secret, "`consumer_secret`")

  app <- httr::oauth_app(
    appname = app,
    key = consumer_key,
    secret = consumer_secret
  )
  
  ## if access token/secret use sign method otherwise browser
  if (!is.null(access_token) && !is.null(access_secret)) {
    stopifnot(is.character(access_token), is.character(access_secret))
    credentials <- list(
      oauth_token = access_token,
      oauth_token_secret = access_secret
    )
    token <- httr::Token1.0$new(
      app = app,
      endpoint = httr::oauth_endpoints("twitter"),
      params = list(as_header = TRUE), 
      credentials = credentials, 
      cache_path = FALSE
    )
  } else {
    token <- TwitterToken1.0$new(
      app = app,
      endpoint = httr::oauth_endpoints("twitter"),
      cache_path = FALSE
    )
  }
  
  if (set_renv) {
    save_cached_token(token)
  }
  
  token
}

check_key <- function(x, name) {
  if (!is.character(x) || length(x) != 1) {
    stop(name, " must be a string", call. = FALSE)
  } 
  
  x <- trimws(x)  
  if (grepl("[^[:alnum:]]", x)) {
    stop(name, " must only contain numbers and letters", call. = FALSE)
  }

  x
}

# Twitter Token -----------------------------------------------------------

# Twitter requires a callback url that uses 127.0.0.1 rather than localhost
# so we temporarily override HTTR_SERVER during initialisation.

TwitterToken1.0 <- R6::R6Class("TwitterToken1.0", inherit = httr::Token1.0, list(
  init_credentials = function(force = FALSE) {
    self$credentials <- twitter_init_oauth1.0(
      self$endpoint, 
      self$app,
      permission = self$params$permission,
      private_key = self$private_key
    )
  }
))

twitter_init_oauth1.0 <- function (endpoint, app, permission = NULL,
                                   is_interactive = interactive(),
                                   private_key = NULL) {
  
  withr::local_envvar("HTTR_SERVER" = "127.0.0.1")
  httr::init_oauth1.0(
    endpoint, 
    app, 
    permission = permission, 
    is_interactive = is_interactive, 
    private_key = private_key
  )
}

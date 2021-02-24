check_token <- function(token = NULL) {
  token <- token %||% get_token()
  
  if (inherits(token, "bearer") || inherits(token, "Token1.0")) {
    return(token)
  }
  
  stop("`token` is not a valid access token", call. = FALSE)
}

#' Fetch Twitter OAuth token
#' 
#' @description 
#' This function retrieves a cached OAuth token. This can be set to your
#' own custom app with \code{\link{create_token}}, but in most cases you
#' can use the built-in app, which this function will use by default. 
#' This function will error if no usable tokens are found.
#' 
#' See `vignettes("auth")` for more details. 
#'
#' @return Twitter OAuth token(s) (Token1.0).
#' @keywords internal
#' @examples
#' \dontrun{
#' get_token()
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
  access_token <- Sys.getenv("RTWEET_ACCESS_TOKEN")
  access_secret <- Sys.getenv("RTWEET_ACCESS_SECRET")
  
  key <- rawToChar(openssl::rsa_decrypt(sysdat$DYKcJfBkgMnGveI[[2]], sysdat$DYKcJfBkgMnGveI[[1]]))
  secret <- rawToChar(openssl::rsa_decrypt(sysdat$MRsnZtaKXqGYHju[[2]], sysdat$MRsnZtaKXqGYHju[[1]]))
  
  if (identical(key, "") || identical(access_secret, "")) {
    # Requires user interaction
    if (!interactive()) {
      return(NULL)
    }
    
    create_token("rstats2twitter", key, secret)
  } else {
    message("Using auth via TWITTER_ACCESS_TOKEN/TWITTER_ACCESS_SECRET")
    create_token("rstats2twitter", key, secret, access_token = access_token, access_secret = access_secret)
  }
}

no_token <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    skip("Auth not available")
  } else {
    stop("Could not authenticate", call. = FALSE)
  }
}

#' Create custom Twitter OAuth token
#'
#' @description 
#' This function allows you to use your own Twitter app, rather than the 
#' one built-in into rtweet. You will need to do this if you want to create
#' tweets or send/receive direct messages.
#' 
#' See `vignette("auth")` for more details.
#' 
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

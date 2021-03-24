#' Authentication options
#' 
#' @description 
#' There are three ways that you can authenticate with the twitter API:
#' 
#' * `auth_user()` interactively authenticates an existing twitter user. 
#'   This form is most appropriate if you want rtweet to control your
#'   twitter account.
#'   
#' * `auth_app()` authenticates as a twitter application. An application can't 
#'    perform actions (i.e. it can't tweet) but otherwise has generally higher 
#'    rate limits (i.e. you can do more searches) generally higher. This form is 
#'    most appropriate if you are collecting data.
#'    
#' * `auth_bot()` authenticates as bot that takes actions on behalf of an app.
#'    This form is most appropriate if you want to create a twitter account that
#'    is run by a computer, rather than a human.
#'    
#' To use `auth_app()` or `auth_bot()` you will need to create your own Twitter
#' application following the instructions in `vignette("auth.Rmd")`.
#' 
#' See [use_auth()] to set an auth mechanism as default for this session, and
#' [save_auth()] to save an authentication mechanism so you can use it across
#' multiple sessions.
#' 
#' @param api_key,api_secret App API key and secret. These are generally not
#'   required for `auth_user()` since if not supplied it will use the built-in
#'   rtweet app. 
#' @param access_token,access_secret Access token and secret. Together with
#'   `api_key` and `api_secret` these give full control over your app, so 
#'   should not be stored in source code. Instead store them in environment
#'   variables and retrieve with [Sys.getenv()]
#' @param bearer_token App bearer token.
#' @export
auth_user <- function(api_key = NULL, api_secret = NULL) {
  if (is.null(api_key) && is.null(api_secret)) {
    decrypt <- function(x) {
      rawToChar(openssl::rsa_decrypt(x[[2]], x[[1]]))
    }
    api_key <- decrypt(sysdat$DYKcJfBkgMnGveI)
    api_secret <- decrypt(sysdat$MRsnZtaKXqGYHju)
  } else {
    stopifnot(is_string(api_key), is_string(api_secret))
  }

  app <- httr::oauth_app("rtweet", key = api_key, secret = api_secret)
  TwitterToken1.0$new(
    app = app,
    endpoint = httr::oauth_endpoints("twitter"),
    params = list(as_header = TRUE), 
    cache_path = FALSE
  )
}

#' @export
#' @rdname auth_user
auth_bot <- function(api_key, api_secret, access_token, access_secret) {
  stopifnot(is_string(api_key), is_string(api_secret))
  stopifnot(is_string(access_token), is_string(access_secret))

  app <- httr::oauth_app("rtweet", key = api_key, secret = api_secret)
  credentials <- list(
    oauth_token = access_token,
    oauth_token_secret = access_secret
  )
  httr::Token1.0$new(
    app = app,
    endpoint = httr::oauth_endpoints("twitter"),
    params = list(as_header = TRUE), 
    credentials = credentials, 
    cache_path = FALSE
  )
}

#' @export
#' @rdname auth_user
auth_app <- function(bearer_token) {
  structure(
    list(token = bearer_token),
    class = "rtweet_bearer"
  )
}

is_auth <- function(x) {
  inherits(x, "Token") || inherits(x, "rtweet_auth")
}

#' @export
print.rtweet_bearer <- function(x, ...) {
   # Make it hard to accidentally reveal token
   cat("<twitter bearer token>\n")
   invisible(x)
}

# Get default auth --------------------------------------------------------

#' Get the current authentication mechanism
#' 
#' If no authentcation has been set up for this session, will call [use_auth()]
#' to set up.
#' 
#' @keywords internal
#' @export
get_auth <- function() {
  if (is.null(.state$auth)) {
    use_auth()
  }
  .state$auth
}

# Save authentication across sessions -------------------------------------

#' Save an authentication mechanism for use in a future session
#' 
#' You can use `save_auth()` and [use_auth()] to avoid accidentally exposing
#' credentials in your R scripts, by caching them in a common place.
#' 
#' @param auth One of [auth_app()], [auth_bot()], or [auth_user()]
#' @param name Cache name to use
#' @export
#' @examples 
#' \dontrun{
#' # save app auth for use in other sessions
#' save_auth(app_app("my-secret-bearer-token"), "my-app")
#' 
#' # later, in a different session...
#' use_auth("my-app")
#' }
save_auth <- function(auth, name) {
  stopifnot(is_auth(auth), is_string(name))
  
  path <- auth_path(paste0(name, ".rds"))
  inform("Saving auth to '", path, "'")
  
  saveRDS(auth, path)
  invisible(path)
}

auth_path <- function(...) {
  file.path(rappdirs::user_config_dir("rtweet", "R"), ...)
}

# Set default auth -------------------------------------------------------------

#' Set default authentication
#' 
#' `use_auth()` sets up the default authentication mechanism used by all 
#' rtweet API calls. See [auth_user()] for to learn more about the three
#' available authentication options.
#' 
#' @param auth One of the following options:
#'   * `NULL`, the default, will look for cached authentication from a previous
#'      session. If none are found, will automatically set up [auth_user()];
#'      if one is found, will use that; if multiple are found will error.
#'   * A string giving the name of a cached auth file.
#'   * An object created by [auth_app()], [auth_bot()], or [auth_user()].
#' @return Invisibly returns the previous auth mechanism.
#' @examples 
#' \dontrun{
#' # Use app auth for the remainder of this session:
#' use_auth(auth_app("my-secret-bearer-token"))
#' 
#' # Switch back to the default
#' use_auth()
#' 
#' # Load auth saved by save_auth()
#' use_auth("my-body")
#' }
use_auth <- function(auth = NULL) {
  old <- .state$auth
  .state$auth <- find_auth(auth)
  invisible(old)
}

find_auth <- function(auth = NULL) {
  if (is.null(auth)) {
    if (is_testing()) {
      auth_test() %||% no_token()
    } else if (is_dev_mode()) {
      auth_test() %||% default_cached_auth()
    } else{
      default_cached_auth()
    }
  } else if (is_auth(auth)) {
    auth
  } else if (is_string(auth)) {
    path <- auth_path(paste0(auth, ".rds"))
    if (!file.exists(path)) {
      abort(paste0("Can't find saved auth with name '", auth, "'"))
    }
    readRDS(path)
  } else {
    abort("Unrecognised input to `auth`")
  }
}

default_cached_auth <- function(path = auth_path()) {
  options <- dir(path, full.names = TRUE, pattern = "\\.rds$")
  
  if (length(options) == 0) {
    inform("No saved authentication found; creating...")
    save_auth(auth_user(), "default")
  } else if (length(options) == 1) {
    readRDS(options)
  } else {
    names <- tools::file_path_sans_ext(basename(options))
    abort(c(
      "Multiple saved tokens",
      "Pick the token you want with `use_auth()`",
      paste0("Options are ", paste0("'", names, "'", collapse = ", "))
    ))
  }
}

no_token <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    testthat::skip("Auth not available")
  } else {
    stop("Could not authenticate", call. = FALSE)
  }
}

auth_test <- function() {
  access_token <- Sys.getenv("RTWEET_ACCESS_TOKEN")
  access_secret <- Sys.getenv("RTWEET_ACCESS_SECRET")
  
  if (identical(access_token, "") || identical(access_secret, "")) {
    return()
  }

  auth_bot(
    "7rX1CfEYOjrtZenmBhjljPzO3",
    "rM3HOLDqmjWzr9UN4cvscchlkFprPNNg99zJJU5R8iYtpC0P0q",
    access_token,
    access_secret
  )
}

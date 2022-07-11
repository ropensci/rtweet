#' Set up default authentication
#' 
#' You'll need to run this function once per computer so that rtweet can use 
#' your personal Twitter account. See [rtweet_app()]/[rtweet_bot] and 
#' [auth_save()] for other authentication options.
#' 
#' It will use the current logged in account on the default browser to detect 
#' the credentials needed for rtweet and save them as "default". 
#' If a default is found it will use it instead. 
#' @return 
#' `auth_setup_default()`: Invisibly returns the previous authentication mechanism.
#' `auth_has_default()`: A logical value `TRUE` if there is a default authentication.
#' @export
#' @family authentication
#' @examples 
#' \dontrun{
#' if (!auth_has_default() && interactive()) {
#'    auth_setup_default()
#' }
#' }
auth_setup_default <- function() {
  if (auth_has_default()) {
    inform("Using default authentication available.")
  } else {
    auth <- rtweet_user()
    auth_save(auth, "default")
  }
  auth_as("default")
}

#' Authentication options
#' 
#' Authenticate methods to use the Twitter API.
#' 
#' @description 
#' There are three ways that you can authenticate with the Twitter API:
#' 
#' * `rtweet_user()` interactively authenticates an existing Twitter user. 
#'   This form is most appropriate if you want rtweet to control your
#'   Twitter account. 
#'   
#' * `rtweet_app()` authenticates as a Twitter application. An application can't 
#'    perform actions (i.e. it can't tweet) but otherwise has generally higher 
#'    rate limits (i.e. you can do more searches). See details
#'    at <https://developer.twitter.com/en/docs/twitter-api/v1/rate-limits>.
#'    This form is most appropriate if you are collecting data. 
#'    
#' * `rtweet_bot()` authenticates as bot that takes actions on behalf of an app.
#'    This form is most appropriate if you want to create a Twitter account that
#'    is run by a computer, rather than a human.
#'    
#' To use `rtweet_app()` or `rtweet_bot()` you will need to create your own 
#' Twitter app following the instructions in `vignette("auth.Rmd")`.
#' `rtweet_user()` _can be_ used with your own app, but generally there is
#' no need to because it uses the Twitter app provided by rtweet.
#' 
#' Use [auth_as()] to set the default auth mechanism for the current session, 
#' and [auth_save()] to save an auth mechanism for use in future sessions.
#' 
#' # Security
#' 
#' All of the arguments to these functions are roughly equivalent to 
#' passwords so should generally not be typed into the console (where they
#' the will be recorded in `.Rhistory`) or recorded in a script (which is
#' easy to accidentally share). Instead, call these functions without arguments
#' since the default behaviour is to use ask_pass that if possible uses 
#' [askpass::askpass()] to interactively safely prompt you for the values.
#' 
#' @param api_key,api_secret Application API key and secret. These are 
#'   generally not required for `tweet_user()` since the defaults will use
#'   the built-in rtweet app. 
#' @param access_token,access_secret Access token and secret.
#' @param bearer_token App bearer token.
#' @family authentication
#' @export
#' @examples 
#' \dontrun{
#' rtweet_user()
#' rtweet_bot()
#' rtweet_app()
#' }
rtweet_user <- function(api_key = NULL, api_secret = NULL) {
  check_installed("httpuv")
  
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
#' @rdname rtweet_user
rtweet_bot <- function(api_key, api_secret, access_token, access_secret) {
  
  if (missing(api_key)) {
    api_key <- ask_pass("API key")
  }
  if (missing(api_secret)) {
    api_secret <- ask_pass("API secret")
  }
  if (missing(access_token)) {
    access_token <- ask_pass("access token")
  }
  if (missing(access_secret)) {
    access_secret <- ask_pass("access token")
  }
  
  
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
#' @rdname rtweet_user
rtweet_app <- function(bearer_token) {
  if (missing(bearer_token)) {
    bearer_token <- ask_pass("bearer token")
  }
  structure(
    list(token = bearer_token),
    class = "rtweet_bearer"
  )
}

ask_pass <- function(type) {
  check_installed("askpass")
  
  message <- paste0("Please enter your ", type, ": ")
  val <- askpass::askpass(message)
  if (is.null(val)) {
    abort("Cancelled by user")
  }
  val
}

is_auth <- function(x) {
  inherits(x, "Token") || inherits(x, "rtweet_bearer")
}

#' @export
print.rtweet_bearer <- function(x, ...) {
   # Make it hard to accidentally reveal token
   cat("<Twitter bearer token>\n")
   invisible(x)
}

# Get default auth --------------------------------------------------------

#' Get the current authentication mechanism
#' 
#' If no authentication has been set up for this session, `auth_get()` will 
#' call [auth_as()] to set it up.
#' @return  The current token used. 
#' @export
#' @family authentication
#' @examples 
#' \dontrun{
#' auth_get()
#' }
auth_get <- function() {
  if (is.null(.state$auth)) {
    auth_as()
  }
  .state$auth
}

# Save authentication across sessions -------------------------------------

#' Save an authentication mechanism for use in a future session
#' 
#' Use `auth_save()` with [auth_as()] to avoid repeatedly entering app 
#' credentials, making it easier to share auth between projects.
#' Use `auth_list()` to list all saved credentials.
#' 
#' The tokens are saved on `tools::R_user_dir("rtweet", "config")`. 
#' 
#' @param auth One of [rtweet_app()], [rtweet_bot()], or [rtweet_user()].
#' @param name Name of the file to use.
#' @return Invisible the path where the authentication is saved.
#' @family authentication
#' @seealso [auth_sitrep()] to help finding and managing authentications.
#' @export
#' @examples 
#' \dontrun{
#' # save app auth for use in other sessions
#' auth <- rtweet_app()
#' auth_save(auth, "my-app")
#' 
#' # later, in a different session...
#' auth_as("my-app")
#' # Show all authentications stored
#' auth_list()
#' }
auth_save <- function(auth, name) {
  stopifnot(is_auth(auth), is_string(name))
  
  path <- auth_path(paste0(name, ".rds"))
  inform(paste0("Saving auth to '", path, "'"))
  
  dir.create(auth_path(), showWarnings = FALSE, recursive = TRUE)
  saveRDS(auth, path)
  invisible(path)
}

#' @export
#' @rdname auth_save
auth_list <- function() {
  paths <- dir(auth_path(), pattern = "\\.rds$")
  tools::file_path_sans_ext(paths)
}

# Set default auth -------------------------------------------------------------

#' Set default authentication for the current session
#' 
#' `auth_as()` sets up the default authentication mechanism used by all 
#' rtweet API calls. See [rtweet_user()] to learn more about the three
#' available authentication options.
#' 
#' @param auth One of the following options:
#'   * `NULL`, the default, will look for rtweet's "default" authentication 
#'      which uses your personal Twitter account. If it's not found, it will 
#'      call [auth_setup_default()] to set it up.
#'   * A string giving the name of a saved auth file made by [auth_save()].
#'   * An auth object created by [rtweet_app()], [rtweet_bot()], or 
#'     [rtweet_user()].
#' @return Invisibly returns the previous authentication mechanism.
#' @seealso [auth_sitrep()] to help finding and managing authentications.
#' @family authentication
#' @export
#' @examples 
#' \dontrun{
#' # Use app auth for the remainder of this session:
#' my_app <- rtweet_app()
#' auth_as(my_app)
#' 
#' # Switch back to the default user based auth
#' auth_as()
#' 
#' # Load auth saved by auth_save()
#' auth_as("my-saved-app")
#' }
auth_as <- function(auth = NULL) {
  old <- .state$auth
  .state$auth <- find_auth(auth)
  invisible(old)
}

find_auth <- function(auth = NULL) {
  if (is.null(auth)) {
    if (is_developing()) {
      rtweet_test() %||% no_token()
    } else{
      default_cached_auth()
    }
  } else if (is_auth(auth)) {
    auth
  } else if (is_string(auth)) {
    if (file.exists(auth)) {
      path <- auth
    } else {
      path <- auth_path(paste0(auth, ".rds"))
      if (!file.exists(path)) {
        abort(paste0("Can't find saved auth with name '", auth, "'"))
      }
    }
    inform(paste0("Reading auth from '", path, "'"))
    readRDS(path)
  } else {
    abort("Unrecognised input to `auth`")
  }
}

default_cached_auth <- function() {
  default <- auth_path("default.rds")
  
  if (file.exists(default)) {
    readRDS(default)
  } else {
    names <- auth_list()
    if (length(names) == 0) {
      abort("No default authentication found. Please call `auth_setup_default()`")
    } else {
      abort(c(
        "No default authentication found. Pick existing auth with:",
        paste0("auth_as('", names, "')")
      ))
    }
  }
}

#' @rdname auth_setup_default
#' @export
auth_has_default <- function() {
  file.exists(auth_path("default.rds"))
}

no_token <- function() {
  if (is_testing()) {
    testthat::skip("Auth not available")
  } else {
    stop("Could not authenticate", call. = FALSE)
  }
}
# Internal function to generate the bot used for testing
# Do not forget to later us auth_as(rtweet_test())
rtweet_test <- function() {
  access_token <- Sys.getenv("RTWEET_ACCESS_TOKEN")
  access_secret <- Sys.getenv("RTWEET_ACCESS_SECRET")
  
  if (identical(access_token, "") || identical(access_secret, "")) {
    return()
  }
  
  rtweet_bot(
    "7rX1CfEYOjrtZenmBhjljPzO3",
    "rM3HOLDqmjWzr9UN4cvscchlkFprPNNg99zJJU5R8iYtpC0P0q",
    access_token,
    access_secret
  )
}

local_auth <- function(env = parent.frame()) {
  auth <- auth_get()
  withr::defer(auth_as(auth), envir = env)
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

twitter_init_oauth1.0 <- function(endpoint, app, permission = NULL,
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

auth_path <- function(...) {
  # Use private option to make testing easier
  path <- getOption("rtweet:::config_dir", tools::R_user_dir("rtweet", "config"))
  file.path(path, ...)
}

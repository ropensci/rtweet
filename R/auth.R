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
#' @param client_id,client_secret Application OAuth client ID and client Secret.
#' These are generally not required for `tweet_user()` since the defaults will use
#' the built-in rtweet app.
#' @param access_token,access_secret Access token and secret.
#' @param api_key,api_secret API key and secret. Deprecated in favor of `client_*` arguments.
#' @param bearer_token App bearer token.
#' @return If the validation is successful the OAuth token.
#' For rtweet_app a rtweet_bearer.
#' @family authentication
#' @export
#' @examples
#' \dontrun{
#' rtweet_user()
#' rtweet_bot()
#' rtweet_app()
#' }
rtweet_user <- function(client_id = NULL, client_secret = NULL,
                        api_key = client_id, api_secret = client_secret) {
  check_installed("httpuv")

  if (is.null(client_id) && is.null(client_secret)) {
    decrypt <- function(x) {
      rawToChar(openssl::rsa_decrypt(x[[2]], x[[1]]))
    }
    api_key <- decrypt(sysdat$DYKcJfBkgMnGveI)
    api_secret <- decrypt(sysdat$MRsnZtaKXqGYHju)
  } else {
    stopifnot(is_string(client_id), is_string(client_secret))
  }
  # From oauth_app help page
  # key: consumer key or client ID
  # secret: consumer secret, or client secret
  app <- httr::oauth_app("rtweet", key = client_id, secret = client_secret)
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
    access_secret <- ask_pass("access secret")
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
    return(readRDS(default))
  }

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

#' @rdname auth_setup_default
#' @export
auth_has_default <- function() {
  file.exists(auth_path("default.rds"))
}

no_token <- function(call = caller_env()) {
  if (is_testing()) {
    testthat::skip("Auth not available")
  } else {
    abort("Could not authenticate", call = call)
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

# Twitter Token -----------------------------------------------------------

# Twitter requires a callback url that uses 127.0.0.1 rather than localhost
# so we temporarily override HTTR_SERVER during initialization.
# <https://developer.twitter.com/en/docs/apps/callback-urls>

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
  # Twitter requirements only allow numbers
  # local environment for httr::oauth_callback to find the localhost IP.
  # Which is also modified to http://127.0.0.1:1410/
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


# Some endpoints require OAuth2.0 with PKCE
# https://developer.twitter.com/en/docs/authentication/oauth-2-0/authorization-code

oauth2_pkce <- function(client_id = "Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ") {

  client <- httr2::oauth_client(
    id = client_id,
    secret = obfuscated(paste0(
      "m3grxV6HQ2Yud-izF0r-",
      "RdaVKXq2vFWcF5rDGjAeHu80O_",
      "Cehg"
    )),
    token_url = "https://api.twitter.com/2/oauth2/token",
    name = "rtweet_academic_dev")
  # Guide to all urls for OAuth 2
  # https://developer.twitter.com/en/docs/authentication/api-reference
  # Guide of which endpoints require KPCE authentication
  # https://developer.twitter.com/en/docs/authentication/guides/v2-authentication-mapping
  # Useful questions: https://twittercommunity.com/t/unable-to-obtain-new-access-token-by-using-refresh-token/164123/14
  # Tutorial confirming the urls: https://developer.twitter.com/en/docs/tutorials/tweet-to-super-followers-with-postman--oauth-2-0-and-manage-twee
  #
  # There's a problem see https://github.com/r-lib/httr2/issues/193
  oa_flow <- oauth_flow_auth_code(client,
                       auth_url = "https://twitter.com/i/oauth2/authorize",
                       pkce = TRUE, # Needed to have a code_challenge but sets code_challenge_method to S265 which should work
                       scope = paste(check_scopes(), collapse = " "),
                       host_name = "127.0.0.1",
                       port = 1410
                       )
  # grant_type = c("grant_token"),
  # code_challange_method = "plain"
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=M1M5R3BMVy13QmpScXkzTUt5OE46MTpjaQ&redirect_uri=https://www.example.com         &scope=tweet.read%20users.read%20offline.access                &state=state                                      &code_challenge=challenge&code_challenge_method=plain
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2Flocalhost%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=t-8Jyr75Mn_W3oRYVBrJU8bFM5dSgaqT18NYmjvab3M&grant_type=grant_token&code_challange_method=plain&code_challenge=1-zcQTpp2EzpXdpI2LLl2dhoBPpHz2JS8hWtxdhZNUg&code_challenge_method=S256
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2Flocalhost%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=FOtz56gDvSuoWIm83X2tbsj7QwdIuSpiOaqMmUI-aFI
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2Flocalhost%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=LInXYZOragmAZCQYRfAgEOvB6E3q6ItYklBkWHkkkNE&code_challenge=j5AcuCjYFFvbFJ48JCKlr6hpl0mHl6DNTucN6-vCFkY&code_challenge_method=S256
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2Flocalhost%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=IV-tnYdVOEItuv5xuHdzEzGxa9rn7hHTzrFD5ihYZaY&code_challange_method=plain&code_challenge=8xoevTrp1OdaZuTAg0TJdIw2tiZRxr0BR51_Ty6h0tE&code_challenge_method=S256
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2Flocalhost%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=_1rgJaxG4DKHIgeJnEnkaJHDzTsgU2jVSnPEueQsf9w&grant_type=grant_token
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2Flocalhost%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=dWDWHW1Y8awylS7D58-CxQPMux8yXtSmNBL74XGY1fo&grant_type=grant_token&code_challange_method=plain
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2F127.0.0.1%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=Uyu6z7BR8YYfw6_nxpzFGDOmUsePaV6Hgj8ZdLrFIc8&code_challenge=9N7FXcCTOGhR4qDpUEMXj1SgXyCV3KYSqHImp6_LPAI&code_challenge_method=S256
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2F127.0.0.1%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=632Bzm25ygvt4NzUheIApjncLG09Nrjb5dnHahTOoVE&code_challenge=EkHl1eJ8QnL8CYQPX0gdCRbuWFt9N-RS3_yG7PWEncA&code_challenge_method=S256
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2F127.0.0.1%3A1410%2F&scope=offline.access%20tweet.read%20users.read%20bookmark.read&state=Q6QLQXbtpM0GPRh0yAod7uoEXW2Ge7VQPUGh3PVn4_U&code_challenge=ErlCL3Lpwj_hhGyksvdg2pNR51zXlNOJ3Zu8zZGK5bE&code_challenge_method=S256
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2F127.0.0.1%3A1410%2F&scope=&state=huIWC4od850lqQBwAiMGBjUcCcpHcBAnzS-8_Zf1O4s&code_challenge=mZYX-rjJyNndfw4PMuuQRPc_9GjmXdBq1P5FFDtrm0c&code_challenge_method=S256
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2F127.0.0.1%3A1410%2F&scope=&state=2EL_expI0FOtc8eo0SaxTdvbVKQLMfOoc4cst5Lylds&code_challenge=yZpfy7t9WTfUV07GiFzW9uDeU-q9RZG24NnHJyPp_ls&code_challenge_method=S256
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2F127.0.0.1%3A1410%2F&scope=tweet.read%20tweet.write%20tweet.moderate.write%20users.read%20follows.read%20follows.write%20offline.access%20space.read%20mute.read%20mute.write%20like.read%20like.write%20list.read%20list.write%20block.read%20block.write%20bookmark.read%20bookmark.write&state=eJVFLfXYGX34EblJV6BhPGVeO9Hsx_j_55AqFHnLzNo&code_challenge=bAPV1-GUJjdOYMbA5QNWqmWfxxgWOU6lqd1p5Ev8bAo&code_challenge_method=S256
  # Works!! redirect url should not be encoded and added a / at the end in form of %2F
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http://127.0.0.1:1410&scope=tweet.read%20tweet.write%20tweet.moderate.write%20users.read%20follows.read%20follows.write%20offline.access%20space.read%20mute.read%20mute.write%20like.read%20like.write%20list.read%20list.write%20block.read%20block.write%20bookmark.read%20bookmark.write&state=eJVFLfXYGX34EblJV6BhPGVeO9Hsx_j_55AqFHnLzNo&code_challenge=bAPV1-GUJjdOYMbA5QNWqmWfxxgWOU6lqd1p5Ev8bAo&code_challenge_method=S256
}

check_scopes <- function(scopes = NULL) {
  all_scopes <- c(
    tweet.read = "All the Tweets you can view, including Tweets from protected accounts.",
    tweet.write = "Tweet and Retweet for you.",
    tweet.moderate.write = "Hide and unhide replies to your Tweets.",
    users.read = "Any account you can view, including protected accounts.",
    follows.read = "People who follow you and people who you follow.",
    follows.write = "Follow and unfollow people for you.",
    offline.access = "Stay connected to your account until you revoke access.",
    space.read = "All the Spaces you can view.",
    mute.read = "Accounts you’ve muted.",
    mute.write = "Mute and unmute accounts for you.",
    like.read = "Tweets you’ve liked and likes you can view.",
    like.write = "Like and un-like Tweets for you.",
    list.read = "Lists, list members, and list followers of lists you’ve created or are a member of, including private lists.",
    list.write = "Create and manage Lists for you.",
    block.read = "Accounts you’ve blocked.",
    block.write = "Block and unblock accounts for you.",
    bookmark.read = "Get Bookmarked Tweets from an authenticated user.",
    bookmark.write = "Bookmark and remove Bookmarks from Tweets."
  )
  if (is.null(scopes)) {
    return(names(all_scopes))
  }
  scopes <- match.arg(scopes, names(all_scopes), several.ok = TRUE)
}

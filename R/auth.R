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
#' See the instructions in `vignette("auth", package = "rtweet")`.
#'
#' There are four ways that you can authenticate with the Twitter API:
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
#' * `rtweet_oauth2()` authenticates as a user using a client.
#'    This authentication is required in some endpoints.
#'
#' To use `rtweet_app()`, `rtweet_bot()`  or `rtweet_oauth2()` you will need to
#' create your own Twitter app following the instructions in
#' `vignette("auth", package = "rtweet")`.
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
#' These are generally not required for `rtweet_user()` since the defaults will use
#' the built-in rtweet app.
#' @param access_token,access_secret Access token and secret.
#' @param api_key,api_secret API key and secret. Deprecated in favor of `client_*` arguments.
#' @param bearer_token App bearer token.
#' @param app Name of the application you are building.
#' @return If the validation is successful the OAuth token.
#' For `rtweet_app()` a `rtweet_bearer`.
#' @family authentication
#' @seealso [rtweet_client()]
#' @export
#' @examples
#' \dontrun{
#' rtweet_app()
#' }
rtweet_user <- function(client_id = NULL, client_secret = NULL,
                        api_key = client_id, api_secret = client_secret) {
  check_installed("httpuv")
  if (is.null(client_id) && is.null(client_secret)) {
    # See https://github.com/ropensci/rtweet/issues/756#issuecomment-1497464678
    # https://github.com/ropensci/rtweet/issues/761#issuecomment-1497468605
    abort(c("The default authentication was suspended.",
            i = "Please provide your own authentication see: `vignette('auth', 'rtweet')`"))
    client <- default_cached_client()
    api_key <- client$id
    api_secret <- client$secret
  } else {
    stopifnot(is_string(client_id), is_string(client_secret))
  }
  # From oauth_app help page
  # key: consumer key or client ID
  # secret: consumer secret, or client secret
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
rtweet_bot <- function(api_key, api_secret, access_token, access_secret,
                       app = "rtweet") {

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

  app <- httr::oauth_app(app, key = api_key, secret = api_secret)
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
  check_installed("askpass", call = current_call())

  message <- paste0("Please enter your ", type, ": ")
  val <- askpass::askpass(message)
  if (is.null(val)) {
    abort("Cancelled by user", call = current_call())
  }
  val
}

#' @export
#' @rdname rtweet_user
rtweet_bearer <- function(client = NULL, scopes  = NULL) {

  client <- client %||% client_get()
  if (!is_client(client)) {
    abort(c("Client not valid",
            i = "Check out the `vignette('auth', 'rtweet')`."),
          call = current_call())
  }

  if (!is.null(scopes) && check_scopes(scopes)) {
    scopes <- scopes
  } else if (is.null(scopes)) {
    scopes <- set_scopes(tweet_moderate = FALSE)
  } else {
    abort("Scopes is not in the right format.", call = current_call())
  }

  token <- httr2::oauth_flow_client_credentials(client,
                                       scope = scopes,
                                       token_params = list(client_secret = client$secret,
                                                           client_type = "third_party_app",
                                                           grant_type = "refresh_token"))
  attr(token, "app") <- client
  token
}

# @seealso [invalidate_bearer()]
rtweet_invalidate <- function(api_key, api_secret, token = NULL) {
  # To store the token at the right place: see ?httr2::oauth_cache_path
  withr::local_envvar(HTTR2_OAUTH_CACHE = auth_path())
  if (missing(api_key)) {
    api_key <- ask_pass("API key")
  }
  if (missing(api_secret)) {
    api_secret <- ask_pass("API secret")
  }

  token <- check_token_v2(token, mechanism = "bearer")
  if (!is.null(token$token)) {
    abort("Not possible to invalidate bearer token generated with OAuth 1.0",
        call = caller_call())
  }
  httr2::request("https://api.twitter.com/oauth2/invalidate_token") |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(api_key, api_secret) |>
    httr2::req_body_form(access_token = token$access_token) |>
    httr2::req_perform()
}

is_auth <- function(x) {
  inherits(x, "Token") || inherits(x, "rtweet_bearer") || inherits(x, "httr2_token") || inherits(x, "httr2_oauth_client")
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
    load_token(auth)
  } else {
    abort("Unrecognised input to `auth`",
          call = current_call())
  }
}

load_token <- function(auth_name) {
  if (file.exists(auth_name)) {
    path <- auth_name
    name <- tools::file_path_sans_ext(basename(auth_name))
  } else {
    path <- auth_path(paste0(auth_name, ".rds"))
    name <- auth_name
  }
  if (!file.exists(path)) {
    abort(paste0("Can't find saved auth with name '", auth_name, "'"),
          call = current_call())
  }
  if (!is_developing()) {
    inform(paste0("Reading auth from '", path, "'"))
  }
  token <- readRDS(path)
  # Store the name of the token as an attribute
  attr(token, "name") <- name
  token
}

default_cached_auth <- function() {
  default <- auth_path("default.rds")

  if (file.exists(default)) {
    return(readRDS(default))
  }

  names <- auth_list()
  if (length(names) == 0) {
    abort("No default authentication found. Please call `auth_setup_default()`",
          call = caller_call())
  } else {
    abort(c(
      "No default authentication found. Pick existing auth with:",
      paste0("auth_as('", names, "')")
    ), call = caller_call())
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
    abort("Could not authenticate", call = current_call())
  }
}
# Internal function to generate the bot used for testing
# Do not forget to later us auth_as()
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
    access_secret,
    app = "rtweet_hadley"
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
  # Which is also modified to `http://127.0.0.1:1410/`
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
  full_path <- file.path(path, ...)
  full_path
}

#' Some endpoints require OAuth 2.0 with specific permissions in order to work.
#' In order to work, the developer must configure the app with  callback url:
#' `http://127.0.0.1:1410/`
#' @param client Which client app will be used, see [rtweet_client()] for details.
#' @param scopes The permissions of the app, see [set_scopes()] for details.
#' By default it uses the client's scopes. Provided here in case you want to modify them.
#' @references <https://developer.twitter.com/en/docs/authentication/oauth-2-0/authorization-code>
#' @export
#' @rdname rtweet_user
rtweet_oauth2 <- function(client = NULL, scopes = NULL) {
  lifecycle::deprecate_stop("2.0.0", what = "rtweet_oatuh2()",
                            details = c("No longer needed, httr2 handles it automatically",
                                        i = "Register your client with `client_as('my_client')` and make your requests"))
  client <- client %||% client_get()
  if (!is_client(client)) {
    abort(c("Client not valid",
          i = "Check out the `vignette('auth', 'rtweet')`."),
          call = current_call())
  }
  if (is.null(scopes)) {
    scopes <- client_scopes(client)
  } else if (!check_scopes(scopes)) {
    abort("Scopes is not in the right format.",
          call = current_call())
  }

  # Guide to all urls for OAuth 2
  # https://developer.twitter.com/en/docs/authentication/api-reference
  # Guide of which endpoints require KPCE authentication
  # https://developer.twitter.com/en/docs/authentication/guides/v2-authentication-mapping
  # Useful questions: https://twittercommunity.com/t/unable-to-obtain-new-access-token-by-using-refresh-token/164123/14
  # Tutorial confirming the urls: https://developer.twitter.com/en/docs/tutorials/tweet-to-super-followers-with-postman--oauth-2-0-and-manage-twee
  token <- httr2::oauth_flow_auth_code(client,
                       auth_url = "https://twitter.com/i/oauth2/authorize",
                       pkce = TRUE,
                       scope = paste(scopes, collapse = " "),
                       redirect_uri = "http://127.0.0.1:1410"
                       # redirect_uri = "http://localhost:1410"
                       )
  # Example of valid url for authorization (created via client_as("academic_dev");rtweet_oauth2() )
  # https://twitter.com/i/oauth2/authorize?response_type=code&client_id=Tm5FWFA3OGFUVUMxTjhUREwzZzQ6MTpjaQ&redirect_uri=http%3A%2F%2F127.0.0.1%3A1410%2F&scope=tweet.read%20tweet.write%20tweet.moderate.write%20users.read%20follows.read%20follows.write%20offline.access%20space.read%20mute.read%20mute.write%20like.read%20like.write%20list.read%20list.write%20block.read%20block.write%20bookmark.read%20bookmark.write&state=PVgWK3MviQ5MBsfj0Iy5D89HBFR4mPwTl0yumjSPlWo&code_challenge=FNcGvupIzNIbWOL8rdJOrxsVS_b2R01vIbynF_iQIMQ&code_challenge_method=S256
  # # Note that the client_id should match in the url
  attr(token, "app") <- attr(client, "app")
  token
}

# Renew token if needed
# Makes the assumption that the right app is still in the user computer
auth_renew <- function() {
  withr::local_envvar(HTTR2_OAUTH_CACHE = auth_path())

  client <- client_get()
  token <- httr2::oauth_token_cached(
    client = client,
    flow = httr2::oauth_flow_refresh,
    flow_params = list(
      scope = paste(client_scopes(client), collapse = " "),
      token_params = list(client_id = client$id)
    ),
    cache_disk = TRUE
  )
  token_file <- list.files(pattern = client$name, path = auth_path(),
                           full.names = TRUE, include.dirs = FALSE)
  token <- readRDS(token_file)
  # Somehow the time doesn't look fine
  loc <- Sys.setlocale("LC_TIME", locale = "C")
  expires <- .POSIXct(token$expires_at) >= Sys.time()
  Sys.setlocale("LC_TIME", locale = loc)



  if (expires && !interactive()) {
    if (is_testing()) {
      testthat::skip(paste0("Not possible to refresh the token automatically",
                            " and not in interactive environment."))
    }
    warn("Automatic refresh of the token was not possible.",
          call = caller_call())
  } else {
    return(NULL)
  }



  token <- httr2::oauth_flow_refresh(client,
                             refresh_token = token$refresh_token,
                             scope = token$scope,
                             token_params = list(client_id = client$id))
  saveRDS(token, token_file)
  message("Automatic refereshing of token")
}

decrypt <- function(x) {
  check_installed("openssl") # Suggested!
  rawToChar(openssl::rsa_decrypt(x[[2]], x[[1]]))
}

token_name <- function(x){
  attr(x, "name", TRUE)
}

resave_token <- function(token) {
  name <- token_name(token)
  if (is.null(name)) {
    inform(c("!" = "It was not possible to save your token",
             "i" = "Saving your new oauth2 token with: `auth_save(auth_get(), 'renewed_token')`"))
    auth_save(auth_get(), 'renewed_token')
  }

  path <- auth_path(paste0(name, ".rds"))
  # Protects in case the folder gets deleted while the token is loaded
  dir.create(auth_path(), showWarnings = FALSE, recursive = TRUE)
  saveRDS(token, path)
}

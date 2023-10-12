# This is almost a copy of auth.R file but for clients
# Only wto tweaks:
# - the default client is provided by rtweet and stored as rtweet.
# - the path to save clients is under a directory clients

#' Set default client for the current session
#'
#' `client_as()` sets up the default client used by rtweet API calls with PKCE. See [rtweet_user()] to learn more about the three
#' available authentication options.
#'
#' @param client One of the following options:
#'   * `NULL`, the default, will look for rtweet's "default" authentication
#'      which uses your personal default Twitter client. If it's not found, it will
#'      call [client_setup_default()] to set it up.
#'   * A string giving the name of a saved auth file made by [client_save()].
#'   * A client object created by [rtweet_client()].
#' @return Invisibly returns the previous authentication mechanism.
#' @family client
#' @export
#' @examples
#' \dontrun{
#' # Use app auth for the remainder of this session:
#' my_app <- rtweet_app()
#' auth_as(my_app)
#'
#' # Switch back to the default user based auth
#' client_as()
#' client_list()
#' }
client_as <- function(client = NULL) {
  old <- .state$client
  .state$client <- find_client(client)
  invisible(old)
}

#' @rdname client_setup_default
#' @export
client_has_default <- function() {
  file.exists(client_path("rtweet.rds"))
}

client_path <- function(...) {
  # Use private option to make testing easier
  path <- getOption("rtweet:::config_dir", tools::R_user_dir("rtweet", "config"))
  file.path(path, "clients", ...)
}

#' Get the current client
#'
#' If no client has been set up for this session, `client_get()` will
#' call [client_as()] to set it up.
#' @return  The current client used.
#' @export
#' @family client
#' @examples
#' \dontrun{
#' client_get()
#' }
client_get <- function() {
  if (is.null(.state$client)) {
    client_as()
  }
  .state$client
}

is_client <- function(client) {
  inherits(client, "httr2_oauth_client")
}

default_client <- function(client_id = NULL, client_secret = NULL) {
  if (is.null(client_id) && is.null(client_secret)) {
    # The sysdat file is in #./R and loaded automagically
    abort(c("The default rtweet client is no longer authorized.",
            i = "You'll need to register as developer in order to use the Twitter API."))
    client_id <- decrypt(sysdat$e914c55d2f)
    client_secret <- decrypt(sysdat$d5571d4003)
  } else {
    stopifnot(is_string(client_id), is_string(client_secret))
  }
  return(c(id = client_id, secret = client_secret))
}

default_cached_client <- function() {
  if (client_has_default()) {
    return(readRDS(client_path("rtweet.rds")))
  }
  client_str <- default_client()
  rtweet_client(client_str["id"], client_str["secret"], "rtweet")
}

#' Save an authentication mechanism for use in a future session
#'
#' Use `client_save()` with [client_as()] to avoid repeatedly entering app
#' credentials, making it easier to share auth between projects.
#' Use `client_list()` to list all saved credentials.
#'
#' The tokens are saved on the clients folder in `tools::R_user_dir("rtweet", "config")`.
#'
#' @param client A client [rtweet_client()].
#' @return Invisible the path where the client is saved.
#' @family client
#' @seealso [auth_sitrep()] to help finding and managing authentications.
#' @export
#' @examples
#' \dontrun{
#' # save app client for use in other sessions
#' client <- rtweet_client()
#' client_save(client)
#'
#' # later, in a different session...
#' client_as("my-app")
#' # Show all authentications stored
#' client_list()
#' }
client_save <- function(client) {
  stopifnot(is_client(client))
  name <- paste0(attr(client, "app"), ".rds")
  path <- client_path(name)
  inform(paste0("Saving client to '", path, "'"))
  dir.create(client_path(), showWarnings = FALSE, recursive = TRUE)
  saveRDS(client, path)
  invisible(path)
}

#' @export
#' @rdname client_save
client_list <- function() {
  paths <- dir(client_path(), pattern = "\\.rds$")
  tools::file_path_sans_ext(paths)
}

find_client <- function(client = NULL) {
  if (is.null(client)) {
    if (is_developing()) {
      load_client("academic_dev") %||% no_client()
    } else{
      default_cached_client()
    }
  } else if (is_client(client)) {
    client
  } else if (is_string(client)) {
    load_client(client)
  } else {
    abort("Unrecognised input to `client`")
  }
}


load_client <- function(client_name) {

  if (file.exists(client_name) && !dir.exists(client_name)) {
    path <- client_name
  } else {
    path <- client_path(paste0(client_name, ".rds"))
  }
  if (!file.exists(path) && !is_developing()) {
    abort(paste0("Can't find saved client with name '", client_name, "'"))
  } else if (!file.exists(path)) {
    return(NULL)
  }

  if (!is_developing()) {
    inform(paste0("Reading client from '", path, "'"))
  }
  readRDS(path)
}

no_client <- function(call = caller_env()) {
  if (is_testing()) {
    testthat::skip("Client not available")
  } else {
    abort("Could not find client", call = call)
  }
}

#' Client
#'
#' Set up your client mechanism for the Twitter API.
#' @inheritParams rtweet_user
#' @param scopes Default scopes allowed for users using this client.
#' Leave `NULL` to allow everything or choose yours with `set_scopes()`.
#' @param app Name of the client, it helps if you make it match with the name of your app.
#' On the Twitter app the Callback URI must be `http://127.0.0.1:1410/`
#' (the trailing / must be included).
#' @seealso scopes
#' @export
#' @examples
#' if (interactive()) {
#'   rtweet_client()
#' }
rtweet_client <- function(client_id, client_secret,
                          app, scopes = NULL) {
  if (missing(client_id) && missing(client_secret) ) {
    dc <- default_client()
    client_id <- dc[1]
    client_secret <- dc[2]
    app <- "rtweet"
  }

  if (missing(client_id) && interactive()) {
    client_id <- ask_pass("client ID key")
  }
  if (missing(client_secret) && interactive()) {
    client_secret <- ask_pass("client secret")
  }

  stopifnot(is_string(client_id), is_string(client_secret))

  if (is.null(scopes)) {
    scopes <- all_scopes
  } else {
    scopes <- check_scopes(scopes)
  }

  client <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = "https://api.twitter.com/2/oauth2/token",
    auth = "header",
    name = app)
  attr(client, "app") <- app
  attr(client, "scopes") <- scopes
  client
}



#' Set up default client
#'
#' You'll need to run this function once per computer so that rtweet can use
#' your client.
#'
#' It will use the current default account for rtweet and save them as "rtweet".
#' If a default is found it will use it instead.
#' @return
#' `client_setup_default()`: Invisibly returns the previous authentication mechanism.
#' `client_has_default()`: A logical value `TRUE` if there is a default authentication.
#' @export
#' @family client
#' @examples
#' \dontrun{
#' if (!client_has_default()) {
#'    client_setup_default()
#' }
#' }
client_setup_default <- function() {
  if (client_has_default()) {
    inform("Using default client available.")
  } else {
    abort(c("The default rtweet client is no longer authorized.",
            i = "You'll need to register as developer in order to use the Twitter API."))
    client <- rtweet_client(decrypt(sysdat$DYKcJfBkgMnGveI),
                            decrypt(sysdat$MRsnZtaKXqGYHju),
                            app = "rtweet")
    client_save(client)
  }
  client_as("rtweet")
  invisible(client_get())
}


client_scopes <- function(client, call = caller_env()) {
  if (!is_client(client)) {
    abort(c("Missing client.",
            ">" = "Check the vignette('auth', 'rtweet')",
            "i" = "Get one with `rtweet_client()`"), call = call)
  }
  attr(client, "scopes", exact = TRUE)
}

client_app <- function(client) {
  stopifnot(is_client(client))
  attr(client, "app", exact = TRUE)
}

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
#'   * An auth object created by [rtweet_client()].
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

client_get <- function() {
  if (is.null(.state$client)) {
    client_as()
  }
  .state$client
}

is_client <- function(client) {
  inherits(client, "httr2_oauth_client")
}

default_cached_client <- function() {
  default <- client_path("rtweet.rds")

  if (file.exists(default)) {
    return(readRDS(default))
  }

  names <- client_list()
  if (length(names) == 0) {
    abort("No default client found. Please call `client_setup_default()`")
  } else {
    abort(c(
      "No default client found. Pick existing cleint with:",
      paste0("cleint_as('", names, "')")
    ))
  }
}

#' Save an authentication mechanism for use in a future session
#'
#' Use `client_save()` with [client_as()] to avoid repeatedly entering app
#' credentials, making it easier to share auth between projects.
#' Use `client_list()` to list all saved credentials.
#'
#' The tokens are saved on `tools::R_user_dir("rtweet", "config")`.
#'
#' @param client A client [rtweet_client()].
#' @return Invisible the path where the client is saved.
#' @family authentication
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
        find_client("academic_dev")
      } else{
        default_cached_client()
      }
    } else if (is_client(client)) {
      client
    } else if (is_string(client)) {
      if (file.exists(client)) {
        path <- client
      } else {
        path <- client_path(paste0(client, ".rds"))
        if (!file.exists(path)) {
          abort(paste0("Can't find saved client with name '", client, "'"))
        }
      }
      if (!is_developing()) {
        inform(paste0("Reading client from '", path, "'"))
      }
      readRDS(path)
    } else {
      abort("Unrecognised input to `client`")
    }
  }

#' Client
#'
#' Set up your client mechanism for rtweet API loking with KPCE.
#' @inheritParams rtweet_user
#' @param scopes Scopes allowed for this client. Leave NULL to use all.
#' @param app Name of the client, it helps if you make it match with the name of your app.
#' On the Twitter app must be "http://127.0.0.1:1410/" (the trailing / must be
#' included)
#' @seealso scopes
#' @export
#' @examples
#' rtweet_client()
rtweet_client <- function(client_id, client_secret,
                            app, scopes = NULL) {

  if (missing(client_id) && missing(client_secret)) {
    client_setup_default()
    return(client_get())
  }
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
    client
}



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
client_setup_default <- function() {
  if (client_has_default()) {
    inform("Using default client available.")
  } else {
    client <- rtweet_client(decrypt(sysdat$DYKcJfBkgMnGveI),
                          decrypt(sysdat$MRsnZtaKXqGYHju),
                          app = "rtweet")
    client_save(client)
  }
  client_as("rtweet")
}


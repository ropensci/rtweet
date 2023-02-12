client_as <- function(client = NULL) {
  old <- .state$client
  .state$client <- find_client(client)
  invisible(old)
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
  default <- client_path("default.rds")

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

client_save <- function(client) {
  stopifnot(is_client(client))
  name <- paste0(attr(client, "app"), ".rds")
  path <- client_path(name)
  inform(paste0("Saving client to '", path, "'"))
  dir.create(client_path(), showWarnings = FALSE, recursive = TRUE)
  saveRDS(client, path)
  invisible(path)
}


client_list <- function() {
  paths <- dir(client_path(), pattern = "\\.rds$")
  tools::file_path_sans_ext(paths)
}

find_client <- function(client = NULL) {
    if (is.null(client)) {
      if (is_developing()) {
        client_default
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
#' Set up your client mechanism for rtweet.
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
                            scopes = NULL, app = "rtweet") {

  if (missing(client_id) && missing(client_secret)) {
    client_save(client_default)
    return(client_default)
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


client_default <- rtweet_client(decrypt(sysdat$DYKcJfBkgMnGveI),
                                decrypt(sysdat$MRsnZtaKXqGYHju))

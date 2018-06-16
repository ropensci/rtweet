

encode_keys <- function(key, secret) {
  try_require("openssl")
  openssl::base64_encode(paste0(key, ":", secret))
}

create_bearer_token <- function(token = NULL) {
  ## Step 1: Encode consumer key and secret

  ## Get app keys from token
  if (is.null(token)) {
    token <- get_token()
  }
  ## get app keys
  app_keys <- encode_keys(get_app_key(token), get_app_secret(token))

  ## Step 2: Obtain a bearer token
  r <- httr::POST("https://api.twitter.com/oauth2/token",
    httr::add_headers(Authorization = paste0("Basic ", app_keys)),
    body = list(grant_type = "client_credentials"))
  httr::stop_for_status(r)
  bearer <- httr::content(r, encoding = "UTF-8")
  bearer_env <- new.env()
  assign(".bearer_env", bearer_env, envir = .state)
  assign("bearer", bearer, envir = bearer_env)
  invisible()
}

## Step 3: Authenticate API requests with the bearer token

#' Bearer token
#'
#' Converts oauth token into bearer token
#'
#' @param token Oauth token created via \code{\link{create_token}}.
#' @return A bearer token
#' @export
bearer_token <- function(token = NULL) {
  bearer <- get_bearer(token)
  if (is.null(bearer)) {
    stop("couldn't find bearer token")
  }
  r <- httr::add_headers(Authorization = paste0("Bearer ", bearer$access_token))
  structure(r, bearer = bearer, class = c("bearer", "list"))
}


get_bearer <- function(token = NULL) {
  ## create if not already
  if (!exists(".bearer_env", envir = .state)) {
    create_bearer_token(token)
  }
  ## retrieve
  bearer_env <- get(".bearer_env", envir = .state)
  bearer <- tryCatch(get("bearer", envir = bearer_env), error = function(e) NULL)
  if (is.null(bearer)) {
    stop("couldn't find bearer token")
  }
  bearer
}

get_app_key <- function(token) {
  token$app$key
}

get_app_secret <- function(token) {
  token$app$secret
}

get_oauth_key <- function(token) {
  token$credentials$oauth_token
}

get_oauth_secret <- function(token) {
  token$credentials$oauth_token_secret
}

print.bearer <- function(bearer) {
  cat(paste0("Bearer token: ", attr(bearer, "bearer")$access_token), fill = TRUE)
}

#' Invalidate bearer token
#'
#' @param token Oauth token created via \code{\link{create_token}}.
#' @export
invalidate_bearer <- function(token = NULL) {
  if (is.null(token)) {
    token <- get_token()
  }
  bearer <- bearer_token(token)
  bearer_token <- attr(bearer, "bearer")
  ## Get app keys from token
  app_keys <- encode_keys(get_app_key(token), get_app_secret(token))
  r <- httr::POST("https://api.twitter.com/oauth2/invalidate_token",
    httr::add_headers(Authorization = paste0("Basic ", app_keys)),
    body = list(access_token = bearer_token$access_token),
    encode = "form")
  httr::warn_for_status(r)
  r
}

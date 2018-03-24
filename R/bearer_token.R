

encode_keys <- function(key, secret) {
  openssl::base64_encode(paste0(key, ":", secret))
}

get_bearer_token <- function(token = NULL) {
  ## Step 1: Encode consumer key and secret

  ## Get app keys from token
  if (is.null(token)) {
    token <- get_token()
  }
  app_key <- token$app$key
  app_secret <- token$app$secret
  app_keys <- encode_keys(app_key, app_secret)

  ## Step 2: Obtain a bearer token
  r <- httr::POST("https://api.twitter.com/oauth2/token",
    httr::add_headers(Authorization = paste0("Basic ", app_keys)),
    body = list(grant_type = "client_credentials"))
  stop_for_status(r)
  bearer <- content(r)
  bearer_env <- new.env()
  assign("bearer", r, envir = bearer_env)
  invisible()
}

## Step 3: Authenticate API requests with the bearer token
bearer_token <- function() {
  bearer <- get_bearer_token()
  if (is.null(bearer)) {
    stop("couldn't find bearer token")
  }
  httr::add_headers(Authorization = paste0("Bearer ", bearer$access_token))
}


get_bearer_token <- function() {
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

## Invalidate bearer token
invalidate_bearer <- function(token) {
  bearer <- get_bearer_token()
  ## Get app keys from token
  app_key <- get_app_key(token)
  app_secret <- get_app_secret(token)
  if (is.null(token)) {
    token <- get_token()
  }
  app_keys <- encode_keys(app_key, app_secret)
  r <- httr::POST("https://api.twitter.com/oauth2/invalidate_token",
    httr::add_headers(Authorization = paste0("Basic ", app_keys)),
    body = list(access_token = bearer$access_token))
  httr::stop_for_status(r)
}

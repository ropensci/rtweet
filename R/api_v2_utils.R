# Checking tokens ####
auth_is_bearer <- function(token = NULL) {

  if (is.null(token)) {
    token <- auth_get()
  }
  # if the bearer is created with rtweet_bearer the class is different
  bearer_httr2 <- inherits(token, "httr2_token") && is.null(token$refresh_token)
  inherits(token, "rtweet_bearer") || bearer_httr2
}

prepare_bearer <- function(x, y) {
  token_credentials <- paste0(x, ":", y, collapse = "")

  check_installed("openssl")
  openssl::base64_encode(token_credentials)
}

auth_is_pkce <- function(token = NULL) {
  if (is.null(token)) {
    token <- auth_get()
  }
  inherits(token, "httr2_token")
}

# Check token readiness for API v2
#
# Check if current authentication is ready for API v2 usage.
# @param token The token to check if can be used for API v2.
# @param mechanism Which flavor of authentication must be used.
#
# The authentication method required for each endpoint might change, this is a
# helper for examples.
#
# @return If no issue is found the original token, if something is amiss it raises an error.
# @export
#
# @examples
#  if (auth_has_default()) {
#     tryCatch(check_token_v2())
#  }
check_token_v2 <- function(token = NULL, mechanism = "bearer") {

  token <- token %||% auth_get()
  return(token)
  mechanism <- match.arg(mechanism, c("bearer", "pkce"), several.ok = TRUE)

  # For endpoints that accept both authentications methods
  if (length(mechanism) == 2 && (auth_is_bearer(token) || auth_is_pkce(token))) {
    return(token)
  } else if (length(mechanism) == 2) {
    # To make it easier testing interactively
    if (is_developing()) {
      return(load_token("bearer_academic_dev"))
    }

    abort(c(
      "x" = "You must use a token accepted by the endpoints v2.",
      "i" = "Check the `vignette('auth', package = 'rtweet')` about how to get them."),
      call = current_call())
  }

  if (mechanism == "bearer" && !auth_is_bearer(token)) {

    abort(c("x" = "A bearer `token` is needed for this endpoint.",
            "i" = "Get one via `rtweet_app()`"),
          call = current_call())
  }
  if (mechanism == "pkce" && !auth_is_pkce(token)) {

    client <- client_get()
    if (!is_client(client)) {
      msg <- c(">" = "Check the vignette('auth', 'rtweet')",
      "i" = "Get a client with `rtweet_client()`")
    } else {
      msg <- NULL
    }
    abort(c("x" = "An OAuth 2.0  is needed for this endpoint.",
            msg,
            "i" = "Get the authorization via `rtweet_oauth2()`"),
          call = current_call())
  }
  token
}

# Provides the required method for the token type
req_auth <- function(req, token) {
  if (auth_is_bearer(token)) {
    httr2::req_auth_bearer_token(req, token$token)
  } else if (auth_is_pkce(token)) {
    token <- auth_renew()
    token <- token$access_token
  }
}

req_is_error <- function(resp) {
  if (is(resp, "httr2_response")) {
    r <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = FALSE)
  } else {
    r <- resp
  }
  has_name_(r, "errors") || httr2::resp_is_error(resp)
}

req_errors <- function(resp) {
  if (is(resp, "httr2_response")) {
    r <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = FALSE)
  } else {
    r <- resp
  }

  if (any(lengths(r$errors) > 1)) {
    if (is.data.frame(r$errors[[1]])) {
      errors <- r$errors
    } else {
      errors <- do.call(rbind, lapply(r$errors, list2DF))
    }
  }  else {
    errors <- r$errors
  }

  if (has_name_(errors, "message")) {
    return(errors$message)
  }
  if (has_name_(errors, "title")) {
    return(paste0(errors$title, " with value: ", errors$value))
  }
}

# Prepare the requests ####
# General function to create the requests for Twitter API v2 with retry limits
# and error handling
req_v2 <- function(scopes) {
  client <- client_get()
  scopes_client <- client_scopes(client)
  req <- httr2::request("https://api.twitter.com/2")
  req_agent <- httr2::req_user_agent(req, "rtweet (https://docs.ropensci.org/rtweet)")
  # req_authorized <- req_auth(req_agent, token)
  check_scopes(scopes_client, scopes)
  # httr2 looks for this path.
  withr::local_envvar(HTTR2_OAUTH_CACHE = auth_path())
  req_authorized <- httr2::req_oauth_auth_code(req_agent,
                             client = client,
                             auth_url = "https://twitter.com/i/oauth2/authorize",
                             pkce = TRUE,
                             scope = paste(scopes_client, collapse = " "),
                             redirect_uri = "http://127.0.0.1:1410",
                             cache_disk = TRUE
                             # redirect_uri = "http://localhost:1410"
  )

  req_content <- httr2::req_headers(req_authorized,
                             `Content-type` = "application/json")
  req_try <- httr2::req_retry(req_content,
                              is_transient = twitter_is_transient,
                              after = twitter_after)
  req <- httr2::req_error(req_try, is_error = httr2::resp_is_error, body = req_errors)
  req
}

twitter_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(403, 503) &&
    identical(httr2::resp_header(resp, "x-rate-limit-remaining"), "0")
}

twitter_after <- function(resp) {
  when <- as.numeric(httr2::resp_header(resp, "x-rate-limit-reset"))
  when - unclass(Sys.time())
}


endpoint_v2 <- function(path, throttle, scopes) {

  req <- httr2::req_url_path_append(req_v2(scopes), path)
  httr2::req_throttle(req, throttle, realm = path)
}

# Help with the arguments of the requests ####
prepare_params <- function(x) {
  lapply(x, paste0, collapse = ",")
}

check_rate <- function(token, rate_app, rate_user) {
  switch(is(token),
         rtweet_bearer = rate_app,
         httr2_token =  rate_user,
         NULL)
}

# Function to set fields and expansions:
#  - NA is reference (aka, all possibilities via set_*)
#  - NULL is NULL
#  - other are the ones provided via argument
arg_def <- function(argument, reference) {

  if (length(argument) >= 1 && !anyNA(argument)) {
    return(argument)
  }

  if (is.null(argument)) {
    return(NULL)
  }

  reference
}

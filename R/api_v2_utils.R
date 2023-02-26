# Checking tokens ####
auth_is_bearer <- function(token = NULL) {

  if (is.null(token)) {
    token <- auth_get()
  }
  inherits(token, "rtweet_bearer")
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
check_token_v2 <- function(token = NULL, mechanism = "bearer", call = caller_env()) {

  token <- token %||% auth_get()

  mechanism <- match.arg(mechanism, c("bearer", "pkce"), several.ok = TRUE)

  # For endpoints that accept both authentications methods
  if (length(mechanism) == 2 && (auth_is_bearer(token) || auth_is_pkce(token))) {
    return(token)
  } else if (length(mechanism) == 2) {
    # To make it easier testing interactively
    if (is_developing()) {
      auth_as("bearer_academic_dev")
      return(auth_get())
    }

    abort(c(
      "x" = "You must use a token accepted by the endpoints v2.",
      "i" = "Check the `vignette('auth', package = 'rtweet')` about how to get them."),
      call = call)
  }

  if (mechanism == "bearer" && !auth_is_bearer(token)) {
    # To make it easier testing interactively
    if (is_developing()) {
      auth_as("bearer_academic_dev")
      return(auth_get())
    }
    abort(c("x" = "A bearer `token` is needed for this endpoint.",
            "i" = "Get one via rtweet_app()"),
          call = call)
  }
  if (mechanism == "pkce" && !auth_is_pkce(token)) {
    # To make it easier testing interactively
    if (is_developing()) {
      auth_as("oauth2_academic")
      return(auth_get())
    }
    abort(c("x" = "An OAuth 2.0  is needed for this endpoint.",
            "i" = "Get one via rtweet_*() "),
          call = call)
  }
  token
}

# Provides the required method for the token type
req_auth <- function(req, token) {
  if (auth_is_bearer(token)) {
    token <- token$token
  } else if (auth_is_pkce(token)) {
    if (.POSIXct(token[["expires_at"]]) <= Sys.time()) {
      token <- auth_renew(token)
    }
    token <- token$access_token
  }
  httr2::req_auth_bearer_token(req, token)
}


# Prepare the requests ####
# General function to create the requests for Twitter API v2 with retry limits
# and error handling
req_v2 <- function(token = NULL, call = caller_env()) {
  req <- httr2::request("https://api.twitter.com/2")
  req_agent <- httr2::req_user_agent(req, "rtweet (https://docs.ropensci.org/rtweet)")
  req_authorized <- req_auth(req_agent, token)
  req_content <- httr2::req_headers(req_authorized,
                             `Content-type` = "application/json")
  req_try <- httr2::req_retry(req_content,
                              is_transient = twitter_is_transient,
                              after = twitter_after)
  req_try
}

twitter_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(403, 503) &&
    identical(httr2::resp_header(resp, "x-rate-limit-remaining"), "0")
}

twitter_after <- function(resp) {
  when <- as.numeric(httr2::resp_header(resp, "x-rate-limit-reset"))
  when - unclass(Sys.time())
}


endpoint_v2 <- function(token, path, throttle, call = caller_call()) {

  req <- httr2::req_url_path_append(req_v2(token, call), path)
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

prepare_params <- function(x) {
  lapply(x, paste, collapse = ",")
}

auth_is_bearer <- function(token = NULL) {

  if (is.null(token)) {
    token <- auth_get()
  }
  inherits(token, "rtweet_bearer")
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

  if (mechanism == "bearer" && auth_is_bearer(token)) {
    abort("A bearer `token` is needed for API v2")
  }
  token
}

# General function to create the requests for Twitter API v2 with retry limits
# and error handling
req_v2 <- function(token = NULL) {

  token <- check_token_v2(token)
  req <- httr2::request("https://api.twitter.com/2")
  req_headers <- httr2::req_headers(req,
                                    `Content-type` = "application/json",
                                    Authorization = paste0("Bearer ", token$token)
                                    )
  req_try <- httr2::req_retry(req_headers,
                              is_transient = twitter_is_transient,
                              after = twitter_after)
  # httr2::req_error(req_try, body = error_body)
  req_try
}

error_body <- function(resp) {
  httr2::resp_body_json(resp)
}

endpoint_v2 <- function(token, path, throttle) {

  req <- httr2::req_url_path_append(req_v2(token), path)
  httr2::req_throttle(req, throttle, realm = path)
}

parsing <- function(x) {
  if (!is.logical(x) || any(is.na(x))) {
    stop("parse should be either TRUE or FALSE", call. = FALSE)
  }
  if (length(x) > 1) {
    stop("parse should be of length 1", call. = FALSE)
  }
  if (isTRUE(x)) {
    stop("Parsing for the rtweet API is not yet implemented", call. = FALSE)
  }
}

# Handle the response and give attributes
resp <- function(obj) {

}


twitter_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(403, 503) &&
    identical(httr2::resp_header(resp, "x-rate-limit-remaining"), "0")
}
twitter_after <- function(resp) {
  when <- as.numeric(httr2::resp_header(resp, "x-rate-limit-reset"))
  when - unclass(Sys.time())
}

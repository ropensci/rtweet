check_fields <- function(fields,
                         media_fields = NULL,
                         place_fields = NULL,
                         poll_fields = NULL,
                         tweet_fields = NULL,
                         user_fields = NULL,
                         metrics_fields = NULL) {
  error <- c(
    check_field_helper(fields, media_fields, "media"),
    check_field_helper(fields, media_fields, "place"),
    check_field_helper(fields, media_fields, "poll"),
    check_field_helper(fields, media_fields, "tweet"),
    check_field_helper(fields, media_fields, "user"),
    check_field_helper(fields, media_fields, "metrics")
  )
  stop(error, call. = FALSE)
}


check_field_helper <- function(passed, allowed, name) {
  y <- passed[[name]]
  if (is.null(allowed) && !is.null(y)) {
    return("No media allowed")
  }
  wrong <- setdiff(y, allowed)
  paste("Fields", paste(wrong, collapse = ", "), " are not allowed or valid.\n")
}

check_extensions <- function(passed, allowed) {
  within <- passed %in% allowed
  if (length(passed) > 10 || any(within)) {
    extensions <- passed[within]
    stop("These extensions are now allowed: ",
         paste(extensions, collapse = ", "), call. = FALSE)
  }
}

prepare_params <- function(x) {
  lapply(x, paste, collapse = ",")
}

check_token_v2 <- function(token = NULL) {
  token <- token %||% auth_get()

  if (!inherits(token, "rtweet_bearer")) {
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
  resp_body_json(resp)
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
  when <- as.numeric(resp_header(resp, "x-rate-limit-reset"))
  when - unclass(Sys.time())
}

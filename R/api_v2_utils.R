# Creating requests ####
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

  if (mechanism == "bearer" && !auth_is_bearer(token)) {
    abort("A bearer `token` is needed for this endpoint.")
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


endpoint_v2 <- function(token, path, throttle) {

  req <- httr2::req_url_path_append(req_v2(token), path)
  httr2::req_throttle(req, throttle, realm = path)
}

prepare_params <- function(x) {
  lapply(x, paste, collapse = ",")
}

# Handling responses ####
parsing <- function(x) {
  if (!is.logical(x) || any(is.na(x))) {
    stop("parse should be either TRUE or FALSE", call. = FALSE)
  }
  if (length(x) > 1) {
    stop("parse should be of length 1", call. = FALSE)
  }
  if (isTRUE(x)) {
    stop("Parsing for the rtweet API v2 is not yet implemented", call. = FALSE)
  }
}

l_minus <- function(l, minus) {
  keep <- setdiff(names(l), minus)
  l[keep]
}

# Handle the response and give attributes
resp <- function(obj, type = "json", ...) {
  out <- switch(type,
                "json" = httr2::resp_body_json(obj, ...),
                "html" = httr2::resp_body_html(obj, ...),
                "xml" = httr2::resp_body_xml(obj, ...))
  class(out) <- c("Twitter_resp", class(out))

  if (has_name_(out, "meta")) {
    meta <- data.frame(sent = strptime(out$meta$sent, tz = "UTC",
                                       format = "%Y-%m-%dT%H:%M:%OS"))
    if (has_name_(out$meta, "summary")) {
      meta <- cbind(meta, list2DF(out$meta$summary))
    }
    rest <- list2DF(l_minus(out$meta, c("summary", "sent")))
    if (ncol(rest) >= 1 && nrow(rest) == 1) {
      meta <- cbind(meta, rest)
    } else if (nrow(rest) > 1) {
      stop("Please check")
    }
    out$meta <- meta
  }

  if (has_name_(out, "errors")) {
    out$errors <- do.call(rbind, lapply(out$errors, list2DF))
  }
  out
}



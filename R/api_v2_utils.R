# Creating requests ####
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
  }

  if (mechanism == "bearer" && !auth_is_bearer(token)) {
    abort(c("x" = "A bearer `token` is needed for this endpoint.",
            "i" = "Get one via rtweet_app()"),
          call = call)
  }
  if (mechanism == "pkce" && !auth_is_pkce(token)) {
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
      token <- auth_renew(token$access_token)
    }
    token <- token$access_token
  }
  httr2::req_auth_bearer_token(req, token)
}

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

prepare_params <- function(x) {
  lapply(x, paste0, collapse = ",")
}

# Handling responses ####
parsing <- function(x, call = caller_env()) {
  if (!is_logical(x)) {
    abort("parse should be either TRUE or FALSE", call = call)
  }

  if (isTRUE(x)) {
    abort("Parsing for the rtweet API v2 is not yet implemented", call = call)
  }
}

list_minus <- function(l, minus) {
  keep <- setdiff(names(l), minus)
  l[keep]
}

# Pagination should be consistent across API v2
# <https://developer.twitter.com/en/docs/twitter-api/pagination>
pagination <- function(req, n_pages, verbose = TRUE) {
  if (is.infinite(n_pages)) {
    n_pages <- 8
  }
  # Temporary file to store data in case of troubles
  tmp <- tempfile("rtweet_tmp", fileext = ".rds")

  all_results <- vector("list", length = n_pages)
  resp <- httr2::req_perform(req)
  x0 <- httr2::resp_body_json(resp)
  all_results[[1]] <- x0
  i <- 2
  next_pag_token <- x0$meta$next_token

  # If already got what we need stop
  if (n_pages == 1) {
    return(list(x0))
  }

  if (verbose)  {
    pb <- progress::progress_bar$new(
      format = "Downloading paginated request :bar")
    withr::defer(pb$terminate())
  }

  while (!is.null(next_pag_token)) {
    req <- httr2::req_url_query(req, pagination_token = next_pag_token)
    resp <- httr2::req_perform(req)
    cnt <- httr2::resp_body_json(resp)
    if (i > length(all_results)) {
      # double length per https://en.wikipedia.org/wiki/Dynamic_array#Geometric_expansion_and_amortized_cost
      length(all_results) <- 2 * length(all_results)
    }
    # Save temporary data: https://github.com/ropensci/rtweet/issues/531
    all_results[[i]] <- cnt
    saveRDS(all_results, tmp)
    if (verbose) {
      inform(paste0("Saving temporary data to ", tmp))
      pb$tick()
    }
    i <- i + 1
    next_pag_token <- cnt$meta$next_token
  }
  empty <- vapply(all_results, is.null, logical(1L))
  all_results[!empty]
}

# Handle the response and give attributes
resp <- function(obj, type = "json", ...) {

  # For each element in the pagination
  # data: Tidy
  # meta: Tidy and add it to the attributes
  # errors: Tidy and What??

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
    rest <- list2DF(list_minus(out$meta, c("summary", "sent")))
    if (ncol(rest) >= 1 && nrow(rest) == 1) {
      meta <- cbind(meta, rest)
    } else if (nrow(rest) > 1) {
      abort("Please check", call = call)
    }
    out$meta <- meta
  }

  if (has_name_(out, "errors")) {
    out$errors <- do.call(rbind, lapply(out$errors, list2DF))
  }
  out
}

check_rate <- function(token, rate_app, rate_user) {
  switch(is(token),
         rtweet_bearer = rate_app,
         httr2_token =  rate_user,
         NULL)
}

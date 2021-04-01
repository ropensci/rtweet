TWIT_get <- function(token, api, params = NULL, parse = TRUE, ..., host = "api.twitter.com") {
  resp <- TWIT_method("GET", 
    token = token, 
    api = api,
    params = params,
    ...,
    host = host
  )
  
  if (parse) {
    from_js(resp)
  } else {
    resp
  }
}

TWIT_post <- function(token, api, params = NULL, body = NULL, ..., host = "api.twitter.com") {
  TWIT_method("POST", 
    token = token, 
    api = api,
    params = params,
    body = body,
    ...,
    host = host
  )
}

TWIT_method <- function(method, token, api, 
                        params = NULL, 
                        host = "api.twiter.com",
                        retryonratelimit = FALSE,
                        verbose = TRUE,
                        ...) {
  # need scipen to ensure large IDs are not displayed in scientific notation
  # need ut8-encoding for the comma separated IDs
  withr::local_options(scipen = 14, encoding = "UTF-8")

  token <- check_token(token)
  url <- paste0("https://", host, api, ".json")
  
  repeat({
    resp <- switch(method,
      GET = httr::GET(url, query = params, token, ...),
      POST = httr::POST(url, query = params, token, ...),
      stop("Unsupported method", call. = FALSE)
    )
    
    switch(resp_type(resp),
      ok = break,
      rate_limit = handle_rate_limit(
        resp, api, 
        retryonratelimit = retryonratelimit,
        verbose = verbose
      ),
      error = handle_error(resp)
    )
  })

  resp
}

#' Pagination
#' 
#' @keywords internal
#' @param get_max_id A single argument function that returns a vector of 
#'   string ids. This is needed because different endpoints store that 
#'   information in different places.
#' @param max_id String giving id of most recent tweet to return. 
#'   Can be used for manual pagination.
#' @param retryonratelimit If `TRUE`, and a rate limit is exhausted, will wait
#'   until it refreshes. Most twitter rate limits refresh every 15 minutes.
#'   If `FALSE`, and the rate limit is exceeded, the function will terminate
#'   early with a warning; you'll still get back all results received up to 
#'   that point.
#'   
#'   If you expect a query to take hours or days to perform, you should not 
#'   rely soley on `retryonratelimit` because it does not handle other common
#'   failure modes like temporarily losing your internet connection.
#' @param verbose Show progress bars and other messages indicating current 
#'   progress?
TWIT_paginate_max_id <- function(token, api, params, 
                                 get_max_id, 
                                 n = 1000, 
                                 page_size = 200, 
                                 parse = TRUE,
                                 max_id = NULL,
                                 count_param = "count", 
                                 retryonratelimit = FALSE,
                                 verbose = TRUE) {
  
  params[[count_param]] <- page_size  
  pages <- ceiling(n / page_size)
  results <- vector("list", pages)
  
  if (verbose)  {
    pb <- progress::progress_bar$new(
      format = "Downloading multiple pages :bar",
      total = pages
    ) 
    withr::defer(pb$terminate())
  }

  for (i in seq_len(pages)) {
    params$max_id <- max_id
    if (i == pages) {
      params[[count_param]] <- n - (pages - 1) * page_size
    }

    resp <- catch_rate_limit(
      TWIT_get(
        token, api, params, 
        retryonratelimit = retryonratelimit,
        parse = FALSE,
        verbose = verbose
      )
    )
    if (is_rate_limit(resp)) {
      warn_early_term(json, 
        hint = paste0("Set `max_id = '", max_id, "' to continue."),
        hint_if = !is.null(max_id)
      )
      break
    }
    
    json <- from_js(resp)
    # no more tweets to return
    if (length(json) == 0) {
      break
    }
    
    max_id <- id_minus_one(last(get_max_id(json)))
    results[[i]] <- if (parse) json else resp
    
    if (verbose) {
      pb$tick()
    }
  }

  results
}

# https://developer.twitter.com/en/docs/pagination
#' @rdname TWIT_paginate_max_id
#'  
#' @param cursor Which page of results to return. The default will return 
#'   the first page; can be used for manual pagination. 
TWIT_paginate_cursor <- function(token, api, params, 
                                 n = 5000, 
                                 page_size = 5000, 
                                 cursor = "-1", 
                                 get_id = function(x) x$ids,
                                 retryonratelimit = FALSE,
                                 verbose = TRUE) {
  params$count <- page_size
  
  # TODO: consider if its worth using fastmap::faststack() here
  results <- list()
  i <- 1
  n_seen <- 0
  
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Downloading multiple pages :spin",
    ) 
    withr::defer(pb$terminate())
  }

  repeat({
    params$cursor <- cursor
    json <- catch_rate_limit(
      TWIT_get(
        token, api, params, 
        retryonratelimit = retryonratelimit,
        verbose = verbose
      )
    )
    if (is_rate_limit(json)) {
      warn_early_term(json, 
        hint = paste0("Set `cursor = '", cursor, "' to continue."),
        hint_if = !identical(cursor, "-1")
      )
      break
    }

    results[[i]] <- json
    cursor <- json$next_cursor_str
    n_seen <- n_seen + length(get_id(json))
    i <- i + 1

    if (identical(cursor, "0") || n_seen >= n) {
      break
    }
    
    if (verbose) {
      pb$tick()
    }
  })

  results
}

#' @rdname TWIT_paginate_max_id
#'  
TWIT_paginate_chunked <- function(token, api, params_list, 
                                  retryonratelimit = FALSE, 
                                  verbose = TRUE) {
  

  pages <- length(params_list)
  results <- vector("list", pages)
  
  if (verbose)  {
    pb <- progress::progress_bar$new(
      format = "Downloading multiple pages :bar",
      total = pages
    ) 
    withr::defer(pb$terminate())
  }

  for (i in seq_along(params_list)) {
    params <- params_list[[i]]
    json <- catch_rate_limit(
      TWIT_get(
        token, api, params, 
        retryonratelimit = retryonratelimit,
        verbose = verbose
      )
    )
    if (is_rate_limit(json)) {
      warn_early_term(json, hint_if = FALSE)
      break
    }
    
    results[[i]] <- json
    
    if (verbose) {
      pb$tick()
    }
  }

  results
}  


# helpers -----------------------------------------------------------------

from_js <- function(resp) {
  if (!grepl("application/json", resp$headers[["content-type"]])) {
    stop("API did not return json", call. = FALSE)
  }
  resp <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(resp)
}

resp_type <- function(resp) {
  x <- resp$status_code
  if (x == 429) {
    "rate_limit"
  } else if (x >= 400) {
    "error"
  } else {
    "ok"
  }
}

# Three possible exits:
# * skip, if testing
# * return, if retryonratelimit is TRUE
# * error, otherwise
handle_rate_limit <- function(x, api, retryonratelimit = FALSE, verbose = TRUE) {
  if (is_testing()) {
    testthat::skip("Rate limit exceeded")
  }

  headers <- httr::headers(x)
  n <- headers$`x-rate-limit-limit`
  when <- .POSIXct(as.numeric(headers$`x-rate-limit-reset`))
  
  if (retryonratelimit) {
    wait_until(when, api, verbose = verbose)
  } else {
    message <- c(
      paste0("Rate limit exceeded for Twitter endpoint '", api, "'"), 
      paste0("Will receive ", n, " more requests at ", format(when, "%H:%M"))
    )
    abort(message, class = "rtweet_rate_limit", when = when)
  }
}

# I don't love this interface because it returns either a httr response object
# or a condition object, but it's easy to understand and avoids having to do
# anything exotic to break from the correct frame.
catch_rate_limit <- function(code) {
  tryCatch(code, rtweet_rate_limit = function(e) e)
}

is_rate_limit <- function(x) inherits(x, "rtweet_rate_limit")

warn_early_term <- function(cnd, hint, hint_if) {
  warn(c(
    "Terminating paginate early due to rate limit.",
    cnd$message,
    i = if (hint_if) hint,
    i = "Set `retryonratelimit = TRUE` to automatically wait for reset"
  ))
}

# https://developer.twitter.com/en/support/twitter-api/error-troubleshooting
handle_error <- function(x) {
  parsed <- from_js(x)
  stop(
    "Twitter API failed [", x$status_code, "]\n",
    paste0(" * ", parsed$errors$message, " (", parsed$errors$code, ")"),
    call. = FALSE
  )
}

check_status <- function(x, api) {
  switch(resp_type(x),
    ok = NULL,
    rate_limit = ,
    error = handle_error(x)
  )
}

check_token <- function(token = NULL) {
  token <- token %||% auth_get()

  if (inherits(token, "Token1.0")) {
    token
  } else if (inherits(token, "rtweet_bearer")) {
    httr::add_headers(Authorization = paste0("Bearer ", token$token))
  } else {
    abort("`token` is not a valid access token")
  }
}

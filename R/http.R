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

TWIT_post <- function(token, api, params = NULL, ..., host = "api.twitter.com") {
  TWIT_method("POST", 
    token = token, 
    api = api,
    params = params,
    ...,
    host = host
  )
}

TWIT_method <- function(method, token, api, 
                        params = NULL, 
                        host = "api.twiter.com",
                        ...) {
  # need scipen to ensure large IDs are not displayed in scientific notation
  # need ut8-encoding for the comma separated IDs
  withr::local_options(scipen = 14, encoding = "UTF-8")

  token <- check_token(token)
  url <- paste0("https://", host, "/1.1/", api, ".json")
  
  resp <- switch(method,
    GET = httr::GET(url, query = params, token, ...),
    POST = httr::POST(url, body = params, token, ...),
    stop("Unsupported method", call. = FALSE)
  )
  check_status(resp)
  resp
}

# Different endpoints return the ids in different parts of the response,
# so `get_max_id()` lets the caller declare where.
TWIT_paginate_max_id <- function(token, query, params, 
                                 get_max_id, 
                                 n = 1000, 
                                 page_size = 200, 
                                 parse = TRUE,
                                 count_param = "count") {
  
  params[[count_param]] <- page_size  
  pages <- ceiling(n / page_size)
  results <- vector("list", pages)
  
  for (i in seq_len(pages)) {
    if (i == pages) {
       params[[count_param]] <- n - (pages - 1) * page_size
    }
    
    resp <- TWIT_get(token, query, params, parse = FALSE)
    json <- from_js(resp)
    results[[i]] <- if (parse) json else resp
    
    if (length(json) == 0) {
      # no more tweets to return
      break
    }
    params$max_id <- id_minus_one(last(get_max_id(json)))
  }

  results
}

TWIT_paginate_cursor <- function(token, query, params, n = 5000, page_size = 5000) {
  params$count <- page_size
  
  # TODO: consider if its worth using fastmap::faststack() here
  results <- list()
  i <- 1
  results[[i]] <- TWIT_get(token, query, params)
  next_cursor <- results[[i]]$next_cursor_str
  n_seen <- length(results[[i]]$ids)
  
  while (!identical(next_cursor, "0") && n_seen < n) {
    i <- i + 1
    params$cursor <- next_cursor
    results[[i]] <- TWIT_get(token, query, params)
    next_cursor <- results[[i]]$next_cursor_str
    n_seen <- n_seen + length(results[[i]]$ids)
  }
  
  results
}


# helpers -----------------------------------------------------------------

#' @importFrom jsonlite fromJSON
from_js <- function(resp) {
  if (!grepl("application/json", resp$headers[["content-type"]])) {
    stop("API did not return json", call. = FALSE)
  }
  resp <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(resp)
}

# https://developer.twitter.com/en/support/twitter-api/error-troubleshooting
check_status <- function(x) {
  if (!httr::http_error(x)) {
    return()
  }
  
  parsed <- from_js(x)
  
  if (is_testing() && identical(x$status_code, 429)) {
    testthat::skip("Rate limit exceeded")
  }
  
  stop(
    "Twitter API failed [", x$status_code, "]\n",
    paste0(" * ", parsed$errors$message, " (", parsed$errors$code, ")"),
    call. = FALSE
  )
}

TWIT_get <- function(token, api, params = NULL, ..., host = "api.twitter.com") {
  resp <- TWIT_method("GET",
                      token = token,
                      api = api,
                      params = params,
                      ...,
                      host = host
  )

  from_js(resp)
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
                        host = "api.twitter.com",
                        retryonratelimit = NULL,
                        verbose = TRUE,
                        ...) {
  lifecycle::deprecate_warn("2.0.0", function_caller(),
                            details = c("This function might work until your bot/app/user is blocked or fail randomly!",
                                        "!" = "The API has been deprecated and the new API v2.2 requires subscriptions for most endpoints.",
                                        i = "See updates of function and API: help('rtweet-deprecated', 'rtweet' )"
                                        ),
                            always = TRUE)
  # need scipen to ensure large IDs are not displayed in scientific notation
  # need ut8-encoding for the comma separated IDs
  withr::local_options(scipen = 14, encoding = "UTF-8")

  token <- check_token(token)
  url <- paste0("https://", host, api, ".json")

  resp <- switch(method,
                 GET = httr::GET(url, query = params, token, ...),
                 POST = httr::POST(url, query = params, token, ...),
                 stop("Unsupported method", call. = FALSE)
  )

  switch(resp_type(resp),
         ok = NULL,
         rate_limit = handle_rate_limit(
           resp, api,
           retryonratelimit = retryonratelimit,
           verbose = verbose
         ),
         error = handle_error(resp, params)
  )

  resp
}

#' Pagination
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' These are internal functions used for pagination inside of rtweet.
#'
#' @keywords internal
#' @param token Use this to override authentication for
#'   a single API call. In many cases you are better off changing the
#'   default for all calls. See [auth_as()] for details.
#' @param n Desired number of results to return. Results are downloaded
#'   in pages when `n` is large; the default value will download a single
#'   page. Set `n = Inf` to download as many results as possible.
#'
#'   The Twitter API rate limits the number of requests you can perform
#'   in each 15 minute period. The easiest way to download more than that is
#'   to use `retryonratelimit = TRUE`.
#'
#'   You are not guaranteed to get exactly `n` results back. You will get
#'   fewer results when tweets have been deleted or if you hit a rate limit.
#'   You will get more results if you ask for a number of tweets that's not
#'   a multiple of page size, e.g. if you request `n = 150` and the page
#'   size is 200, you'll get 200 results back.
#' @param get_id A single argument function that returns a vector of ids given
#'   the JSON response. The defaults are chosen to cover the most common cases,
#'   but you'll need to double check whenever implementing pagination for
#'   a new endpoint.
#' @param max_id Supply a vector of ids or a data frame of previous results to
#'   find tweets **older** than `max_id`.
#' @param since_id Supply a vector of ids or a data frame of previous results to
#'   find tweets **newer** than `since_id`.
#' @param retryonratelimit If `TRUE`, and a rate limit is exhausted, will wait
#'   until it refreshes. Most Twitter rate limits refresh every 15 minutes.
#'   If `FALSE`, and the rate limit is exceeded, the function will terminate
#'   early with a warning; you'll still get back all results received up to
#'   that point. The default value, `NULL`, consults the option
#'   `rtweet.retryonratelimit` so that you can globally set it to `TRUE`,
#'   if desired.
#'
#'   If you expect a query to take hours or days to perform, you should not
#'   rely solely on `retryonratelimit` because it does not handle other common
#'   failure modes like temporarily losing your internet connection.
#' @param verbose Show progress bars and other messages indicating current
#'   progress?
#' @returns A list with the json output of the API.
TWIT_paginate_max_id <- function(token, api, params,
                                 get_id = function(x) x$id_str,
                                 n = 1000,
                                 page_size = 200,
                                 since_id = NULL,
                                 max_id = NULL,
                                 count_param = "count",
                                 retryonratelimit = NULL,
                                 verbose = TRUE) {
  if (!is.null(max_id)) {
    max_id <- rtweet::max_id(max_id)
  }
  if (!is.null(since_id)) {
    since_id <- rtweet::since_id(since_id)
  }

  params$since_id <- since_id
  params[[count_param]] <- page_size
  pages <- if (is.infinite(n)) page_size else max(c(n %/% page_size), 1)
  results <- vector("list", pages)

  if (verbose)  {
    pb <- progress::progress_bar$new(
      format = "Downloading multiple pages :bar",
      total = pages
    )
    withr::defer(pb$terminate())
  }

  i <- 0
  while (i < pages) {
    i <- i + 1
    params$max_id <- max_id
    if (i == pages) {
      params[[count_param]] <- n - (pages - 1) * page_size
    }

    json <- catch_rate_limit(
      TWIT_get(
        token, api, params,
        retryonratelimit = retryonratelimit,
        verbose = verbose
      )
    )
    if (is_rate_limit(json)) {
      warn_early_term(json,
                      hint = paste0("Set `max_id = '", max_id, "' to continue."),
                      hint_if = !is.null(max_id)
      )
      break
    }

    id <- get_id(json)
    # no more tweets to return
    if (length(id) == 0) {
      break
    }
    if(i > length(results)) {
      # double length per https://en.wikipedia.org/wiki/Dynamic_array#Geometric_expansion_and_amortized_cost
      length(results) <- 2 * length(results)
    }

    max_id <- max_id(id)
    results[[i]] <- json

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
#'   the first page; you can supply the result from a previous call to
#'   continue pagination from where it left off.
TWIT_paginate_cursor <- function(token, api, params,
                                 n = 5000,
                                 page_size = 5000,
                                 cursor = "-1",
                                 get_id = function(x) x$ids,
                                 retryonratelimit = NULL,
                                 verbose = TRUE) {
  params$count <- page_size
  cursor <- next_cursor(cursor)
  if (identical(cursor, "0")) {
    # Last request retrieved all available results
    return(list())
  }

  # TODO: consider if its worth using fastmap::faststack() here
  pages <- if (is.infinite(n)) page_size else max(c(n %/% page_size), 1)
  results <- vector("list", pages)
  i <- 1
  n_seen <- 0

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Downloading multiple pages :bar",
      total = length(results)
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

    # Rate limit hit, repeat call to continue
    error_limit <- "errors" %in% names(json)
    continue_limit <- !is.null(retryonratelimit) && retryonratelimit
    if (error_limit && continue_limit) {
      json <- catch_rate_limit(
        TWIT_get(
          token, api, params,
          retryonratelimit = retryonratelimit,
          verbose = verbose
        ))
    }

    if (is_rate_limit(json)) {
      if (!is.null(retryonratelimit)){
        warn_early_term(json,
                        hint = paste0("Set `cursor = '", cursor, "' to continue."),
                        hint_if = !identical(cursor, "-1")
        )
      }
      next
    }
    if (i > length(results)) {
      # double length per https://en.wikipedia.org/wiki/Dynamic_array#Geometric_expansion_and_amortized_cost
      length(results) <- 2 * length(results)
    }
    if (any(grepl("next_cursor", names(json)))) {
      cursor <- ifelse(!is.null(json$next_cursor_str),
                       json$next_cursor_str,
                       json$next_cursor)
    } else {
      # If next_cursor is missing there are no message within the last 30 days
      cursor <- "0"
    }
    results[[i]] <- json
    n_seen <- n_seen + length(get_id(json))
    i <- i + 1
    empty_response <- !is.null(json$events) && length(json$events) == 0
    if (identical(cursor, "0") || n_seen >= n || empty_response) {
      break
    }

    if (verbose) {
      pb$update(n_seen / n)
    }
  })

  structure(results, rtweet_cursor = cursor)
}

#' @rdname TWIT_paginate_max_id
#' @keywords internal
TWIT_paginate_chunked <- function(token, api, params_list,
                                  retryonratelimit = NULL,
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
    # Rate limit hit, repeat call to continue
    error_limit <- "errors" %in% names(json)
    continue_limit <- !is.null(retryonratelimit) && retryonratelimit
    if (error_limit && continue_limit) {
      json <- catch_rate_limit(
        TWIT_get(
          token, api, params,
          retryonratelimit = retryonratelimit,
          verbose = verbose
        ))
    }
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

#' @rdname TWIT_paginate_max_id
TWIT_paginate_premium <- function(token, api, params,
                                  n = 100,
                                  page_size = 100,
                                  cursor = "next",
                                  retryonratelimit = NULL,
                                  verbose = TRUE) {
  if (identical(cursor, "next")) {
    # Last request retrieved all available results
    cursor <- NULL
  } else {
    cursor <- next_cursor(cursor)
  }
  params[["next"]] <- cursor

  # TODO: consider if its worth using fastmap::faststack() here
  pages <- if (is.infinite(n)) page_size else max(c(n %/% page_size), 1)
  results <- vector("list", pages)
  i <- 1
  n_seen <- 0

  if (length(results) > 1) {
    params[["maxResults"]] <- page_size
  }

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Downloading multiple pages :bar",
      total = length(results))
    withr::defer(pb$terminate())
  }
  # Time to sleep avoid hitting the lowest rate limits
  min_sleep <- 0.9
  if (page_size == 500) {
    min_sleep <- min_sleep * 2
  }
  repeat({

    Sys.sleep(min_sleep)
    params[["next"]] <- cursor
    json <- catch_rate_limit(
      TWIT_get(
        token, api, params,
        retryonratelimit = retryonratelimit,
        verbose = verbose
      )
    )

    if (is_rate_limit(json)) {
      if (!is.null(retryonratelimit)){
        warn_early_term(json,
                        hint = paste0("Set `continue = '", cursor, "' to continue."),
                        hint_if = !identical(cursor, "next")
        )
      }
      break
    }

    # Rate limit hit, repeat call to continue
    error_limit <- "errors" %in% names(json)
    continue_limit <- !is.null(retryonratelimit) && retryonratelimit
    if (error_limit && continue_limit) {
      json <- catch_rate_limit(
        TWIT_get(
          token, api, params,
          retryonratelimit = retryonratelimit,
          verbose = verbose
        ))
    }

    if (i > length(results)) {
      # double length per https://en.wikipedia.org/wiki/Dynamic_array#Geometric_expansion_and_amortized_cost
      length(results) <- 2 * length(results)
    }
    results[[i]] <- json$results
    if (any(grepl("next", names(json)))) {
      cursor <- if (!is.null(json[["next"]])) json[["next"]]
    }
    n_seen <- n_seen + nrow(json$results)
    i <- i + 1
    empty_response <- !is.null(json$results) && length(json$results) == 0
    if ( length(n_seen) == 0 || n_seen >= n || empty_response) {
      break
    }

    if (verbose) {
      pb$update(n_seen / n)
    }
  })

  structure(results, "next" = cursor)
}


# helpers -----------------------------------------------------------------

from_js <- function(resp) {
  if (is.null(resp$headers[["content-type"]]) || !grepl("application/json", resp$headers[["content-type"]])) {
    stop("API did not return json", call. = FALSE)
  }
  resp <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(resp)
}

resp_type <- function(resp) {
  status <- httr::status_code(resp)
  if (status == 429) {
    "rate_limit"
  } else if (status >= 400) {
    "error"
  } else {
    "ok"
  }
}

# Three possible exits:
# * skip, if testing
# * return, if retryonratelimit is TRUE
# * error, otherwise
handle_rate_limit <- function(x, api, retryonratelimit = NULL, verbose = TRUE) {
  if (is_testing()) {
    testthat::skip("Rate limit exceeded")
  }

  headers <- httr::headers(x)
  n <- headers$`x-rate-limit-limit`
  when <- .POSIXct(as.numeric(headers$`x-rate-limit-reset`))

  retryonratelimit <- retryonratelimit %||% getOption("rtweet.retryonratelimit", FALSE)

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

# Function for responses that might be errors
# Depending on the internal error code it is provided to the users as a warning or error
handle_codes <- function(x) {
  if ("errors" %in% names(httr::content(x))) {
    errors <- httr::content(x)$errors[[1]]
    for (e in seq_len(max(lengths(errors)))) {
      funct <- switch(as.character(errors$code[e]),
                      "89" = stop,
                      warning)
      funct(paste0(errors$message[e], " (", errors$code[e], ")"),
            call. = FALSE)

    }
  }
}

# https://developer.twitter.com/en/support/twitter-api/error-troubleshooting
handle_error <- function(x, params) {
  chk_message <- "Check error message at https://developer.twitter.com/en/support/twitter-api/error-troubleshooting"
  if (is.null(x$headers[["content-type"]])) {
    abort(paste0("Twitter API failed [", x$status_code, "]\n", chk_message),
          call = caller_call())
  }
  json <- from_js(x)
  error <- if (!is.null(json[["error"]])) json[["error"]] else json[["errors"]]
  if (x$status_code %in% c("401", "403") && is_developing()) {
    testthat::skip("API v1.1 no longer works")
    if (length(error) == 1) {
      if (any(c("screen_name", "user_id") %in% names(params))) {
        account <- params$screen_name
        if (is.null(account))
          account <- params$user_id
        warn(paste0("Skipping unauthorized account: ", account))
      } else {
        warn(paste0("Something went wrong with the authentication:\n\t", error))
      }
    } else if (length(error) == 2) {
      abort(c(paste0("Twitter API failed [", x$status_code, "]:"),
              paste0(error$message, " (", error$code, ")")),
            call. = caller_call())
    } else {
      if (is_testing()) {
        testthat::skip("Something went wrong with the requests")
      }
      warn("Something went wrong with the requests")
    }
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
  } else if (auth_is_bearer(token)) {
    httr::add_headers(Authorization = paste0("Bearer ", token$token))
  } else if (auth_is_pkce(token)) {
    abort(c("This OAuth 2.0 `token` is not a valid access token",
            "i" = "Please use a bearer token via `rtweet_app()`."),
          call = caller_call())
  } else {
    abort("`token` is not a valid access token", call = caller_call())
  }
}

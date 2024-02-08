#' Rate limit helpers
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @description
#' * `rate_limit()` returns a tibble of info about all rate limits
#' * `rate_limit_reset()` returns the next reset for a endpoint
#' * `rate_limit_wait()` waits for the next reset for an endpoint
#'
#' You should not need to use these function in the usual operation of rtweet
#' because all paginated functions will wait on your behalf if you set
#' `retryonratelimit = TRUE`.
#'
#' @inheritParams lookup_users
#' @param resource_match An optional regular expression used to filter the
#'   resources listed in returned rate limit data.
#' @param endpoint Name of Twitter endpoint like `"lookup/users"`,
#'   `"/media/upload"`, or `"/feedback/show/:id"`.
#' @family tokens
#' @seealso [`rtweet-deprecated`]
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/developer-utilities/rate-limit-status>
rate_limit <- function(resource_match = NULL, token = NULL) {
  json <- TWIT_get(token, "/1.1/application/rate_limit_status")

  resources <- unlist(unname(json$resources), recursive = FALSE)
  df <- tibble::tibble(
    resource = names(resources),
    limit = unlist(lapply(resources, "[[", "limit"), use.names = FALSE),
    remaining = unlist(lapply(resources, "[[", "remaining"), use.names = FALSE),
    reset_at = unlist(lapply(resources, "[[", "reset"), use.names = FALSE),
  )
  df$reset_at <- .POSIXct(df$reset_at)
  df$reset <- round(difftime(df$reset_at, Sys.time(), units = "mins"))

  if (!is.null(resource_match)) {
    df <- df[grepl(resource_match, df$resource), ]
  }

  df
}

#' @export
#' @rdname rate_limit
rate_limit_reset <- function(endpoint, token = NULL) {
  endpoint <- gsub("^/", "", endpoint)

  resource <- strsplit(endpoint, "/")[[1]][[1]]
  params <- list(resource = resource)
  json <- TWIT_get(token, "/1.1/application/rate_limit_status", params)

  info <- json$resources[[resource]][[paste0("/", endpoint)]]
  if (is.null(info)) {
    stop("Unrecognised endpoint '", endpoint, "'", call. = FALSE)
  }

  if (info$remaining > 0) {
    Sys.time()
  } else {
    .POSIXct(info$reset)
  }
}

#' @export
#' @rdname rate_limit
rate_limit_wait <- function(endpoint, token = NULL) {
  reset <- unclass(rate_limit_reset(endpoint, token))
  wait_until(reset, endpoint)

  invisible()
}

wait_until <- function(until, api, fps = 8, verbose = TRUE) {
  until <- unclass(until)
  seconds <- until - unclass(Sys.time())

  if (!verbose) {
    Sys.sleep(ceiling(seconds))
    return()
  }

  if (seconds < 0) {
    return(invisible())
  }

  pb <- progress::progress_bar$new(
    total = seconds * fps,
    format = paste0(
      "Rate limit exceeded for Twitter endpoint '", api, "'. ",
      "Waiting for refresh in :mins mins :spin"
    )
  )
  withr::defer(pb$terminate())

  while(Sys.time() < until) {
    Sys.sleep(1 / fps)
    mins <- round((until - unclass(Sys.time())) / 60)
    pb$tick(tokens = list(mins = mins))
  }

  invisible()
}

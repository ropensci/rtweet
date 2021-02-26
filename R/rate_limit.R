#' Rate limit helpers
#'
#' * `rate_limit()` returns a tibble of info about all rate limits
#' * `rate_limit_reset()` returns the next reset for a endpoint
#' * `rate_limit_wait()` waits for the next reset for an endpoint
#'
#' @inheritParams lookup_users
#' @param resources Constraint results to specific set of resources.
#' @param endpoint Name of twitter endpoint like `"lookup/users"`,
#'   `"/media/upload"`, or `"/feedback/show/:id"`.
#' @seealso
#'   <https://developer.twitter.com/en/docs/developer-utilities/rate-limit-status/api-reference/get-application-rate_limit_status>
#' @examples
#'
#' \dontrun{
#' rate_limit()
#' }
#' @family tokens
#' @export
rate_limit <- function(resources = NULL, token = NULL) {
  if (length(resources) > 0) {
    resources <- paste0(resources, collapse = ",") 
  }
  params <- list(resources = resources)
  json <- TWIT_get(token, "application/rate_limit_status", params)
  
  resources <- unlist(unname(json$resources), recursive = FALSE)
  
  df <- tibble::tibble(
    resource = names(resources),
    limit = unlist(lapply(resources, "[[", "limit"), use.names = FALSE),
    remaining = unlist(lapply(resources, "[[", "remaining"), use.names = FALSE),
    reset_at = unlist(lapply(resources, "[[", "reset"), use.names = FALSE),
  )
  df$reset_at <- .POSIXct(df$reset_at)
  df$reset <- round(difftime(df$reset_at, Sys.time(), units = "mins"))
  
  df
}

#' @export
#' @rdname rate_limit
rate_limit_reset <- function(endpoint, token = NULL) {
  endpoint <- gsub("^/", "", endpoint)
  
  resource <- strsplit(endpoint, "/")[[1]][[1]]
  params <- list(resource = resource)
  json <- TWIT_get(token, "application/rate_limit_status", params)
  
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
  now <- unclass(Sys.time())
  seconds <- reset - now
  
  if (seconds < 0) {
    return(invisible())
  }
  
  fps <- 4
  pb <- progress::progress_bar$new(
    total = seconds * fps,
    format = paste0("Waiting for '", endpoint, "' refresh :spin :bar in :mins mins")
  )
  withr::defer(pb$terminate())
  
  while(Sys.time() < reset) {
    Sys.sleep(1 / fps)
    mins <- round((reset - unclass(Sys.time())) / 60)
    pb$tick(tokens = list(mins = mins))
  }

  invisible()
}

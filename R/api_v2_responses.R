# Handling responses ####
parsing <- function(x, expansions, fields, call = caller_env()) {
  if (!is_logical(x)) {
    abort("parse should be either TRUE or FALSE", call = call)
  }
  if (isTRUE(x) && (!is.null(expansions) || !is.null(fields))) {
    abort(c("Not yet implemented!",
            i = "Stay tuned for further updates or use `parse = FALSE`"))
  }
}

list_minus <- function(l, minus) {
  keep <- setdiff(names(l), minus)
  l[keep]
}

# Pagination should be consistent across API v2
# <https://developer.twitter.com/en/docs/twitter-api/pagination>
pagination <- function(req, n_pages, count, verbose = TRUE) {
  if (is.infinite(n_pages)) {
    n_pages <- 8
  }
  # Temporary file to store data in case of troubles
  tmp <- tempfile("rtweet_tmp", fileext = ".rds")

  all_results <- vector("list", length = n_pages)
  resp <- httr2::req_perform(req)
  x0 <- resp(resp)
  all_results[[1]] <- x0

  # If already got what we need stop
  next_pag_token <- x0$meta$next_token
  if (n_pages == 1 || is.null(next_pag_token)) {
    return(list(x0))
  }
  i <- 2
  # counts in the tweet/counts/* endpoints return total_tweet_count
  total <- x0$meta[[names(x0$meta)[endsWith(names(x0$meta), "_count")]]]

  if (verbose)  {
    pb <- progress::progress_bar$new(
      format = "Downloading paginated request :bar",
      total = n_pages)
    pb$message(paste0("Saving temporary data to ", tmp))
    withr::defer(pb$terminate())
  }

  while (!is.null(next_pag_token) && i <= n_pages) {
    req <- httr2::req_url_query(req, pagination_token = next_pag_token)
    resp <- httr2::req_perform(req)
    cnt <- resp(resp)
    if (i > length(all_results)) {
      # double length per https://en.wikipedia.org/wiki/Dynamic_array#Geometric_expansion_and_amortized_cost
      length(all_results) <- 2 * length(all_results)
    }
    # Save temporary data: https://github.com/ropensci/rtweet/issues/531
    all_results[[i]] <- cnt
    if (verbose) {
      pb$tick()
      # Only save the data if verbose (assuming non verbose output will be quick)
      # Also to avoid a verbose and save argument.
      saveRDS(all_results, tmp)
    }
    i <- i + 1
    total <- total + cnt$meta[[names(cnt$meta)[endsWith(names(cnt$meta), "_count")]]]
    next_pag_token <- cnt$meta$next_token
  }
  if (!is.infinite(count) && total < count) {
    warn("The API returned less results than requested and possible.")
  }
  if (verbose && !is.null(next_pag_token)) {
    inform("The API might allow you to continue the same query via the `next_token`.")
  }
  empty <- vapply(all_results, is.null, logical(1L))
  all_results[!empty]
}

# Initial conversion of data about the requests returned by the API
resp <- function(x, ...) {
  # Simplify so that a list is converted to a vector and that there are data.frames
  # Might make it harder when a tweet has some data and others don't!
  out <- httr2::resp_body_json(x, simplifyVector = TRUE, flatten = FALSE)
  class(out) <- c("Twitter_resp", class(out))

  if (has_name_(out, "errors")) {
    abort(req_errors(out), call = NULL)
  }

  if (has_name_(out, "meta")) {
    # Summary and sent are fields in some endpoints:
    # streaming endpoints, ?
    rest <- list2DF(list_minus(out$meta, c("summary", "sent")))
    # Protection in case of other fields are empty
    rest <- rest[1, seq_len(NCOL(rest)), drop = FALSE]
    rownames(rest) <- NULL
    if (has_name_(out$meta, "sent")) {
      sent <- format_date_precison(out$meta$sent)
      rest$sent <- sent
    }

    if (has_name_(out$meta, "summary")) {
      rest$summary <- list2DF(out$meta$summary)
    }

    if (nrow(rest) > 1) {
      abort("Please check", call = call)
    }
    out$meta <- rest
  }
  out
}

#' Expose errors of the response
#'
#' @param expr An expression that might cause an error.
#' If `NULL` it looks for the last error.
#' @examples
#' if (FALSE){
#'   new_rule <- stream_add_rule(list(value = "#rstats", tag = "rstats1"))
#'   stream_add_rule(list(value = "#rstats", tag = "rstats2")) # ERROR
#'   # See the full information provided by the API:
#'   retrieve_errors(stream_add_rule(list(value = "#rstats", tag = "rstats2")))
#'   retrieve_errors()
#' }
retrieve_errors <- function(expr = NULL) {
  if (is.null(expr)) {
    le <- rlang::last_error()
    if (is(class(le), "rtweet_API_errors")) {
      return(le$errors)
    } else {
      le
    }
  }
  # This is experimental on the rlang side!
  try_fetch(expr, error = function(cnd){cnd$errors}
  )
}

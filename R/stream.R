#' Collect a live stream of Twitter data
#'
#' @description
#' Streams public statuses to a file via one of the following four methods:
#'
#' 1. Sampling a small random sample of all publicly available tweets
#' 2. Filtering via a search-like query (up to 400 keywords)
#' 3. Tracking via vector of user ids (up to 5000 user_ids)
#' 4. Location via geo coordinates (1-360 degree location boxes)
#'
#' Learn more in `vignette("stream", package = "rtweet")`
#'
#' @inheritParams lookup_users
#' @param q Query used to select and customize streaming collection
#'   method.  There are four possible methods:
#'
#'   1. The default, `q = ""`, returns a small random sample of all
#'      publicly available Twitter statuses.
#'   2. To filter by keyword, provide a comma separated character string with
#'      the desired phrase(s) and keyword(s).
#'   3. Track users by providing a comma separated list of user IDs or
#'      screen names.
#'   4. Use four latitude/longitude bounding box points to stream by geo
#'      location. This must be provided via a vector of length 4, e.g.,
#'      `c(-125, 26, -65, 49)`.
#' @param timeout Integer specifying number of seconds to stream tweets for.
#'   Stream indefinitely with `timeout = Inf`.
#'
#'   The stream can be interrupted at any time, and `file_name` will still be
#'   valid file.
#' @param file_name Character with name of file. If not specified,
#'   will write to a temporary file `stream_tweets*.json`.
#' @param append If `TRUE`, will append to the end of `file_name`; if
#'   `FALSE`, will overwrite.
#' @param verbose If `TRUE`, display a progress bar.
#' @param parse Use `FALSE` to opt-out of parsing the tweets.
#' @param ... Other arguments passed in to query parameters.
#' @seealso [parse_stream()].
#' @references
#' They were removed from the website.
#' @examples
#' \dontrun{
#' # stream tweets mentioning "#rstats" for 10 seconds
#' rstats1 <- stream_tweets("#rstats", timeout = 10, file_name = "rstats.json")
#' rstats1
#'
#' # Download another 10s worth of data to the same file
#' rstats2 <- stream_tweets("#rstats", timeout = 10, file_name = "rstats.json",
#'                          append = TRUE)
#'
#' # stream tweets about continental USA for 10 seconds
#' usa <- stream_tweets(location = lookup_coords("usa"), file_name = "usa.json",
#'                      timeout = 10)
#'
#' }
#' @return A tibble with one row per tweet
#' @export
#' @references
#' The webpages describing how it used to work were removed.
stream_tweets <- function(q = "",
                          timeout = 30,
                          parse = TRUE,
                          token = NULL,
                          file_name = NULL,
                          verbose = TRUE,
                          append = TRUE,
                          ...) {
  lifecycle::deprecate_stop("1.1.0", "stream_tweets()", "filtered_stream()",
                            details = "The streaming endpoint it used does no longer work.")
  if (is.null(file_name)) {
    file_name <- tempfile(pattern = "stream_tweets", fileext = ".json")
    inform(paste0("Writing to '", file_name, "'"))
  }
  output <- file(file_name)

  prep <- stream_prep(token, q, ...)
  stream <- curl::curl(prep$url, handle = prep$handle)

  quiet_interrupt(download_from_stream(stream, output,
    timeout = timeout,
    verbose = verbose
  ))

  if (parse) {
    return(parse_stream(file(file_name)))
  } else {
    return(invisible(NULL))
  }
}

download_from_stream <- function(stream, output, append = TRUE, timeout = 10, verbose = TRUE) {
  if (!timeout) {
    timeout <- Inf
  }
  stopifnot(is.numeric(timeout), timeout > 0)
  stop_time <- Sys.time() + timeout

  n_seen <- 0
  if (verbose) {
    pb <- progress::progress_bar$new(
      total = NA,
      show_after = 0,
      format = "Streaming tweets: :n tweets written / :bytes / :rate / :elapsedfull"
    )
  }

  open(stream, "rb")
  withr::defer(close(stream), current_env(), priority = "first")

  open(output, if (append) "ab" else "b")
  withr::defer(close(output), current_env(), priority = "last")

  lines <- list(lines = character(), fragment = "")
  while (isIncomplete(stream) && Sys.time() < stop_time) {
    buf <- readBin(stream, raw(), 64 * 1024)
    if (length(buf) == 0) {
      if (verbose()) {
        pb$tick()
      }
      Sys.sleep(0.25)
      next
    }

    text <- rawToChar(buf)
    lines <- whole_lines(text, lines$fragment)

    # only keep tweets
    # TODO: process more messages from
    # https://developer.twitter.com/en/docs/twitter-api/v1/tweets/filter-realtime/overview
    json <- lapply(lines$lines, jsonlite::fromJSON)
    is_tweet <- vapply(json, function(x) has_name(x, "created_at"), logical(1))

    n_seen <- n_seen + sum(is_tweet)
    if (verbose) {
      pb$tick(length(buf), tokens = list(n = n_seen))
    }

    writeLines(lines$lines[is_tweet], output, useBytes = TRUE)
  }

  if (verbose) {
    cat("\n")
  }

  invisible()
}

quiet_interrupt <- function(code) {
  tryCatch(code, interrupt = function(e) NULL)
}

whole_lines <- function(text, fragment = "") {
  lines <- strsplit(text, "\r\n")[[1]]
  lines[[1]] <- paste0(fragment, lines[[1]])

  n <- length(lines)
  complete <- grepl("\r\n$", text)
  if (!complete) {
    fragment <- lines[[n]]
    lines <- lines[-n]
  } else {
    fragment <- ""
  }

  # Drop empty keep-alive lines
  lines <- lines[lines != ""]

  list(lines = lines, fragment = fragment)
}

stream_prep <- function(token, q = "", ..., filter_level = "none") {
  token <- check_token(token)
  stopifnot(is.atomic(q) && !is.null(q) || inherits(q, "coords"))

  if (identical(q, "")) {
    path <- "1.1/statuses/sample.json"
    params <- NULL
  } else {
    path <- "1.1/statuses/filter.json"
    params <- stream_params(q, ..., filter_level = filter_level)
  }
  # Detect if q is too long in character size (using double of premium limit)
  # https://developer.twitter.com/en/docs/twitter-api/tweets/filtered-stream/introduction
  if (any(nchar(params) > 2048)) {
    warning("Detected a long parameter, this query will probably fail.", call. = FALSE)
  }

  url <- httr::modify_url("https://stream.twitter.com",
    path = path,
    query = params
  )

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, .list = token$sign("GET", url)$headers)

  list(url = url, handle = handle)
}

stream_params <- function(stream, ...) {
  if (inherits(stream, "coords")) {
    params <- list(locations = paste(stream$box, collapse = ","))
  } else if ((length(stream) %% 4 == 0) && is.numeric(stream)) {
    params <- list(locations = paste(stream, collapse = ","))
  } else if (is_user_id(stream)) {
    params <- list(follow = stream, ...)
  } else {
    params <- list(track = stream, ...)
  }

  params
}


#' Parser of stream
#'
#' Converts Twitter stream data (JSON file) into parsed data frame.
#' @param path Character, name of JSON file with data collected by
#'   [stream_tweets()].
#' @param ... Unused, keeping it for back compatibility.
#' @export
#' @seealso [stream_tweets()]
#' @examples
#' \dontrun{
#' stream_tweets(timeout = 1, file_name = "stream.json", parse = FALSE)
#' parse_stream("stream.json")
#' }
parse_stream <- function(path, ...) {
  if (...length() > 0) {
    lifecycle::deprecate_soft("1.0.0", "parse_steam(...)",
                              details = c("Parameters ignored"))
  }

  if (!is(path, "file")) {
    path <- file(path)
  }

  tweets <- jsonlite::stream_in(path, pagesize = 1, verbose = FALSE)
  tweets <- tweet(tweets)
  tweets$created_at <- format_date(tweets$created_at)

  tweets0 <- tweet(NULL)[0, ]
  if (length(tweets) != 0) {
    tweets <- tweets[, colnames(tweets0)]
  }


  users <- user(NULL)[0, ]
  if (has_name_(tweets, "user") && length(tweets$user) != 0 && all(lengths(tweets$user) != 0)) {
    users <- do.call(rbind, tweets[["user"]])[, order(colnames(users))]
  }

  tweets <- tweets[!colnames(tweets) %in% "user"]
  users <- tibble::as_tibble(users)
  tweets <- tibble::as_tibble(tweets)

  out <- structure(tweets, users = users)
  class(out) <- c("tweets", class(out))
  out
}


# Deprecated -----------------------------------------------------------------
#' A more robust version of stream_tweets
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Please use [stream_tweets()] instead.
#'
#' @param dir Name of directory in which json files should be written.
#'   The default, NULL, will create a timestamped "stream" folder in the
#'   current working directory. If a dir name is provided that does not
#'   already exist, one will be created.
#' @param append Logical indicating whether to append or overwrite
#'   file_name if the file already exists. Defaults to FALSE, meaning
#'   this function will overwrite the preexisting file_name (in other
#'   words, it will delete any old file with the same name as
#'   file_name) meaning the data will be added as new lines to file if
#'   pre-existing.
#' @return Returns data as expected using original search_tweets
#'   function.
#' @export
#' @keywords internal
stream_tweets2 <- function(..., dir = NULL, append = FALSE) {
  lifecycle::deprecate_stop("1.0.0", "stream_tweets2()","stream_tweets()")
}

#' Collect a live stream of Twitter data.
#'
#' Returns public statuses via one of the following four methods:
#'   \itemize{
#'     \item 1. Sampling a small random sample of all publicly
#'              available tweets
#'     \item 2. Filtering via a search-like query (up to 400
#'              keywords)
#'     \item 3. Tracking via vector of user ids (up to 5000
#'              user_ids)
#'     \item 4. Location via geo coordinates (1-360 degree
#'              location boxes)
#'   }
#' @param q Query used to select and customize streaming collection
#'   method.  There are four possible methods. (1) The default,
#'   \code{q = ""}, returns a small random sample of all publicly
#'   available Twitter statuses. (2) To filter by keyword, provide a
#'   comma separated character string with the desired phrase(s) and
#'   keyword(s). (3) Track users by providing a comma separated list
#'   of user IDs or screen names. (4) Use four latitude/longitude
#'   bounding box points to stream by geo location. This must be
#'   provided via a vector of length 4, e.g., c(-125, 26, -65, 49).
#' @param timeout Numeric scalar specifying amount of time, in
#'   seconds, to leave connection open while streaming/capturing
#'   tweets.  By default, this is set to 30 seconds. To stream
#'   indefinitely, use \code{timeout = FALSE} to ensure json file is
#'   not deleted upon completion or \code{timeout = Inf}.
#' @param parse Logical, indicating whether to return parsed data.  By
#'   default, \code{parse = TRUE}, this function does the parsing for
#'   you. However, for larger streams, or for automated scripts
#'   designed to continuously collect data, this should be set to
#'   false as the parsing process can eat up processing resources and
#'   time. For other uses, setting parse to TRUE saves you from having
#'   to sort and parse the messy list structure returned by
#'   Twitter. (Note: if you set parse to false, you can use the
#'   \code{\link{parse_stream}} function to parse the json file at a
#'   later point in time.)
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param file_name Character with name of file. By default, a
#'   temporary file is created, tweets are parsed and returned to
#'   parent environment, and the temporary file is deleted.
#' @param gzip Logical indicating whether to request gzip compressed
#'   stream data. By default this is set to FALSE. After performing
#'   some tests, it appears gzip requires less bandwidth, but also
#'   returns slightly fewer tweets. Use of gzip option should, in
#'   theory, make connection more reliable (by hogging less bandwidth,
#'   there's less of a chance Twitter cuts you off for getting
#'   behind).
#' @param verbose Logical, indicating whether or not to include output
#'   processing/retrieval messages.
#' @param fix.encoding Logical indicating whether to internally
#'   specify encoding to prevent possible errors caused by things such
#'   as non-ascii characters.
#' @param \dots Insert magical paramaters, spell, or potion here. Or
#'   filter for tweets by language, e.g., \code{language = "en"}.
#' @seealso \url{https://stream.twitter.com/1.1/statuses/filter.json}
#' @examples
#' \dontrun{
#' ## stream tweets mentioning "election" for 90 seconds
#' e <- stream_tweets("election", timeout = 90)
#'
#' ## data frame where each observation (row) is a different tweet
#' e
#'
#' ## users data also retrieved, access it via users_data()
#' users_data(e)
#'
#' ## plot tweet frequency
#' ts_plot(e, "secs")
#'
#' ## stream tweets mentioning Obama for 30 seconds
#' djt <- stream_tweets("realdonaldtrump", timeout = 30)
#'
#' ## preview tweets data
#' djt
#'
#' ## get user IDs of people who mentioned trump
#' usrs <- users_data(djt)
#'
#' ## lookup users data
#' usrdat <- lookup_users(unique(usrs$user_id))
#'
#' ## preview users data
#' usrdat
#'
#' ## store large amount of tweets in files using continuous streams
#' ## by default, stream_tweets() returns a random sample of all tweets
#' ## leave the query field blank for the random sample of all tweets.
#' stream_tweets(
#'   timeout = (60 * 10),
#'   parse = FALSE,
#'   file_name = "tweets1"
#' )
#' stream_tweets(
#'   timeout = (60 * 10),
#'   parse = FALSE,
#'   file_name = "tweets2"
#' )
#'
#' ## parse tweets at a later time using parse_stream function
#' tw1 <- parse_stream("tweets1.json")
#' tw1
#'
#' tw2 <- parse_stream("tweets2.json")
#' tw2
#'
#' ## streaming tweets by specifying lat/long coordinates
#'
#' ## stream continental US tweets for 5 minutes
#' usa <- stream_tweets(
#'   c(-125, 26, -65, 49),
#'   timeout = 300
#' )
#'
#' ## use lookup_coords() for a shortcut verson of the above code
#' usa <- stream_tweets(
#'   lookup_coords("usa"),
#'   timeout = 300
#' )
#'
#' ## stream world tweets for 5 mins, save to json file
#' ## shortcut coords note: lookup_coords("world")
#' world.old <- stream_tweets(
#'   c(-180, -90, 180, 90),
#'   timeout = (60 * 5),
#'   parse = FALSE,
#'   file_name = "world-tweets.json"
#' )
#'
#' ## read in json file
#' rtworld <- parse_stream("word-tweets.json")
#'
#' ## world data set with with lat lng coords variables
#' x <- lat_lng(rtworld)
#'
#' }
#'
#' @return Tweets data returned as data frame with users data as attribute.
#' @family stream tweets
#' @importFrom httr POST write_disk add_headers progress timeout
#' @export
stream_tweets <- function(q = "",
                          timeout = 30,
                          parse = TRUE,
                          token = NULL,
                          file_name = NULL,
                          gzip = FALSE,
                          verbose = TRUE,
                          fix.encoding = TRUE,
                          ...) {

  if (all(fix.encoding,
    !identical(getOption("encoding"), "UTF-8"))) {
    op <- getOption("encoding")
    options(encoding = "UTF-8")
    on.exit(options(encoding = op))
  }
  token <- check_token(token)
  if (!timeout) {
    timeout <- Inf
  }
  stopifnot(
    is.numeric(timeout), timeout > 0,
    any(is.atomic(q), inherits(q, "coords")),
    is.atomic(file_name)
  )
  if (missing(q)) q <- ""
  if (identical(q, "")) {
    query <- "statuses/sample"
    params <- NULL
  } else {
    query <- "statuses/filter"
    params <- stream_params(q, ...)
  }
  url <- make_url(
    restapi = FALSE,
    query,
    param = params
  )
  tmp <- FALSE
  if (is.null(file_name)) {
    tmp <- TRUE
    file_name <- tempfile(fileext = ".json")
  }
  if (is.infinite(timeout)) tmp <- FALSE
  if (!grepl("\\.json$", file_name)) {
    file_name <- paste0(file_name, ".json")
  }
  if (!file.exists(file_name)) file.create(file_name)
  if (verbose) {
    message(paste0("Streaming tweets for ",
      timeout, " seconds..."))
  }
  r <- NULL
  if (gzip) {
    r <- tryCatch(httr::POST(
      url = url,
      httr::config(token = token, timeout = timeout),
      httr::write_disk(file_name, overwrite = TRUE),
      httr::add_headers(`Accept-Encoding` = "deflate, gzip"),
      httr::progress()),
      error = function(e) return(NULL)
    )

  } else {
    r <- tryCatch(httr::POST(
      url = url,
      httr::config(token = token, timeout = timeout),
      httr::write_disk(file_name, overwrite = TRUE),
      httr::progress()),
      error = function(e) return(NULL)
    )
  }

  if (verbose) {
    message("Finished streaming tweets!")
  }
  if (parse) {
    out <- stream_data(file_name)
    if (tmp) {
      file.remove(file_name)
    } else {
      message("streaming data saved as ", file_name)
    }
    return(out)
  } else {
    message("streaming data saved as ", file_name)
    invisible()
  }
}

stream_params <- function(stream, ...) {
  if (inherits(stream, "coords")) {
    stream <- stream$box
  }
  if (length(stream) > 1) {
    params <- list(locations = paste(stream, collapse = ","))
  } else if (!all(suppressWarnings(is.na(as.numeric(stream))))) {
    params <- list(follow = stream, ...)
  } else {
    params <- list(track = stream, ...)
  }
  params[["filter_level"]] <- "low"
  params
}




#' @importFrom jsonlite stream_in
stream_data <- function(file_name, ...) {
  tw <- .parse_stream(file_name, ...)
  usr <- users_data(tw)
  if (nrow(tw) > 1L) {
    tw <- tw[!is.na(tw$status_id), ]
  }
  if (nrow(usr) > 1L) {
    usr <- usr[!is.na(usr$user_id), ]
  }
  attr(tw, "users") <- usr
  tw
}

.parse_stream <- function(file_name) {
  if (!identical(getOption("encoding"), "UTF-8")) {
    op <- getOption("encoding")
    options(encoding = "UTF-8")
    on.exit(options(encoding = op))
  }

  s <- tryCatch(suppressWarnings(
    jsonlite::stream_in(file_name, verbose = TRUE)),
    error = function(e) return(NULL)
  )

  if (is.null(s)) {
    rl <- readr::read_lines(file_name)
    rl <- grep("^\\{\"created\\_at.*\"\\}$", rl, value = TRUE)
    if (length(rl) < 2L) return(tweets_with_users(NULL))
    readr::write_lines(rl, file_name)
    s <- tryCatch(suppressWarnings(
      jsonlite::stream_in(file(file_name),
        verbose = TRUE)),
      error = function(e) return(NULL))
  }
  if (is.null(s)) {
    rl <- readr::read_lines(file_name)
    if (length(rl) < 2L) return(tweets_with_users(NULL))
    readr::write_lines(
      rl[-length(rl)],
      file_name
    )
    s <- tryCatch(suppressWarnings(
      jsonlite::stream_in(file(file_name),
                          verbose = TRUE)),
      error = function(e) return(NULL))
  }
  if (is.null(s)) stop(paste0(
    "wasn't able to parse-in json file. ",
    "normal fixes didn't work. perhaps you should try ",
    "starting a new R session.",
    call. = FALSE))
  tweets_with_users(s)
}



data_from_stream <- function(x, n = 10000L, n_max = -1L) {
  if (!file.exists(x)) {
    stop("No such file exists", call. = FALSE)
  }
  ## initalize counters and output vector
  d <- NA_character_
  skip <- 0L
  data <- list()
  ## read in chunks until completion
  if (identical(n_max, -1L)) {
    n_max2 <- Inf
  } else {
    n_max2 <- n_max
  }
  while (length(d) > 0L && skip < n_max2) {
    #if (n_max > 0L && (skip + n) > n_max2) {
    #  n <- n_max - skip
    #}
    d <- readr::read_lines(x, skip = skip, n_max = n)
    skip <- length(d) + skip
    tmp <- tempfile()
    readr::write_lines(d, tmp)
    data[[length(data) + 1L]] <- stream_data(tmp)
    if (NROW(data[[length(data)]]) == 0L) break
  }
  #twt <- do.call("rbind", data)
  #usr <- do.call("rbind", lapply(data, attr, "users"))
  #attr(twt, "users") <- usr
  do_call_rbind(data)
  #twt
}


#' Converts Twitter stream data (json file) into parsed data frame.
#'
#' @param path Character, name of json file with data collected by
#'   \code{\link{stream_tweets}}.
#' @param ... Other arguments passed on to interal data_from_stream
#'   function.
#' @return A tbl of tweets data with attribute of users data
#' @examples
#' \dontrun{
#' ## run and save stream to json file
#' stream_tweets(
#'   "the,a,an,and", timeout = 60,
#'   file_name = "theaanand.json",
#'   parse = FALSE
#' )
#'
#' ## parse stream file into tibble data frame
#' rt <- parse_stream("theaanand.json")
#' }
#' @export
#' @family stream tweets
parse_stream <- function(path, ...) {
  dots <- list(...)
  if (length(dots) > 0L) {
    do.call("data_from_stream", c(path, dots))
  } else {
    eval(call("data_from_stream", path))
  }
}


#' Stream with hardwired reconnection method to ensure timeout integrity.
#'
#' @param ... Args passed to \code{\link{stream_tweets}} function.
#' @return Returns data as expected using original search_tweets function.
#' @export
#' @importFrom readr read_lines write_lines
stream_tweets2 <- function(...) {
  ## capture dots
  dots <- list(...)

  ## start time
  start <- Sys.time()
  ## finish time (given requested timeout)
  reqtime <- start + dots[["timeout"]]

  ## initialize output vector
  rt <- list()

  ## start counter
  i <- 1L

  ## save file name for final file
  file_name <- dots[["file_name"]]

  ## create temp dir for while streams
  tmp <- tempdir()
  dots[["file_name"]] <- file.path(tmp, paste0(i, ".json"))

  ## store parse value, then override to FALSE
  parse <- dots[["parse"]]
  dots[["parse"]] <- FALSE

  ## restart and continue stream until reqtime
  while (Sys.time() <= reqtime) {
    do.call("stream_tweets", dots)
    i <- i + 1L
    dots[["timeout"]] <- ceiling(as.numeric(reqtime - Sys.time(), "secs"))
    dots[["file_name"]] <- file.path(tmp, paste0(i, ".json"))
  }

  ## merge json files into single file (named file_name)
  jsons <- list.files(tmp, pattern = "\\.json$", full.names = TRUE)
  for (i in jsons) {
    x <- readr::read_lines(i, progress = FALSE)
    readr::write_lines(x, file_name, append = TRUE)
  }
  if (!parse) {
    return(invisible())
  }
  ## return parsed data
  parse_stream(file_name)
}

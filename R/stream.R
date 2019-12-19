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
#'   indefinitely, use \code{timeout = FALSE} to ensure JSON file is
#'   not deleted upon completion or \code{timeout = Inf}.
#' @param parse Logical, indicating whether to return parsed data.  By
#'   default, \code{parse = TRUE}, this function does the parsing for
#'   you. However, for larger streams, or for automated scripts
#'   designed to continuously collect data, this should be set to
#'   false as the parsing process can eat up processing resources and
#'   time. For other uses, setting parse to TRUE saves you from having
#'   to sort and parse the messy list structure returned by
#'   Twitter. (Note: if you set parse to false, you can use the
#'   \code{\link{parse_stream}} function to parse the JSON file at a
#'   later point in time.)
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @param file_name Character with name of file. By default, a
#'   temporary file is created, tweets are parsed and returned to
#'   parent environment, and the temporary file is deleted.
#' @param verbose Logical, indicating whether or not to include output
#'   processing/retrieval messages.
#' @param \dots Insert magical parameters, spell, or potion here. Or
#'   filter for tweets by language, e.g., \code{language = "en"}.
#' @seealso \url{https://developer.twitter.com/en/docs/tweets/sample-realtime/api-reference/decahose}
#' @examples
#' \dontrun{
#' ## stream tweets mentioning "election" for 90 seconds
#' e <- stream_tweets("election", timeout = 90)
#'
#' ## data frame where each observation (row) is a different tweet
#' e
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
#' ## stream world tweets for 5 mins, save to JSON file
#' ## shortcut coords note: lookup_coords("world")
#' world.old <- stream_tweets(
#'   c(-180, -90, 180, 90),
#'   timeout = (60 * 5),
#'   parse = FALSE,
#'   file_name = "world-tweets.json"
#' )
#'
#' ## read in JSON file
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
                          verbose = TRUE,
                          ...) {
  if ("append" %in% names(list(...))) {
    stop("append should only be used with stream_tweets2() (which is in development)",
         call. = FALSE)
  }
  ## set encoding
  if (!identical(getOption("encoding"), "UTF-8")) {
    op <- getOption("encoding")
    options(encoding = "UTF-8")
    on.exit(options(encoding = op), add = TRUE)
  }
  token <- check_token(token)
  if (!timeout) {
    timeout <- Inf
  }
  ## setp q to blank for sample stream
  if (missing(q) || is.null(q)) q <- ""
  stopifnot(
    is.numeric(timeout),
    timeout > 0,
    any(is.atomic(q), inherits(q, "coords")),
    is.atomic(file_name)
  )
  ## select stream API
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
  ## temp file if none provided
  if (is.null(file_name)) {
    tmp <- TRUE
    file_name <- tmp_json()
  } else {
    tmp <- FALSE
  }
  if (is.infinite(timeout)) tmp <- FALSE
  if (!grepl("\\.json$", file_name)) {
    file_name <- paste0(file_name, ".json")
  }
  if (!file.exists(file_name)) file.create(file_name)
  if (verbose) {
    message(
      paste0("Streaming tweets for ", timeout, " seconds...")
    )
  }
  r <- NULL
  con <- file(file_name, "wt")
  on.exit({sh <- tryCatch(close(con), error = function(e) return(NULL),
    warning = function(w) return(NULL))}, add = TRUE)

  start_time <- Sys.time()
  stop_time <- Sys.time() + timeout
  ctr <- 0
  while (timeout > 0) {
    r <- tryCatch(httr::POST(
      url = url,
      httr::config(token = token, timeout = timeout),
      httr::write_stream(write_fun(con)),
      httr::add_headers(Accept = "application/json"),
      httr::add_headers(`Accept-Encoding` = "gzip, deflate")),
      error = function(e) return(e))
    timeout <- as.numeric(difftime(stop_time, Sys.time(), units = "secs"))
    if (timeout > 0) {
      ctr <- ctr + 1
      if (ctr == 1 && verbose) message(
        "The stream disconnected prematurely. Reconnecting...")
      if (ctr == 2 && verbose) message("Reconnecting again...")
      if (ctr == 5) break
    } else if (verbose) {
      message("Finished streaming tweets!")
    }
  }
  close(con)
  if (parse) {
    out <- parse_stream(file_name, verbose = verbose)
    if (tmp) {
      file.remove(file_name)
    }
    return(out)
  }
  if (verbose) message("streaming data saved as ", file_name)
  invisible(r)
}

write_fun <- function(con) {
  function(x) {
    writeLines(rawToChar(x), con)
  }
}

tmp_json <- function() {
  timestamp <- gsub("[^[:digit:]_]", "", Sys.time())
  paste0("stream-", timestamp, ".json")
}

is_user_ids <- function(x) {
  if (length(x) == 1L && grepl(",", x)) {
    x <- strsplit(x, "\\,")[[1]]
  }
  isTRUE(all(!is.na(suppressWarnings(as.numeric(x)))))
}

stream_params <- function(stream, ...) {
  ## gotta have ut8-encoding for the comma separated IDs
  op <- getOption("encoding")
  on.exit(options(encoding = op), add = TRUE)
  options(encoding = "UTF-8")

  if (inherits(stream, "coords")) {
    stream <- stream$box
  }
  ## if [coordinates] vector is > 1 then locations
  ## if comma separated stream of IDs then follow
  ## otherwise use query string to track
  if ((length(stream) %% 4 == 0) && is.numeric(stream)) {
    params <- list(locations = paste(stream, collapse = ","))
  } else if (is_user_ids(stream)) {
    params <- list(follow = stream, ...)
  } else {
    params <- list(track = stream, ...)
  }
  ## if filter level not provided, set to low
  if (!has_name_(params, "filter_level")) {
    ## filter level
    params[["filter_level"]] <- "none"
  }
  params
}



good_lines <- function(x) {
  grep("^\\{\"created.*ms\":\"\\d+\"\\}$", x, value = TRUE)
}
good_lines2 <- function(x) {
  x <- x[nchar(x) > 0]
  x <- grep("{\"delete", x, fixed = TRUE, invert = TRUE, value = TRUE)
  x <- grep("{\"limit", x, fixed = TRUE, invert = TRUE, value = TRUE)
  co <- grep("\\d+\"\\}$", x, invert = TRUE)
  if (length(co) > 0) {
    for (i in seq_along(co)) {
      if (co[i] + 1 > length(x)) break
      x[co[i]] <- paste0(x[co[i]], x[co[i] + 1])
    }
    x <- x[-c(co + 1)]
    while (!grepl("\\d+\"\\}$", x[length(x)])) {
      x <- x[-length(x)]
      if (length(x) == 0) break
    }
  }
  x
}

limits_data <- function(x) {
  if (has_name_(attributes(x), "limit")) {
    attr(x, "limit")
  } else {
    data.frame()
  }
}

stream_data <- function(file_name, ...) {
  .parse_stream(file_name, ...)
}


.parse_stream_two <- function(d) {
  d <- paste0("[", paste0(d, collapse = ","), "]")
  d <- jsonlite::fromJSON(d)
  tweets_with_users(d)
}


#' @importFrom jsonlite stream_in
.parse_stream <- function(file_name, ...) {
  if (!identical(getOption("encoding"), "UTF-8")) {
    op <- getOption("encoding")
    options(encoding = "UTF-8")
    on.exit(options(encoding = op), add = TRUE)
  }
  s <- tryCatch(jsonlite::stream_in(file(file_name), ...), error = function(e)
    return(NULL))
  if (is.null(s)) {
    d <- readr::read_lines(file_name)
    if (length(d) > 0) {
      tmp <- tempfile()
      on.exit(file.remove(tmp), add = TRUE)
      d <- good_lines2(d)
    }
    if (length(d) > 0) {
      dd <- sapply(d, function(x) {
        o <- tryCatch(jsonlite::fromJSON(x),
          error = function(e) return(FALSE))
        if (identical(o, FALSE)) return(FALSE)
        return(TRUE)
      }, USE.NAMES = FALSE)
      writeLines(d[dd], tmp)
      s <- jsonlite::stream_in(file(tmp, "rb"))
    }
  }
  if (length(s) == 0L) s <- NULL
  tweets_with_users(s)
}

data_from_stream <- function(x, n = 10000L, n_max = -1L, ...) {
  if (!file.exists(x)) {
    stop("No such file exists", call. = FALSE)
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    warning("For better performance when reading large twitter .json files, ",
      "try installing the readr package before using this function.")
    return(stream_data(x, ...))
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
    if (n_max > 0L && (skip + n) > n_max2) {
      n <- n_max - skip
    }
    d <- readr::read_lines(x, skip = skip, n_max = n)
    if (length(d) == 0) break
    skip <- length(d) + skip
    tmp <- tempfile()
    d <- good_lines2(d)
    if (length(d) == 0) break
    readr::write_lines(d, tmp)
    data[[length(data) + 1L]] <- stream_data(tmp, ...)
    if (NROW(data[[length(data)]]) == 0L) break
  }
  do.call("rbind", data)
}



data_from_stream2 <- function(x, n = 10000L, n_max = -1L, ...) {
  if (!inherits(x, "connection") && !file.exists(x)) {
    stop("No such file exists", call. = FALSE)
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    warning("For better performance when reading large twitter .json files, ",
      "try installing the readr package before using this function.")
    return(stream_data(x, ...))
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
    if (n_max > 0L && (skip + n) > n_max2) {
      n <- n_max - skip
    }
    d <- readr::read_lines(x, skip = skip, n_max = n)
    skip <- length(d) + skip
    d <- good_lines2(d)
    if (length(d) == 0) break
    data[[length(data) + 1L]] <- .parse_stream_two(d)
    if (NROW(data[[length(data)]]) == 0L) break
  }
  do.call("rbind", data)
}


#' Converts Twitter stream data (JSON file) into parsed data frame.
#'
#' @param path Character, name of JSON file with data collected by
#'   \code{\link{stream_tweets}}.
#' @param ... Other arguments passed on to internal data_from_stream
#'   function.
#' @return A tbl of tweets data with attribute of users data
#' @examples
#' \dontrun{
#' ## run and save stream to JSON file
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
    do.call("data_from_stream2", c(path, dots))
  } else {
    eval(call("data_from_stream2", path))
  }
}


#' A more robust version of stream_tweets
#'
#' Stream with hardwired reconnection method to ensure timeout integrity.
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
#' @rdname stream_tweets
stream_tweets2 <- function(..., dir = NULL, append = FALSE) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("this function requires the readr package. please install it first")
  }
  if (is.null(dir)) {
    dir <- stream_dir()
  }
  if (!dir.exists(dir)) {
    new_dir(dir)
  }

  ## capture and match dots
  dots <- match_fun(list(...), "stream_tweets")
  ## start time
  start <- Sys.time()
  ## finish time (given requested timeout)
  reqtime <- start + dots[["timeout"]]

  ## save file name for final file
  file_name <- dots[["file_name"]]
  if (is.null(file_name)) {
    file_name <- "stream"
  } else if (grepl("\\.json$", file_name)) {
    file_name <- gsub("\\.json$", "", file_name)
  }
  ## store parse value, then override to FALSE
  parse <- dots[["parse"]]
  dots[["parse"]] <- FALSE
  ## store verbose value, then override to FALSE
  verbose <- dots[["verbose"]]
  dots[["verbose"]] <- FALSE

  ## display message if verbose
  if (verbose) {
    message(paste0("Streaming tweets for ", dots[["timeout"]], " seconds..."))
  }

  ## initialize output vector
  rt <- list()
  ## start counter
  i <- 1L

  ## restart and continue stream until reqtime
  while (Sys.time() <= reqtime) {
    dots[["file_name"]] <- file.path(dir, paste0(file_name, "-", i, ".json"))
    rt[[length(rt) + 1L]] <- do.call("stream_tweets", dots)
    i <- i + 1L
    dots[["timeout"]] <- ceiling(as.numeric(reqtime - Sys.time(), "secs"))
  }
  if (verbose) {
    message("Finished streaming tweets!")
  }
  ## merge JSON files into single file (named file_name)
  pat <- paste0(file_name, "\\-[[:digit:]]{1,}\\.json$")
  jsons <- list.files(dir, pattern = pat, full.names = TRUE)
  file_name <- paste0(dir, ".json")
  unlink(dir, recursive = TRUE)
  if (!parse) {
    return(invisible())
  }
  ## return parsed data
  parse_stream(file_name)
}

new_dir <- function(dir, force = TRUE) {
  stopifnot(is.character(dir) && length(dir) == 1L)
  if (force && dir.exists(dir)) {
    dirs <- list.dirs(recursive = FALSE)
    old_dir <- dir
    dir <- paste0(dir, "-", 1:1000)
    dir <- dir[!dir %in% dirs][1]
    message(old_dir, " already exists. creating ", dir, "...")
  }
  dir.create(dir)
}

match_fun <- function(dots, fun) {
  rfuns <- names(formals(fun))
  nms <- match(names(dots), rfuns)
  nms[names(dots) != ""] <- names(dots)[names(dots) != ""]
  is_na <- function(x) is.na(x) | x == "NA"
  nms[is_na(nms) & names(dots) == ""] <- names(
    formals(fun))[which(is_na(nms) & names(dots) == "")]
  names(dots) <- nms
  names(dots)[is.na(names(dots))] <- ""
  fmls <- formals(fun)
  dotsdots <- dots[!names(dots) %in% names(fmls)]
  dots <- dots[names(dots) %in% names(fmls)]
  fmls <- fmls[!names(fmls) %in% names(dots) & names(fmls) != "..."]
  c(dots, fmls, dotsdots)
}


parse_streamlimit <- function(x) {
  x <- grep("^\\{\"limit", x, value = TRUE)
  x <- strsplit(x, ":|,|\"")
  x <- x[lengths(x) >= 7L]
  tibble::tibble(
    track = unlist(lapply(x, "[[", 7L)),
    timestamp = as.POSIXct(
      as.numeric(unlist(lapply(x, "[[", 12L))) / 1000, origin = "1970-01-01")
  )
}

stream_dir <- function() {
  timestamp <- gsub("\\s|\\:|\\-", "", substr(Sys.time(), 1, 19))
  paste0("stream-", timestamp)
}



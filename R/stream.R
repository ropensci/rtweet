#' stream_tweets
#'
#' @description Returns public statuses via one of three methods
#'   described below. By design, this function deciphers which
#'   method to use when processing the \code{stream} argument.
#' \itemize{
#'   \item 1. Filtering via a search-like query (up to 400 keywords)
#'   \item 2. Tracking via vector of user ids (up to 5000 user_ids)
#'   \item 3. Location via geo coordinates (1-360 degree location boxes)
#' }
#' @param q Character vector with desired phrases and keywords
#'   used to filter tweets, a comma separated list of desired
#'   user IDs to track, or a set of bounding boxes to track. If
#'   left empty, the default \code{q = ""}, stream function will
#'   return sample of all tweets.
#' @param timeout Numeric scalar specifying amount of time, in seconds,
#'   to leave connection open while streaming/capturing tweets.
#'   By default, this is set to 30 seconds. To stream indefinitely,
#'   use \code{timeout = FALSE} to ensure json file is not deleted
#'   upon completion or \code{timeout = Inf}.
#' @param parse Logical, indicating whether to return parsed data.
#'   By default, \code{parse = TRUE}, this function does the parsing for
#'   you. However, for larger streams, or for automated scripts designed
#'   to continuously collect data, this should be set to false as the
#'   parsing process can eat up processing resources and time. For other
#'   uses, setting parse to TRUE saves you from having to sort and parse
#'   the messy list structure returned by Twitter. (Note: if you set parse
#'   to false, you can use the \code{\link{parse_stream}} function to
#'   parse the json file at a later point in time.)
#' @param clean_tweets logical indicating whether to remove non-ASCII
#'   characters in text of tweets. defaults to TRUE.
#' @param as_double logical indicating whether to handle ID variables
#'   as double (numeric) class. By default, this is set to FALSE, meaning
#'   ID variables are treated as character vectors. Setting this to
#'   TRUE can provide performance (speed and memory) boost but can also
#'   lead to issues when printing and saving, depending on the format.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param file_name Character with name of file. By default, a temporary
#'   file is created, tweets are parsed and returned to parent environment,
#'   and the temporary file is deleted.
#' @param gzip Logical indicating whether to request gzip compressed
#'   stream data. By default this is set to FALSE. After performing some
#'   tests, it appears gzip requires less bandwidth, but also returns
#'   slightly fewer tweets. Use of gzip option should, in theory, make
#'   connection more reliable (by hogging less bandwidth, there's less of
#'   a chance Twitter cuts you off for getting behind).
#' @param verbose Logical, indicating whether or not to include output
#'   processing/retrieval messages.
#' @param \dots Insert magical paramaters, spell, or potion here.
#' @seealso \url{https://stream.twitter.com/1.1/statuses/filter.json}
#' @examples
#' \dontrun{
#' # stream tweets mentioning "election" for 90 seconds
#' e <- stream_tweets("election", timeout = 90)
#'
#' # data frame where each observation (row) is a different tweet
#' e
#'
#' # users data also retrieved. can access it via users_data()
#' users_data(e)
#'
#' # stream tweets mentioning Obama for 30 seconds
#' djt <- stream_tweets("realdonaldtrump", timeout = 30)
#' djt # prints tweets data preview
#' users_data(djt) # prints users data preview
#'
#' # store large amount of tweets in files using continuous streams
#' # by default, stream_tweets() returns a random sample of all tweets
#' # leave the query field blank for the random sample of all tweets.
#' stream_tweets(timeout = (60 * 10), parse = FALSE, file_name = "tweets1")
#' stream_tweets(timeout = (60 * 10), parse = FALSE, file_name = "tweets2")
#'
#' # parse tweets at a later time using parse_stream function
#' tw1 <- parse_stream("tweets1.json")
#' tw1
#'
#' tw2 <- parse_stream("tweets2.json")
#' tw2
#' }
#'
#' @return Tweets data returned as data frame with users data as attribute.
#' @family tweets
#' @importFrom httr POST write_disk add_headers progress timeout
#' @export
stream_tweets <- function(q = "", timeout = 30, parse = TRUE,
  clean_tweets = TRUE, as_double = FALSE, token = NULL, file_name = NULL,
  gzip = FALSE, verbose = TRUE, ...) {

  token <- check_token(token)

  if (!timeout) {
    timeout <- Inf
  }

  stopifnot(
    is.numeric(timeout), timeout > 0,
    is.atomic(q), is.atomic(file_name))

  if (missing(q)) q <- ""

  if (identical(q, "")) {
    query <- "statuses/sample"
    params <- NULL
  } else {
    query <- "statuses/filter"
    params <- stream_params(q)
  }

  url <- make_url(
    restapi = FALSE,
    query,
    param = params)

  tmp <- FALSE

  if (is.null(file_name)) {
  	tmp <- TRUE
  	file_name <- tempfile(fileext = ".json")
  }

  if (is.infinite(timeout)) tmp <- FALSE

  if (!grepl(".json", file_name)) {
    file_name <- paste0(file_name, ".json")
  }

  if (!file.exists(file_name)) file.create(file_name)

  if (verbose) {
    message(paste0("Streaming tweets for ", timeout, " seconds..."))
  }

  r <- NULL

  if (gzip) {
  	r <- tryCatch(POST(url = url,
  		config = token, write_disk(file_name, overwrite = TRUE),
  		add_headers(`Accept-Encoding` = "deflate, gzip"),
  		progress(), timeout(timeout)),
  		error = function(e) return(NULL))
  } else {
  	r <- tryCatch(POST(url = url,
  		config = token, write_disk(file_name, overwrite = TRUE),
  		progress(), timeout(timeout)),
  		error = function(e) return(NULL))
  }

  if (verbose) {
  	message("Finished streaming tweets!")
  }

  if (parse) {
  	out <- parse_stream(file_name, clean_tweets = clean_tweets, as_double = as_double)
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



#' parse_stream
#'
#' @param file_name name of file to be parsed. NOTE: if file
#'   was created via \code{\link{stream_tweets}}, then it will
#'   end in ".json" (see example below)
#' @param clean_tweets logical indicating whether to remove non-ASCII
#'   characters in text of tweets. defaults to TRUE.
#' @param as_double logical indicating whether to handle ID variables
#'   as double (numeric) class. By default, this is set to FALSE, meaning
#'   ID variables are treated as character vectors. Setting this to
#'   TRUE can provide performance (speed and memory) boost but can also
#'   lead to issues when printing and saving, depending on the format.
#'
#' @return Parsed tweets data with users data attribute.
#'
#' @examples
#' \dontrun{
#' stream_tweets(q = "", file_name = "tw", parse = FALSE)
#' tw <- parse_stream("tw.json")
#' tw
#' }
#' @importFrom jsonlite stream_in
#' @export
parse_stream <- function(file_name, clean_tweets = TRUE,
                         as_double = FALSE) {

	s <- tryCatch(suppressWarnings(
    stream_in(file(file_name),
		verbose = TRUE)), error = function(e) return(NULL))

	if (is.null(s)) {
		rl <- readLines(file_name, warn = FALSE)
		cat(paste0(rl[seq_len(length(rl) - 1)],
		  collapse = "\n"),
      file = file_name)

		s <- tryCatch(suppressWarnings(
      stream_in(file(file_name),
			verbose = TRUE)), error = function(e) return(NULL))
	}
	if (is.null(s)) {
		cat("\n", file = file_name, append = TRUE)

		s <- tryCatch(suppressWarnings(
      stream_in(file(file_name),
			verbose = TRUE)), error = function(e) return(NULL))
	}
	if (is.null(s)) stop("it's not right. -luther",
    call. = FALSE)

	s <- parser(s, clean_tweets = clean_tweets,
	  as_double = as_double)

	attr_tweetusers(s)
}

#' @keywords internal
stream_params <- function(stream, ...) {
  if (length(stream) > 1) {
    params <- list(location = paste(stream, collapse = ","))
  } else if (!all(suppressWarnings(is.na(as.numeric(stream))))) {
    if (all(is.integer(as.numeric(stream)))) {
      params <- list(follow = stream, ...)
    }
  } else {
    params <- list(track = stream, ...)
  }

  params[["filter_level"]] <- "low"
  params
}

#' parse_stream_xl
#'
#' Returns tweets data frame from large json file
#'
#' @param x Path name for json file
#' @param by Number of Tweets to per chunk. By default this is set to
#'   10,000 tweets.
#' @return Data frame of tweets data
#' @details Reading and simplifying json files can be very slow. To
#'   make things more managable, \code{parse_stream_xl} does one chunk
#'   of Tweets at a time and then compiles the data into a data frame.
#' @export
parse_stream_xl <- function(x, by = 10000) {
  stopifnot(is.character(x), is.numeric(by))
  x <- readLines(x)
  n <- length(x)
  N <- ceiling(n / by)
  jmin <- 1L
  df <- vector("list", N)
  for (i in seq_len(N)) {
    tmp <- tempfile()
    jmax <- jmin + (by - 1)
    if (jmax > n) jmax <- n
    cat(paste(x[jmin:jmax], collapse = "\n"),
      file = tmp, append = FALSE)
    df[[i]] <- parse_stream(tmp)
    if (jmax >= n) break
    jmin <- jmax + 1
    message(i, " of ", N)
  }
  do.call("rbind", df)
}

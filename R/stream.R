#' stream_tweets
#'
#' @description Returns public statuses via one of three methods
#'   described below. By default, this function deciphers which
#'   method is when processing the \code{stream} argument.
#'
#'   1. Filtering via a search-like query (up to 400 keywords)
#'   2. Tracking via vector of user ids (up to 5000 user_ids)
#'   3. Location via geo coordinates (1-360 degree location boxes)
#'
#' @param q Character vector with desired phrases and keywords
#'   used to filter tweets, a comma separated list of desired
#'   user IDs to track, or a set of bounding boxes to track. If
#'   left empty, the default \code{q = ""}, stream function will
#'   return sample of all tweets.
#' @param timeout Numeric specifying amount of time, in seconds,
#'   to leave connection open while streaming/capturing tweets.
#'   By default, this is set at 30 seconds.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param file_name Character with name of file. By default, this
#'   generates random file name and parses tweets.
#' @param gzip Logical indicating whether to request gzip compressed
#'   stream data.
#' @param verbose Logical, indicating whether or not to output
#'   processing/retrieval messages.
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
#' }
#'
#' @return Tweets data returned as a tibble data_frame
#' @export
stream_tweets <- function(q = "", timeout = 30, parse = TRUE,
                          token = NULL, file_name = NULL,
                          gzip = FALSE, verbose = TRUE) {

  token <- check_token(token)

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

  if (!grepl(".json", file_name)) {
    file_name <- paste0(file_name, ".json")
  }

  if (!file.exists(file_name)) file.create(file_name)

  if (verbose) {
    message(paste0("Streaming tweets for ", timeout, " seconds..."))
  }

  r <- NULL

  if (gzip) {
  	r <- tryCatch(GET(url = url,
  		config = token, write_disk(file_name, overwrite = TRUE),
  		add_headers(`Accept-Encoding` = "deflate, gzip"),
  		progress(), timeout(timeout)),
  		error = function(e) return(NULL))
  } else {
  	r <- tryCatch(GET(url = url,
  		config = token, write_disk(file_name, overwrite = TRUE),
  		progress(), timeout(timeout)),
  		error = function(e) return(NULL))
  }

  if (!is.null(r)) {
  	return(r)
  }

  if (verbose) {
  	message("Finished streaming tweets!")
  }

  if (parse) {
  	out <- parse_stream(file_name)
  	if (tmp) file.remove(file_name)
  	return(out)
  } else {
  	invisible()
  }
}



#' parse_stream
#'
#' @param file_name name of file to be parsed. NOTE: if file
#'   was created via \code{\link{stream_tweets}}, then it will
#'   end in ".json" (see example below)
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
parse_stream <- function(file_name) {

	s <- tryCatch(stream_in(file(file_name),
		verbose = TRUE), error = function(e) return(NULL))

	if (is.null(s)) {
		rl <- readLines(file_name)
		cat(rl[[seq_len(length(rl) - 1)]], file = file_name)

		s <- tryCatch(stream_in(file(file_name),
			verbose = TRUE), error = function(e)
				stop("it's not right. -luther", call. = FALSE))
	}

	s <- parser(s)

	attr_tweetusers(s)
}

#' @keywords internal
stream_params <- function(stream) {
  stream <- unlist(trimws(unlist(strsplit(stream, ","))))

  if (!all(suppressWarnings(is.na(as.numeric(stream))))) {
    if (all(is.integer(as.integer(stream)))) {
      params <- list(follow = stream)
    } else {
      params <- list(locations = stream)
    }
  } else {
    params <- list(track = stream)
  }

  c(params, filter_level = "low", count = 10000)
}

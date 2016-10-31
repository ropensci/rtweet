#' search_tweets
#'
#' @description Returns two data frames (tweets data and users data)
#'   using a provided search query.
#'
#' @param q Character, search query of no greater than
#'   500 characters maximum.
#' @param n Numeric, specifying the total number of desired tweets to
#'   return. Defaults to 100. Maximum number of tweets returned from
#'   a single token is 18,000. See details for more information.
#' @param type Character, specifies what type of search results
#'   you would prefer to receive. The current default is
#'   \code{type = "mixed"}, which is a mix between the other two
#'   valid values \code{type = "recent"} and \code{type = "popular"}.
#' @param include_rts Logical, indicating whether to include retweets
#'   in search results.
#' @param max_id Character, specifying the [oldest] status id beyond
#'   which results should resume returning.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
#' @param clean_tweets logical indicating whether to remove non-ASCII
#'   characters in text of tweets. defaults to FALSE.
#' @param as_double logical indicating whether to handle ID variables
#'   as double (numeric) class. By default, this is set to FALSE, meaning
#'   ID variables are treated as character vectors. Setting this to
#'   TRUE can provide performance (speed and memory) boost but can also
#'   lead to issues when printing and saving, depending on the format.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param verbose Logical, indicating whether or not to output
#'   processing/retrieval messages.
#' @param \dots Futher arguments passed on to \code{make_url}.
#'   All named arguments that do not match the above arguments
#'   (i.e., count, type, etc.) will be built into the request.
#'   To return only English language tweets, for example, use
#'   \code{lang = "en"}. For more options see Twitter's
#'   API documentation.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @details Twitter API document recommends limiting searches to
#'   10 keywords and operators. Complex queries may also produce
#'   API errors preventing recovery of information related to
#'   the query.
#'   It should also be noted Twitter's search API does not consist
#'   of an index of all Tweets. At the time of searching, the
#'   search API index includes between only 6-9 days of Tweets.
#'
#'
#'   Number of tweets returned will often be less than what was
#'   specified by the user. This can happen because (a) the search
#'   query did not return many results (the search pool is already
#'   thinned out from the population of tweets to begin with) or
#'   (b) because you hit your rate limit for a given token. Even if
#'   the query has lots of hits and the rate limit should be able to
#'   max out at 18,000, the returned number of tweets may be lower,
#'   but that's only because the functions filter out duplicates
#'   (e.g., 18,000 tweets were actually returned, but 30 of them were
#'   removed because they were repeats).
#' @examples
#' \dontrun{
#' # search for 1000 tweets mentioning Hillary Clinton
#' hrc <- search_tweets(q = "hillaryclinton", n = 1000)
#'
#' # data frame where each observation (row) is a different tweet
#' hrc
#'
#' # users data also retrieved. can access it via users_data()
#' users_data(hrc)
#'
#' # search for 1000 tweets in English
#' djt <- search_tweets(q = "realdonaldtrump", n = 1000, lang = "en")
#' djt # prints tweets data preview
#' users_data(djt) # prints users data preview
#' }
#' @return List object with tweets and users each returned as a
#'   data frame.
#' @family tweets
#' @export
search_tweets <- function(q, n = 100, type = "mixed", max_id = NULL,
	                        include_rts = TRUE, parse = TRUE,
	                        clean_tweets = FALSE,
                          as_double = FALSE, token = NULL,
                          verbose = TRUE, ...) {

  query <- "search/tweets"
  stopifnot(is_n(n), is.atomic(q), is.atomic(max_id))
  token <- check_token(token, query)
  #n.times <- rate_limit(token, query)[["remaining"]]
  n.times <- 180

  if (nchar(q) > 500) {
    stop("q cannot exceed 500 characters.", call. = FALSE)
  }

  if (length(type) > 1) {
    stop("can only select one search type. Try type = 'mixed'.",
      call. = FALSE)
  }

  if (!isTRUE(tolower(type) %in% c("mixed", "recent", "popular"))) {
    stop("invalid search type - must be mixed, recent, or popular.",
      call. = FALSE)
  }

  if (!include_rts) q <- paste0(q, " -RT")

  params <- list(q = q,
    result_type = type,
    count = 100,
    max_id = max_id,
    ...)

  url <- make_url(
    query = query,
    param = params)

  if (verbose) message("Searching for tweets...")

  tw <- scroller(url, n, n.times, type = "search", token)

  if (parse) {
    tw <- parser(tw, n, clean_tweets = clean_tweets,
      as_double = as_double)
    if (!is.null(tw)) {
      if (is.list(tw)) {
        tw <- attr_tweetusers(tw)
      }
    }
  }

  if (verbose) {
    message("Finished collecting tweets!")
  }

  tw
}


#' search_users
#'
#' @description Returns data frame of users data using a provided
#'   search query.
#'
#' @param q Character, search query of no greater than
#'   500 characters maximum.
#' @param n Numeric, specifying the total number of desired users to
#'   return. Defaults to 100. Maximum number of users returned from
#'   a single search is 1,000.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
#' @param clean_tweets logical indicating whether to remove non-ASCII
#'   characters in text of tweets. defaults to FALSE.
#' @param as_double logical indicating whether to handle ID variables
#'   as double (numeric) class. By default, this is set to FALSE, meaning
#'   ID variables are treated as character vectors. Setting this to
#'   TRUE can provide performance (speed and memory) boost but can also
#'   lead to issues when printing and saving, depending on the format.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param verbose Logical, indicating whether or not to output
#'   processing/retrieval messages.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # search for 1000 tweets mentioning Hillary Clinton
#' pc <- search_users(q = "political communication", n = 1000)
#'
#' # data frame where each observation (row) is a different user
#' pc
#'
#' # tweets data also retrieved. can access it via tweets_data()
#' users_data(hrc)
#' }
#' @return Data frame of users returned by query.
#' @family users
#' @export
search_users <- function(q, n = 20, parse = TRUE,
  clean_tweets = FALSE, as_double = FALSE,
  token = NULL, verbose = TRUE) {

	query <- "users/search"
	stopifnot(is_n(n), is.atomic(q))
	token <- check_token(token, query)
	n.times <- ceiling(n / 20)
  if (n.times > 50) n.times <- 50

	if (nchar(q) > 500) {
		stop("q cannot exceed 500 characters.", call. = FALSE)
	}

	params <- list(q = q,
		count = 20,
		page = 1)

	url <- make_url(
		query = query,
		param = params)

	if (verbose) message("Searching for users...")

	usr <- vector("list", n.times)
  k <- 0
  nrows <- NULL

	for (i in seq_len(n.times)) {
		r <- tryCatch(
			TWIT(get = TRUE, url, token),
			error = function(e) NULL)

		if (is.null(r)) break

		usr[[i]] <- from_js(r)

    if (identical(length(usr[[i]]), 0)) break
    if (isTRUE(is.numeric(nrow(usr[[i]])))) {
      nrows <- nrow(usr[[i]])
    } else {
      if (identical(nrows, 0)) break
      nrows <- 0
    }

    k <- k + nrows

		if (k >= n) break

		url$query$page <- (i + 1L)
	}

  if (parse) {
    usr <- parser(usr, n, clean_tweets = clean_tweets,
      as_double = as_double)
    if (!is.null(usr)) {
      if (is.list(usr)) {
        usr <- usr[c("users", "tweets")]
        usr <- attr_tweetusers(usr)
      }
    }
  }

	if (verbose) {
		message("Finished collecting users!")
	}

	usr
}

count_users_returned <- function(x) {
	length(unique(unlist(lapply(x, function(x) x[["id_str"]]),
		use.names = FALSE)))
}



#' Get value for max_id
#'
#' @param df Tweets data frame with "created_at" and "status_id" variables.
#'
#' @return Character string of max_id to be used in future function calls.
#' @export
#'
next_id <- function(df) {
	if (!all(c("created_at", "status_id") %in% names(df))) {
		stop("wrong data frame - function requires tweets data")
	}
	if (any(grepl("posix", class(df$created_at), ignore.case = TRUE))) {
		df$created_at <- format_date(df$created_at)
	}
	df <- df[!is.na(df$status_id), ]
	df <- df[order(df$created_at), ]
	df$status_id[1]
}

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
#' @param max_id Character, specifying the [oldest] status id beyond
#'   which results should resume returning.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
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
#'   \code{lang = "en"}. Or, to exclude retweets, use
#'   \code{include_rts = FALSE}. For more options see Twitter's
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
  parse = TRUE, token = NULL, verbose = TRUE, ...) {

  query <- "search/tweets"

  stopifnot(is_n(n), is.atomic(q), is.atomic(max_id))

  token <- check_token(token, query)

  n.times <- rate_limit(token, query)[["remaining"]]

  if (nchar(q) > 500) {
    stop("q cannot exceed 500 characters.", call. = FALSE)
  }

  if (length(type) > 1) {
    stop("can only select one search type. Try type = 'mixed'.",
      call. = FALSE)
  }

  if (!tolower(type) %in% c("mixed", "recent", "popular")) {
    stop("invalid search type - must be mixed, recent, or popular.",
      call. = FALSE)
  }

  params <- list(q = q,
    result_type = type,
    count = 100,
    max_id = max_id,
    ...)

  url <- make_url(
    query = query,
    param = params)

  if (verbose) message("Searching for tweets...")

  tw <- scroller(url, n, n.times, token)

  if (parse) {
    tw <- parser(tw, n)
    tw[["meta_search"]] <- list(query = q, functions = "search_tweets()")
    tw <- attr_tweetusers(tw)
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
search_users <- function(q, n = 20, parse = TRUE, token = NULL,
	verbose = TRUE) {

	query <- "users/search"

	stopifnot(is_n(n), is.atomic(q))

	token <- check_token(token, query)

	n.times <- rate_limit(token, query)[["remaining"]]
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

	usr <- list()

	for (i in seq_len(n.times)) {
		r <- tryCatch(
			TWIT(get = TRUE, url, token),
			error = function(e) NULL)

		if (is.null(r)) break

		usr[[i]] <- from_js(r)

		if (count_users_returned(usr) >= n) break

		url$query$page <- (i + 1)
	}

	if (parse) {
		usr <- parser(usr, n)
		usr <- usr[c("users", "tweets")]
		usr[["meta_search"]] <- list(query = q, functions = "search_users()")
    usr <- attr_tweetusers(usr)
	}

	if (verbose) {
		message("Finished collecting users!")
	}

	usr
}

count_users_returned <- function(x) length(unique(unlist(lapply(x, function(x) x[["id_str"]]))))

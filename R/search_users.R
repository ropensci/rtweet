#' Get users data on accounts identified via search query.
#'
#' Returns data for up to 1,000 users matched by user provided search
#' query.
#'
#' @param q Query to be searched, used in filtering relevant tweets to
#'   return from Twitter's REST API. Should be a character string not
#'   to exceed 500 characters maximum. Spaces are assumed to function
#'   like boolean "AND" operators. To search for tweets including one
#'   of multiple possible terms, separate search terms with spaces and
#'   the word "OR". For example, the search \code{query =
#'   "data science"} searches for tweets using both "data" and
#'   "science" though the words can appear anywhere and in any order
#'   in the tweet. However, when OR is added between search terms,
#'   \code{query = "data OR science"}, Twitter's REST API should
#'   return any tweet that includes either "data" or "science"
#'   appearing in the tweets. At this time, Twitter's users/search API
#'   does not allow complex searches or queries targeting exact
#'   phrases as is allowed by \code{search_tweets}.
#' @param n Numeric, specifying the total number of desired users to
#'   return. Defaults to 100. Maximum number of users returned from a
#'   single search is 1,000.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list object. By default,
#'   \code{parse = TRUE} saves users from the time [and frustrations]
#'   associated with disentangling the Twitter API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param verbose Logical, indicating whether or not to output
#'   processing/retrieval messages.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#'
#' \dontrun{
#'
#' ## search for 1000 tweets mentioning Hillary Clinton
#' pc <- search_users(q = "political communication", n = 1000)
#'
#' ## data frame where each observation (row) is a different user
#' pc
#'
#' ## tweets data also retrieved. can access it via tweets_data()
#' users_data(hrc)
#'
#' }
#'
#' @return Data frame of users returned by query.
#' @family users
#' @export
search_users <- function(q, n = 100,
                         parse = TRUE,
                         token = NULL,
                         verbose = TRUE) {
  args <- list(
    q = q,
    n = n,
    parse = parse,
    token = token,
    verbose = verbose
  )
  do.call("search_users_call", args)
}


search_users_call <- function(q, n = 20,
                              parse = TRUE,
                              token = NULL,
                              verbose = TRUE) {
  query <- "users/search"
  stopifnot(is_n(n), is.atomic(q))
  token <- check_token(token, query)
  if (n > 1000) {
    warning(
      paste0("search only returns up to 1,000 users per ",
        "unique search. Setting n to 1000..."))
    n <- 1000
  }
  n.times <- ceiling(n / 20)
  if (n.times > 50) n.times <- 50
  if (n < 20) {
    count <- n
  } else {
    count <- 20
  }

  if (nchar(q) > 500) {
    stop("q cannot exceed 500 characters.", call. = FALSE)
  }
  if (verbose) message("Searching for users...")

  usr <- vector("list", n.times)
  k <- 0
  nrows <- NULL

  for (i in seq_len(n.times)) {
    params <- list(
      q = q,
      count = count,
      page = i,
      tweet_mode = "extended"
    )
    url <- make_url(
      query = query,
      param = params
    )
    r <- tryCatch(
      TWIT(get = TRUE, url, token),
      error = function(e) return(NULL))

    if (is.null(r)) break

    usr[[i]] <- from_js(r)

    if (i > 1L) {
      if (identical(usr[[i]], usr[[i - 1L]])) {
        usr <- usr[-i]
        break
      }
    }

    if (identical(length(usr[[i]]), 0)) break
    if (isTRUE(is.numeric(NROW(usr[[i]])))) {
      nrows <- NROW(usr[[i]])
    } else {
      if (identical(nrows, 0)) break
      nrows <- 0
    }
    k <- k + nrows
    if (k >= n * 20) break
  }
  if (parse) {
    usr2 <- users_with_tweets(usr)
    if (nrow(usr2) > 0L) {
      uq <- !duplicated(usr2$user_id)
      usr <- usr2[uq, ]
      attr(usr, "tweets") <- tweets_data(usr2)[uq, ]
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

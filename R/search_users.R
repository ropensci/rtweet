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
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @param verbose Logical, indicating whether or not to output
#'   processing/retrieval messages.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#'
#' \dontrun{
#'
#' ## search for up to 1000 users using the keyword rstats
#' rstats <- search_users(q = "rstats", n = 1000)
#'
#' ## data frame where each observation (row) is a different user
#' rstats
#'
#' ## tweets data also retrieved. can access it via tweets_data()
#' tweets_data(rstats)
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
  token <- check_token(token)
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
    usr <- tweets_with_users(usr)
  }
  if (verbose) {
    message("Finished collecting users!")
  }
  usr
}

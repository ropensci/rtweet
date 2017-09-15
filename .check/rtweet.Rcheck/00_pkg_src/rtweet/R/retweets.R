#' GET statuses/retweets/:id
#'
#' @param id required The numerical ID of the desired status.
#' @param count optional Specifies the number of records to retrieve.
#'   Must be less than or equal to 100.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return data
statuses_retweets <- function(id, count = 100, token = NULL) {
  query <- "statuses/retweets"
  params <- list(id = id,
                 count = count)
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  from_js(r)
}


#' GET statuses/retweeters/ids
#'
#' Returns a collection of up to 100 user IDs belonging to users who
#'   have retweeted the Tweet specified by the id parameter.
#' 
#' @param id required The numerical ID of the desired status.
#' @param cursor semi-optional Causes the list of IDs to be broken into pages
#'   of no more than 100 IDs at a time. The number of IDs returned is not
#'   guaranteed to be 100 as suspended users are filtered out after
#'   connections are queried. If no cursor is provided, a
#'   value of -1 will be assumed, which is the first "page."
#'   The response from the API will include a previous_cursor and next_cursor
#'   to allow paging back and forth. See our cursor
#'   docs for more information.
#'   While this method supports the cursor parameter, the entire result set
#'   can be returned in a single cursored collection. Using the count
#'   parameter with this method will not provide segmented cursors for
#'   use with this parameter.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return data
#' @export
statuses_retweeters <- function(id, cursor = "-1", token = NULL) {
  query <- "statuses/retweets"
  params <- list(id = id,
                 cursor = cursor,
                 stringify_ids = TRUE)
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  from_js(r)
}


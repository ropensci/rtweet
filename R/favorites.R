#' Get tweets data for statuses favorited by one or more target users.
#'
#' Returns up to 3,000 statuses favorited by each of one or more
#' specific Twitter users.
#'
#' @param user Vector of user names, user IDs, or a mixture of both.
#' @param n Specifies the number of records to retrieve. Defaults to 200,
#'   which is the maximum number of records that can be retrieved in a single
#'   request. Higher numbers will require multiple requests.
#'   
#'   `n` is applied before removing any tweets that have been suspended or 
#'    deleted. 
#' @param since_id,max_id Limit tweets to ids in `(since_id, max_id]`.
#'   If `since_id` is smaller than the earliest tweet available to the API,
#'   it will be forced to the oldest tweet available.
#' @inheritParams lookup_users
#' @return A tibble with one row for each tweet.
#' @examples
#' \dontrun{
#'
#' ## get max number of statuses favorited by KFC
#' kfc <- get_favorites("KFC", n = 3000)
#' kfc
#'
#' ## get 400 statuses favorited by each of three users
#' favs <- get_favorites(c("Lesdoggg", "pattonoswalt", "meganamram"))
#' favs
#'
#' }
#' @family tweets
#' @seealso
#' <https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-favorites-list>
#' @export
get_favorites <- function(user,
                          n = 200,
                          since_id = NULL,
                          max_id = NULL,
                          parse = TRUE,
                          token = NULL) {
  token <- check_token(token)
  stopifnot(is.atomic(user), is.numeric(n))

  rt <- lapply(user, get_favorites_user, 
    n = n,
    since_id = since_id,
    max_id = max_id,
    parse = parse,
    token = token
  )

  if (parse) {
    rt <- do_call_rbind(rt)
  }
  rt
}

get_favorites_user <- function(user,
                           n = 200,
                           since_id = NULL,
                           max_id = NULL,
                           parse = TRUE,
                           token = NULL) {

  params <- list(
    tweet_mode = "extended",
    include_ext_alt_text = "true",
    max_id = max_id,
    since_id = since_id
  )
  params[[.id_type(user)]] <- user
  
  results <- TWIT_paginate_max_id(token, "/1.1/favorites/list", params,
    get_max_id = function(x) x$id_str,
    page_size = 200,
    n = n,
    parse = parse
  )

  if (parse) {
    results <- tweets_with_users(results)
    results$favorited_by <- user
  }
  
  results
}

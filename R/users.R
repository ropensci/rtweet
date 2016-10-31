#' lookup_users
#'
#' @description Returns Twitter user data_frame object for
#'   specified user_ids or screen_names.
#'
#' @param users User id or screen name of target user.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable @describeIn tokens.
#' @param parse Logical, indicating whether or not to parse
#'   return object into data frame(s).
#' @param clean_tweets logical indicating whether to remove non-ASCII
#'   characters in text of tweets. defaults to TRUE.
#' @param as_double logical indicating whether to handle ID variables
#'   as double (numeric) class. By default, this is set to FALSE, meaning
#'   ID variables are treated as character vectors. Setting this to
#'   TRUE can provide performance (speed and memory) boost but can also
#'   lead to issues when printing and saving, depending on the format.
#'
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # lookup vector of 1 or more user_id or screen_name
#' users <- c("potus", "hillaryclinton", "realdonaldtrump",
#'   "fivethirtyeight", "cnn", "espn", "twitter")
#'
#' usr_df <- lookup_users(users)
#' usr_df
#'
#' # view tweet data for these users via tweets_data()
#' tweets_data(usr_df)
#' }
#'
#' @return json response object (max is 18000 per token)
#' @family users
#' @export
lookup_users <- function(users, token = NULL, parse = TRUE,
  clean_tweets = TRUE, as_double = FALSE) {

  if (is.list(users)) {
    users <- unlist(users, use.names = FALSE)
  }
  if (length(users) < 101) {
    usr <- .user_lookup(users, token)
  } else if (length(users) > 18000) {
    users <- users[1:18000]
    n.times <- ceiling(length(users) / 100)
    from <- 1
    usr <- vector("list", n.times)
    for (i in seq_len(n.times)) {
      to <- from + 99
      if (to > length(users)) {
        to <- length(users)
      }
      usr[[i]] <- .user_lookup(users[from:to], token)
      from <- to + 1
      if (from > length(users)) break
    }
  }
  if (parse) {
    usr <- parser(usr, clean_tweets = clean_tweets, as_double = as_double)
    usr <- attr_tweetusers(usr[c("users", "tweets")])
  }
  usr
}

.user_lookup <- function(users, token = NULL) {

  query <- "users/lookup"
  if (is.list(users)) {
    users <- unlist(users)
  }
  stopifnot(is.atomic(users))
  if (length(users) > 100) {
    users <- users[1:100]
  }
  params <- list(id_type = paste(users, collapse = ","))
  names(params)[1] <- .ids_type(users)
  url <- make_url(
    query = query,
    param = params)
  token <- check_token(token, query = "users/lookup")
  resp <- TWIT(get = TRUE, url, token)
  from_js(resp)
}

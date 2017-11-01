#' Get Twitter users data for given users (user IDs or screen names).
#'
#' Returns data on up to 90,000 Twitter users. To return data on more
#' than 90,000 users, code must be written to iterate through user IDs
#' whilst avoiding rate limits, which reset every 15 minutes.
#'
#' @param users User id or screen name of target user.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param parse Logical, indicating whether or not to parse
#'   return object into data frame(s).
#'
#' @seealso \url{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-lookup}
#' @examples
#'
#' \dontrun{
#'
#' ## select one or more twitter users to lookup
#' users <- c(
#'   "potus", "hillaryclinton", "realdonaldtrump",
#'   "fivethirtyeight", "cnn", "espn", "twitter"
#' )
#'
#' ## get users data
#' usr_df <- lookup_users(users)
#'
#' ## view users data
#' usr_df
#'
#' ## view tweet data for these users via tweets_data()
#' tweets_data(usr_df)
#'
#' }
#'
#' @return A tibble of users data.
#' @family users
#' @export
lookup_users <- function(users, parse = TRUE, token = NULL) {
  args <- list(users = users, parse = parse, token = token)
  do.call("lookup_users_", args)
}

lookup_users_ <- function(users,
                          token = NULL,
                          parse = TRUE) {
  if (is.list(users)) {
    users <- unlist(users, use.names = FALSE)
  }
  stopifnot(is.atomic(users))
  users <- unique(as.character(users))
  users <- users[!is.na(users)]
  if (length(users) < 101L) {
    usr <- .user_lookup(users, token)
  } else {
    if (length(users) > 90000L) {
      message("max number of users exceeded; looking up first 90,000")
      users <- users[1:90000]
    }
    n.times <- ceiling(length(users) / 100)
    from.id <- 1L
    usr <- vector("list", n.times)
    for (i in seq_len(n.times)) {
      to.id <- from.id + 99L
      if (to.id > length(users)) {
        to.id <- length(users)
      }
      usr[[i]] <- .user_lookup(users[from.id:to.id], token)
      from.id <- to.id + 1
      if (from.id > length(users)) break
    }
  }
  if (parse) {
    if (identical(c("code", "message"), names(usr[[1]]))) {
      message("Error code: ", usr[[1]]$code)
      message(usr[[1]]$message)
      usr <- tibble::as_tibble()
      attr(usr, "tweets") <- tibble::as_tibble()
    } else if (is.character(usr)) {
      message(usr)
      usr <- tibble::as_tibble()
      attr(usr, "tweets") <- tibble::as_tibble()
    } else {
      usr <- users_with_tweets(usr)
    }
  }
  usr
}

.user_lookup <- function(users, token = NULL) {
  query <- "users/lookup"
  get <- TRUE
  if (length(users) > 100) {
    users <- users[1:100]
  }
  if (length(users) > 80) get <- FALSE
  params <- list(id_type = paste0(users, collapse = ","))
  names(params)[1] <- .ids_type(users)
  url <- make_url(
    query = query,
    param = params
  )
  token <- check_token(token, query = query)
  resp <- TWIT(get = get, url, token)
  from_js(resp)
}

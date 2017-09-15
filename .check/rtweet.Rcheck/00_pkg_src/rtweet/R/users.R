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
#'
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' ## lookup vector of 1 or more user_id or screen_name
#' users <- c("potus", "hillaryclinton", "realdonaldtrump",
#'   "fivethirtyeight", "cnn", "espn", "twitter")
#'
#' ## get users data
#' usr_df <- lookup_users(users)
#'
#' ## view users data
#' usr_df
#'
#' ## view tweet data for these users via tweets_data()
#' tweets_data(usr_df)
#' }
#'
#' @return json response object
#' @family users
#' @export
lookup_users <- function(users, ...) {
  UseMethod("lookup_users")
}

lookup_users <- function(users,
                         token = NULL,
                         parse = TRUE) {

  if (is.list(users)) {
    users <- unlist(users, use.names = FALSE)
  }
  stopifnot(is.atomic(users))
  users <- as.character(users) %>% unique()
  users <- users[!is.na(users)]
  if (length(users) < 101) {
    usr <- .user_lookup(users, token)
  } else {
    if (length(users) > 90000) {
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
    param = params)
  token <- check_token(token, query = query)
  resp <- TWIT(get = get, url, token)
  from_js(resp)
}

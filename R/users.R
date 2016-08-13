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
#'   return object into data frame(s)
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # search for 1000 tweets mentioning Hillary Clinton
#' hrc <- search_tweets(q = "hillaryclinton", count = 1000)
#'
#' # lookup returned user_id values
#' users <- lookup_users(hrc$user_id)
#' users
#'
#' # merge data objects
#' dat <- dplyr::left_join(hrc, users, by = "user_id")
#' dat
#' }
#'
#' @return json response object (max is 18000 per token)
#' @importFrom dplyr bind_rows data_frame
#' @export
lookup_users <- function(users, token = NULL, parse = TRUE) {

  if (is.list(users)) {
    users <- unlist(users)
  }

  if (length(users) > 18000) {
    users <- users[1:18000]
  }

  n.times <- ceiling(length(users) / 100)

  from <- 1

  usr <- vector("list", n.times)

  for (i in seq_len(n.times)) {
    to <- from + 99

    if (to > length(users)) {
      to <- length(users)
    }

    usr[[i]] <- .user_lookup(
      users[from:to],
      token, parse = parse)

    from <- to + 1

    if (from > length(users)) break
  }

  if (parse) usr <- parser(usr)

  usr
}

.user_lookup <- function(users, token = NULL, parse) {

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

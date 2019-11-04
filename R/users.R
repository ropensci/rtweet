#' Get Twitter users data for given users (user IDs or screen names).
#'
#' Returns data on up to 90,000 Twitter users. To return data on more
#' than 90,000 users, code must be written to iterate through user IDs
#' whilst avoiding rate limits, which reset every 15 minutes.
#'
#' @param users User id or screen name of target user.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
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
  stopifnot(is.atomic(users))
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
      if (parse) {
        usr[[i]] <- check_for_errors(usr[[i]])
      }
      from.id <- to.id + 1L
      if (from.id > length(users)) break
    }
  }
  if (parse) {
    usr <- users_with_tweets(usr)
  }
  usr
}


check_for_errors <- function(x) {
  if (identical(c("code", "message"), names(x))) {
    message("Error code: ", x$code)
    message(x$message)
    return(tibble::as_tibble())
  } else if (is.character(x)) {
    message(x)
    return(tibble::as_tibble())
  }
  x
}


has_read_access <- function(x) {
  al <- get_access_level(x)
  identical(al, "read")
}


get_access_level <- function(token) {
  if ("access_level" %in% names(attributes(token))) {
    return(attr(token, "access_level"))
  }
  r <- httr::GET(
    "https://api.twitter.com/1.1/account/verify_credentials.json",
    token)
  headers <- r$all_headers[[1]]$headers
  if (has_name_(headers, "x-access-level")) {
    access_level <- headers$`x-access-level`
  } else {
    access_level <- ""
  }
  attr(token, "access_level") <- access_level
  access_level
}


.user_lookup <- function(users, token = NULL) {
  ## gotta have ut8-encoding for the comma separated IDs
  ## set scipen to ensure IDs are not rounded
  op_enc <- getOption("encoding")
  op_sci <- getOption("scipen")
  on.exit(options(scipen = op_sci, encoding = op_enc), add = TRUE)
  options(scipen = 14, encoding = "UTF-8")

  query <- "users/lookup"
  get <- TRUE
  if (length(users) > 100) {
    users <- users[1:100]
  }
  token <- check_token(token)
  if (length(users) > 80 && has_write_access(token)) {
    get <- FALSE
  }
  params <- list(id_type = paste0(users, collapse = ","))
  names(params)[1] <- .ids_type(users)
  url <- make_url(
    query = query,
    param = params
  )
  resp <- TWIT(get = get, url, token)
  from_js(resp)
}

has_write_access <- function(x) {
  al <- get_access_level(x)
  length(al) == 1 && grepl("write", al)
}

#' rtweet
#'
#' @description base function.
#' @param url list output object from make_url
#' @param n numeric, number of desired tweets to return
#' @param parse logical, indicating whether to return parsed or
#'   unparsed (json) object
#' @param \dots named arguments passed on to httr request
#' @export
rtweet <- function(url, n, parse = TRUE, ...) {
  x <- scroll(
    url = url,
    n = n, ...)

  if (parse) x <- parser(x)

  x
}

#' parse_tweets
#'
#'
#'
parse_tweets <- function(x) {

  if ("statuses" %in% names(x)) {
    x <- x[["statuses"]]
  }

  if (!"friends_count" %in% names(x)) {
    return(tweets_df(x))
  } 

  return(invisible()
}

#' parse_users
#'
#'
#'
parse_users <- function(x) {

  if ("friends_count" %in% names(x)) {
    return(user_df(x))
  }

  if ("statuses" %in% names(x)) {
    x <- x[["statuses"]]
  }

  if ("user" %in% names(x)) {
    return(user_df(x[["user"]]))
  }
  
  return(invisible()
}

#' parser
#'
#'
#'
#' @importFrom dplyr bind_rows
#' @export
parser <- function(x) {

  tweets <- bind_rows(lapply(x, parse_tweets))

  users <- bind_rows(lapply(x, parse_users))

  list(
    tweets = tweets[!duplicated(tweets), ], 
    users = users[!duplicated(users), ])
}

#' scroll
#'
#'
#' @export
scroll <- function(url, n, ...) {

  stopifnot(is.double(n), is.list(url))

  x <- list()

  count <- n

  while (count > 0) {

    r <- tryCatch(TWIT(get = TRUE, url = url, ...),
      error = function(e) NULL)

    if (is.null(r)) break

    r <- from_js(r)

    if (break_check(r, url)) break

    x[[length(x) + 1]] <- r

    count <- count - unique_id_count(r)

    url$query$max_id <- get_max_id(r)
  }

  x
}



unique_id_count <- function(x) {

  if ("statuses" %in% tolower(names(x))) {
    x <- x[["statuses"]]
  }
  
  length(unique(x$id_str[!is.na(nanull(x$id_str))]))
}

#' get_max_id
#'
#'
#'
get_max_id <- function(x) {

  if ("statuses" %in% tolower(names(x))) {
    x <- x[["statuses"]]
  }
  
  tail(x$id_str[!is.na(nanull(x$id_str))], 1)
}

#' nanull
#'
#'
#'
nanull <- function(x) {
  if (is.null(x)) return(NA)
  if (identical(x, "")) return(NA)
  if (length(x) == 0) return(NA)
  x[x == ""] <- NA
  x[is.null(x)] <- NA
  x
}

#' break_check
#'
#'
#'
#' @importFrom dplyr bind_rows
break_check <- function(r, url) {
  if (is.null(r)) return(TRUE)

  x <- get_max_id(r)

  if (is.null(x)) return(TRUE)
  if (any(x == 0, x == "0")) return(TRUE)
  if ("max_id" %in% names(url$query)) {
    if (x == url$query$max_id) return(TRUE)
  }

  FALSE
}

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
  } else {
    return(invisible())
  }
}

#' parse_users
#'
#'
#'
parse_users <- function(x) {
  if ("statuses" %in% names(x)) {
    x <- x[["statuses"]]
  }
  if ("user" %in% names(x)) {
    return(user_df(x[["user"]]))
  } else {
    return(invisible())
  }
}

#' parser
#'
#'
#'
#' @importFrom dplyr bind_rows
#' @export
parser <- function(x) {
  tweets <- bind_rows(lapply(x, parse_tweets))
  tweets <- tweets[!duplicated(tweets), ]

  users <- bind_rows(lapply(x, parse_users))
  users <- users[!duplicated(users), ]

  list(tweets, users)
}

#' scroll
#'
#'
#' @export
scroll <- function(url, n, ...) {

  n <- ceiling(n / as.numeric(url$query$count))

  x <- list()

  for (i in seq_len(n)) {
    r <- tryCatch(TWIT(get = TRUE, url = url, ...),
      error = function(e) NULL)

    if (is.null(r)) break

    r <- from_js(r)

    if (max_breaker(r, url)) break

    url$query$max_id <- get_max_id(r)

    x[[i]] <- r
  }
  x
}





#' get_max_id
#'
#'
#'
get_max_id <- function(x) {
  if ("statuses" %in% tolower(names(x))) {
    x <- x[["statuses"]]
  }
  max_id <- tail(x$id_str[!is.na(nanull(x$id_str))], 1)

  max_id
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

#' max_breaker
#'
#'
#'
#' @importFrom dplyr bind_rows
max_breaker <- function(r, url) {
  if (is.null(r)) return(TRUE)

  x <- get_max_id(r)

  if (is.null(x)) return(TRUE)
  if (any(x == 0, x == "0")) return(TRUE)
  if ("max_id" %in% names(url$query)) {
    if (x == url$query$max_id) return(TRUE)
  }

  FALSE
}

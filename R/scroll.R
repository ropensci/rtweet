#' parser
#'
#' @description Parses tweets and users objects. Returns data frames
#'   for each.
#'
#' @param x List, fromJSON nested list object
#' @param n Numeric, number of desired tweets to return
#'
#' @importFrom dplyr bind_rows
#' @export
parser <- function(x, n = NULL) {
  tweets <- bind_rows(lapply(x, parse_tweets))
  tweets <- tweets[!duplicated(tweets), ]

  if (!is.null(n)) {
    if (is.data.frame(tweets)) {
      if (nrow(tweets) > n) {
        tweets <- tweets[seq_len(n), ]
      }
    }
  }

  users <- bind_rows(lapply(x, parse_users))
  users <- users[!duplicated(users), ]

  if (!is.null(n)) {
    if (is.data.frame(users)) {
      if (nrow(users) > n) {
        users <- users[seq_len(n), ]
      }
    }
  }

  list(
    tweets = tweets,
    users = users)
}

#' @importFrom httr stop_for_status
scroller <- function(url, n, catch_error = FALSE, ...) {

  stopifnot(is.numeric(n), is.list(url))

  x <- list()

  i <- 1

  count <- n

  while (count > 0) {

    r <- tryCatch(
      TWIT(get = TRUE, url, ...),
      error = function(e) NULL)

    if (catch_error) {
      stop_for_status(r)
    }

    if (is.null(r)) break

    r <- from_js(r)

    if (break_check(r, url)) break

    x[[i]] <- r

    i <- i + 1

    count <- n - unique_id_count(x)

    url$query$max_id <- get_max_id(r)
  }

  x
}


unique_id <- function(x) {
  if ("statuses" %in% tolower(names(x))) {
    x <- x[["statuses"]]
  }
  x$id_str
}

unique_id_count <- function(x) {
  x <- unlist(lapply(x, unique_id))
  length(unique(x))
}


#' @importFrom utils tail
get_max_id <- function(x) {

  if ("statuses" %in% tolower(names(x))) {
    x <- x[["statuses"]]
  }

  tail(x$id_str[!is.na(nanull(x$id_str))], 1)
}


nanull <- function(x) {
  if (is.null(x)) return(NA)
  if (identical(x, "")) return(NA)
  if (length(x) == 0) return(NA)
  x[x == ""] <- NA
  x[is.null(x)] <- NA
  x
}

#' @importFrom dplyr bind_rows
break_check <- function(r, url) {

  if (is.null(r)) return(TRUE)

  x <- get_max_id(r)

  if (is.null(x)) return(TRUE)
  if (any(x == 0, x == "0")) return(TRUE)

  if ("max_id" %in% names(url$query)) {
    if (is.null(url$query$max_id)) return(FALSE)
    if (x == url$query$max_id) return(TRUE)
  }

  FALSE
}

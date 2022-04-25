#' Extract cursor (for cursor based pagination)
#'
#' This internal helper extracts the cursor from the object passed to the
#' `cursor` argument of the functions that use [TWIT_paginate_cursor()].
#'
#' @keywords internal
#' @examples
#' if (auth_has_default()) {
#' page1 <- get_followers("_R_Foundation")
#' page2 <- get_followers("_R_Foundation", cursor = page1)
#' }
#' @keywords internal
#' @export
next_cursor <- function(x) {
  if (is_string(x) || is_null(x)) {
    x
  } else if (is.data.frame(x)) {
    cursor <- attr(x, "rtweet_cursor")
    if (is.null(cursor)) {
      abort("`cursor` must have a `rtweet_cursor` attribute")
    }
    cursor
  } else {
    abort("`cursor` must be a string or data frame")
  }
}

copy_cursor <- function(to, from) {
  attr(to, "rtweet_cursor") <- attr(from, "rtweet_cursor")
  to
}

#' Previous cursor
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' Reverse pagination is no longer supported.
#'
#' @keywords internal
#' @export
previous_cursor <- function(x) {
  lifecycle::deprecate_stop("1.0.0", "previous_cursor()")
}

#' Extract min/max id (for id based pagination)
#' 
#' These internal helpers extract the ids passed on to the `max_id`
#' and `since_id` arguments to functions that use [TWIT_paginate_max_id()].
#' 
#' @keywords internal
#' @param x Either a data frame of tweets or a character vector of status ids.
#' @export
#' @examples 
#' if (auth_has_default()) {
#' tw <- search_tweets("#rstats")
#' 
#' # retrieve older tweets
#' older <- search_tweets("#rstats", max_id = tw)
#' even_older <- search_tweets("#rstats", max_id = older)
#' 
#' # retrieve newer tweets
#' newer <- search_tweets("#rstats", since_id = tw)
#' }
max_id <- function(x) {
  id <- find_id(x, "max_id")
  as.character(min(bit64::as.integer64(id)) - 1L)
}

#' @rdname next_cursor
#' @export
since_id <- function(x) {
  id <- find_id(x, "since_id")
  as.character(max(bit64::as.integer64(id)))
}

find_id <- function(x, arg_name) {
  if (is.character(x)) {
    x
  } else if (is.data.frame(x)) {
    if (!has_name(x, "id"))  {
      abort(paste0("`", arg_name, "` must contain a `id` column"))
    }
    y <- x$id
    if (is.factor(y)) {
      y <- as.numeric(levels(y))[y]
    }
    y
  } else {
    abort(paste0("`", arg_name, "` must be a character vector or data frame"))
  }
}

#' next_cursor
#'
#' Method for returning next value (used to request next page or results)
#' object returned from Twitter APIs.
#'
#' @param x Data object returned by Twitter API.
#'
#' @examples
#' \dontrun{
#'
#' ## Retrieve user ids of accounts following POTUS
#' f1 <- get_followers("potus", n = 75000)
#'
#' ## store next_cursor in page
#' page <- next_cursor(f1)
#'
#' ## max. number of ids returned by one token is 75,000 every 15
#' ##   minutes, so you'll need to wait a bit before collecting the
#' ##   next batch of ids
#' sys.Sleep(15 * 60) ## Suspend execution of R expressions for 15 mins
#'
#' ## Use the page value returned from `next_cursor` to continue
#' ##   where you left off.
#' f2 <- get_followers("potus", n = 75000, cursor = page)
#'
#' ## combine
#' f <- do.call("rbind", list(f1, f2))
#'
#' ## count rows
#' nrow(f)
#'
#' }
#'
#' @return Character string of next cursor value used to retrieved
#'   the next page of results. This should be used to resume data
#'   collection efforts that were interrupted by API rate limits.
#'   Modify previous data request function by entering the returned
#'   value from `next_cursor` for the `page` argument.
#' @keywords internal
#' @family ids
#' @family extractors
#' @rdname next_cursor
#' @export
next_cursor <- function(x) UseMethod("next_cursor")

#' @export
next_cursor.default <- function(x) last(x)

#' @export
next_cursor.numeric <- function(x) {
  sp <- getOption("scipen")
  on.exit(options(sp), add = TRUE)
  options(scipen = 14)
  x <- as.character(x)
  NextMethod()
}

#' @export
next_cursor.character <- function(x) {
  last(x)
}

#' @export
next_cursor.data.frame <- function(x) {
  if (has_name_(x, "next_cursor_str")) return(x[["next_cursor_str"]])
  if (has_name_(x, "next_cursor")) return(x[["next_cursor"]])
  if (has_name_(attributes(x), "next_cursor")) return(attr(x, "next_cursor"))
  x <- x[[grep("id$", names(x))[1]]]
  NextMethod()
}

#' @export
next_cursor.list <- function(x) {
  if (has_name_(x, "next_cursor_str")) return(x[["next_cursor_str"]])
  if (has_name_(x, "next_cursor")) return(x[["next_cursor"]])
  if (has_name_(attributes(x), "next_cursor")) return(attr(x, "next_cursor"))
  if (!is.null(names(x))) {
    x <- list(x)
  }
  x <- lapply(x, function(x) x[[grep("id$", names(x))[1]]])
  x <- unlist(lapply(x, next_cursor))
  last(x)
}

#' @export
next_cursor.response <- function(x) {
  x <- from_js(x)
  NextMethod()
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

#' Extract minimum/maximum id from a data frame of tweets
#' 
#' These internal helpers extract the ids passed on to the `max_id`
#' and `since_id` arguments to functions that use [TWIT_paginate_max_id()].
#' 
#' @keywords internal
#' @param x Either a data frame of tweets or a character vector of status ids.
#' @export
#' @examples 
#' \dontrun{
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
    if (!has_name(x, "status_id"))  {
      abort(paste0("`", arg_name, "` must contain a `status_id` column"))
    }
    x$status_id
  } else {
    abort(paste0("`", arg_name, "` must be a character vector or data frame"))
  }
}

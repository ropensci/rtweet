#' next_cursor/max_id
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
#' ## Use the page value returned from \code{next_cursor} to continue
#' ##   where you left off.
#' f2 <- get_followers("potus", n = 75000, page = page)
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
#'   value from \code{next_cursor} for the \code{page} argument.
#' @family ids
#' @family extractors
#' @rdname next_cursor
#' @export
next_cursor <- function(x) UseMethod("next_cursor")


#next_cursor_ <- function(ids) {
#  x <- attr(ids, "next_cursor")
#  if (is.numeric(x)) {
#    op <- options()
#    on.exit(options(op))
#    options(scipen = 14)
#    x <- as.character(x)
#  }
#  x
#}



#' @export
next_cursor.default <- function(x) return_last(x)

#' @export
next_cursor.numeric <- function(x) {
  op <- options()
  on.exit(options(op))
  options(scipen = 14)
  x <- as.character(x)
  NextMethod()
}

#' @export
next_cursor.character <- function(x) {
  return_last(x)
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
  return_last(x)
}

#' @export
next_cursor.response <- function(x) {
  x <- from_js(x)
  NextMethod()
}


get_max_id <- function(x, adj = -1L) {
  if (!is.atomic(x)) {
    if (has_name_(x, "statuses")) {
      x <- x[["statuses"]]
    }
    if (has_name_(x, "id")) {
      x <- x[["id"]]
    } else if (has_name_(x, "ids")) {
      return(x[["next_cursor_str"]])
    } else if (is.null(names(x))) {
      if (has_name_(x, "ids")) {
        return(x[[1]][["next_cursor_str"]])
      }
    } else if (has_name_(x, "status_id")) {
      x <- x[["status_id"]]
    } else if (has_name_(x, "user_id")) {
      x <- x[["user_id"]]
    }
  }
  ##return_last(x) + adj
  bit64::as.integer64(return_last(x)) + adj
}


#' @rdname next_cursor
#' @export
max_id <- function(x) UseMethod("max_id")

#' @export
max_id.default <- function(x) return_last(x)

#' @export
max_id.character <- function(x) {
  x <- sort(bit64::as.integer64(x))
  as.character(x[1])
}

#' @export
max_id.data.frame <- function(x) {
  if (has_name_(attributes(x), "max_id_str")) return(attr(x, "max_id_str"))
  if (has_name_(attributes(x), "max_id")) return(attr(x, "max_id"))
  x <- x[[grep("id$", names(x))[1]]]
  NextMethod()
}

#' @export
max_id.list <- function(x) {
  if (has_name_(x, "max_id_str")) return(x[["max_id_str"]])
  if (has_name_(x, "max_id")) return(x[["max_id"]])
  if (!is.null(names(x))) {
    x <- list(x)
  }
  x <- lapply(x, function(x) x[[grep("id$", names(x))[1]]])
  x <- unlist(lapply(x, max_id))
  return_last(x)
}

#' @export
max_id.response <- function(x) {
  x <- from_js(x)
  NextMethod()
}

#' next_cursor
#'
#' Returns next cursor value from ids object. Return
#'   object used to retrieve next page of results from API request.
#'
#' @param ids Data frame of Twitter IDs generated via
#'   \code{\link{get_followers}} or \code{\link{get_friends}}.
#'
#' @examples
#' \dontrun{
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
#' }
#'
#' @return Character string of next cursor value used to retrieved
#'   the next page of results. This should be used to resume data
#'   collection efforts that were interrupted by API rate limits.
#'   Modify previous data request function by entering the returned
#'   value from \code{next_cursor} for the \code{page} argument.
#' @aliases next_page cursor_next
#' family ids
#' @export
next_cursor <- function(ids) {
  x <- attr(ids, "next_cursor")
  if (is.numeric(x)) {
    op <- options()
    on.exit(options(op))
    options(scipen = 14)
    x <- as.character(x)
  }
  x
}

#' Search tweets (vectorized)
#'
#' search_tweets2 Passes all args to search_tweets. Returns data from
#' one OR MORE search queries.
#'
#' @inheritParams search_tweets.
#' @return A tbl data frame with additional "query" column.
#' @rdname search_tweets
#' @examples
#'
#' \dontrun{
#'
#' ## search using multilple queries
#' st2 <- search_tweets2(
#'   c("\"data science\"", "rstats OR python"),
#'   n = 500
#' )
#'
#' ## preview tweets data
#' st2
#'
#' ## preview users data
#' users_data(st2)
#'
#' ## check breakdown of results by search query
#' table(st2$query)
#'
#' }
#'
#' @export
search_tweets2 <- function(...) {
  dots <- match_fun(list(...), "search_tweets")
  q <- dots[["q"]]
  dots[["q"]] <- NULL
  ## is parse = TRUE?
  parse <- dots[["parse"]]
  ## search for each string in column of queries
  rt <- Map("search_tweets", q, MoreArgs = dots)
  ## if parse is false, return rt
  if (!parse) {
    return(rt)
  }
  ## deal with queries that returned zero tweets
  kp <- lengths(rt) > 0L
  if (sum(kp, na.rm = TRUE) == 0L) return(data.frame())
  rt <- rt[kp]
  q <- q[kp]
  ## add query variable to data frames
  rt <- Map("add_var", rt, query = q)
  ## merge users data into one data frame
  do_call_rbind(rt)
}

add_var <- function(x, ...) {
  dots <- list(...)
  if (!is.null(names(dots))) {
    varname <- names(dots)
  } else {
    varname <- deparse(substitute(...))
  }
  x[[varname]] <- unlist(dots, use.names = FALSE)
  x
}

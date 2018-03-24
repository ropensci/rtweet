#' Joining users and tweets data
#'
#' @param x Data frame returned by rtweet API calls
#'
#' @return Joined users and tweets dta
#' @export
#'
join_rtweet <- function(x) {
  if ("users" %in% names(attributes(x))) {
    ## get users data and drop screen name
    u <- attr(x, "users")
    u <- u[, names(u) != "screen_name"]
    ## remove duplicate user rows
    u <- u[!duplicated(u$user_id), ]
    ## merge tweets and users data
    x <- merge(x, u, by = "user_id")
    ## remove duplicate tweet rows
    x <- x[!duplicated(x$status_id), ]
    x <- tibble::as_tibble(x, validate = FALSE)
  } else if ("tweets" %in% names(attributes(x))) {
    ## get tweets data and drop screen_name
    w <- attr(x, "tweets")
    w <- w[, names(w) != "screen_name"]
    ## remove duplicate user rows
    x <- x[!duplicated(x$user_id), ]
    ## merge tweets and users data
    x <- merge(x, w, by = "user_id")
    ## order by date and then remove duplicate tweet rows
    x <- x[!duplicated(x$status_id), ]
    x <- tibble::as_tibble(x, validate = FALSE)
  }
  x <- x[order(x$created_at), ]
  x
}

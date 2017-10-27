#' Search tweets (vectorized)
#'
#' Returns data from one or more search queries.
#'
#' @param q Vector of search queries.
#' @param n Number of tweets to return per query. Defaults to 100.
#'   Must be of length 1 or equal to length of user.
#' @param ... Other arguments passed on to \code{search_tweets}.
#' @return A tbl data frame with additional "query" feature.
#' @noRd
search_tweets_ <- function(q, n = 100, ...) {
  ## check inputs
  stopifnot(is.atomic(q), is.numeric(n))
  if (length(q) == 0L) {
    stop("No query found", call. = FALSE)
  }
  ## if missing issue warning and save NA position
  if (any(is.na(q))) {
    warning("NA queries found. Empty data frames returned for missing q values",
            call. = FALSE)
    mia <- which(is.na(q))
    q[is.na(q)] <- ""
  }
  ## search for each string in column of queries
  rt <- Map(search_tweets, q, n = n, ...)
  ## if missing, label appropriately
  if (any(is.na(q))) {
    q[mia] <- NA_character_
  }
  kp <- vapply(rt, function(x) NROW(x) > 0L, logical(1))
  if (sum(kp, na.rm = TRUE) == 0L) return(data.frame())
  rt <- rt[kp]
  q <- q[kp]
  ## add query variable to data frames
  rt <- Map(cbind, rt, query = q, stringsAsFactors = FALSE)
  ## merge users data into one data frame
  rt_users <- do.call("rbind", lapply(rt, users_data))
  ## merge tweets data into one data frame
  rt <- do.call("rbind", rt)
  ## set users attribute
  attr(rt, "users") <- rt_users
  ## return tibble (validate = FALSE makes it a bit faster)
  tibble::as_tibble(rt, validate = FALSE)
}

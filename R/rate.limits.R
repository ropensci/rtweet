#' rate_limit
#'
#' @param token An OAuth token (1.0 or 2.0)
#' @param query If null, returns entire rate limit request object as
#'   data frame. otherwise, query returns specific values matching
#'   the query of interest; e.g., \code{query = "lookup"} returns
#'   remaining limit for user lookup requests;
#'   \code{type = "followers"} returns remaining limit for
#'   follower id requests; \code{type = "friends"} returns
#'   remaining limit for friend id requests.
#' @seealso See \url{https://dev.twitter.com/overview/documentation}
#'   for more information on using Twitter's API.
#' @return response Rate limit response object or specific value of
#'   remaining requests
#' @export
rate_limit <- function(token, query = NULL, rest = TRUE) {

  url <- make_url(restapi = rest,
    query = "application/rate_limit_status",
    param = list(resources = "users,statuses,friends,search"))

  r <- TWIT(get = TRUE, url, config = token, catch_error = FALSE)

  json.obj <- from_js(r)

  if (is.null(query)) {
    rl_df <- as.data.frame(from_js(r), stringsAsFactors = FALSE)
    rl_df <- rl_df[!names(rl_df) == "access_token"]

    asdf <- data.frame(
      names = unique(gsub(".limit$|.reset$|.remaining$|", "",
        names(rl_df))),
      reset =  unlist(rl_df[seq(1, length(rl_df), 3)]),
      limit = unlist(rl_df[seq(2, length(rl_df), 3)]),
      remaining = unlist(rl_df[seq(3, length(rl_df), 3)]),
      stringsAsFactors = FALSE,
      row.names = NULL)

    asdf$remaining <- as.POSIXct(
        as.numeric(asdf$remaining),
        origin = "1970-01-01")

    asdf$names <- gsub(
      ".limit$|.reset$|.remaining$|", "", asdf$names)

    return(asdf[!duplicated(asdf[, 1]), ])
  }

  return(unlist(json.obj)[
      grepl(paste0(query,
          ".[limit|remaining|reset]"),
        names(unlist(
          json.obj)))])
}

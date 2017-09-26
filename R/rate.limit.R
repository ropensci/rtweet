#' rate_limit
#'
#' @description Returns rate limit information for Twitter
#'   access tokens.
#'
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param query If null, returns entire rate limit request object as
#'   data frame. otherwise, query returns specific values matching
#'   the query of interest; e.g., \code{query = "lookup/users"} returns
#'   remaining limit for user lookup requests;
#'   \code{type = "followers/ids"} returns remaining limit for
#'   follower id requests; \code{type = "friends/ids"} returns
#'   remaining limit for friend id requests.
#' @param rest Logical indicating whether to send request to REST
#'   API. At this time, this should always be TRUE.
#' @param parse Logical indicating whether to parse response object
#'   into tidy data frame.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#'
#' @return Data frame with rate limit respones details. If query
#'   is specified, only relevant rows are returned.
#' @export
rate_limit <- function(query = NULL,
                       token = NULL,
                       parse = TRUE,
                       ...) {
  if (!is.null(query) && inherits(query, "Token") ||
        is.list(query) && inherits(query[[1]], "Token")) {
    if (!is.null(token) && is.character(token)) {
      fix_query <- token
    } else {
      fix_query <- NULL
    }
    token <- query
    query <- fix_query
  } 
  if (is.null(token)) {
    token <- get_tokens()
  }
  if (is.list(token) && length(token) > 1L) {
    rl <- Map("rate_limit_", token, MoreArgs = list(query = query, parse = parse, ...))
    token_names <- go_get_var(token, "app", "appname", expect_n = length(rl))
    if (!parse) {
      names(rl) <- token_names
      return(rl)
    }
    for (i in seq_along(rl)) {
      rl[[i]]$token <- token_names[i]
    }
    do.call("rbind", rl)
  } else {
    rate_limit_(token, query, parse = parse, ...)
  }
}

rate_limit_ <- function(token,
                        query = NULL,
                        rest = TRUE,
                        parse = TRUE) {
  token <- check_token(token, query = NULL)
  url <- make_url(
    restapi = rest,
    query = "application/rate_limit_status")
  r <- TWIT(get = TRUE, url, config = token)
  if (parse) {
    rl_df <- .rl_df(r)
    if (!is.null(query)) {
      query <- fun2api(query)
      rl_df <- rl_df[grep(query, rl_df$query), ]
      row.names(rl_df) <- NULL
    }
    rl_df
  } else {
    r
  }
}


.rl_df <- function(r) {

    r <- from_js(r)

    data <- r$resources

    rl_df <- data.frame(
        query = gsub(".limit|.remaining|.reset", "",
                     gsub(".*[.][/]", "",
                          grep(".limit$", names(unlist(data)),
                               value = TRUE))),
        limit = unlist(lapply(data, function(y)
            lapply(y, function(x) getElement(x, "limit")))),
        remaining = unlist(lapply(data, function(y)
            lapply(y, function(x) getElement(x, "remaining")))),
        reset = unlist(lapply(data, function(y)
            lapply(y, function(x) getElement(x, "reset")))),
  	stringsAsFactors = FALSE,
        row.names = NULL)

    rl_df$reset <- difftime(
        as.POSIXct(as.numeric(rl_df$reset),
                   origin = "1970-01-01"),
        Sys.time(),
        units = "mins")

    rl_df
}


funs_and_apis <- function() {
  list(
    `search/tweets` = "search",
    `statuses/user_timeline` = "timeline",
    `statuses/user_timeline` = "get_timeline",
    `statuses/home_timeline` = "home_timeline",
    `statuses/home_timeline` = "get_home_timeline",
    `trends/place` = "get_trends",
    `followers/ids` = "get_followers",
    `followers/ids` = "followers",
    `friends/ids` = "get_friends",
    `friends/ids` = "friends",
    `favorites/list` = "get_favorites",
    `favorites/list` = "favorites",
    `search/tweets` = "search_tweets",
    `users/lookup` = "users",
    `users/search` = "search_users",
    `statuses/lookup` = "lookup_statuses",
    `statuses/lookup` = "statuses",
    `users/lookup` = "lookup_users"
  )
}


fun2api <- function(x) {
  funs <- funs_and_apis()
  if (x %in% names(funs)) {
    return(x)
  }
  names(funs)[match(x, funs)]
}

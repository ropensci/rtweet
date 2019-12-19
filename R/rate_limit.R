#' Get rate limit data for given Twitter access tokens.
#'
#' Returns rate limit information for one or more Twitter tokens,
#' optionally filtered by rtweet function or specific Twitter API
#' path(s)
#'
#' @param token One or more OAuth tokens. By default token = NULL
#'   fetches a non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param query Specific API (path) or a character function name,
#'   e.g., query = "get_timeline", used to subset the returned data.
#'   If null, this function returns entire rate limit request object
#'   as a tibble data frame. Otherwise, query returns specific values
#'   matching the query of interest; e.g., query = "lookup/users"
#'   returns remaining limit for user lookup requests; type =
#'   "followers/ids" returns remaining limit for follower id requests;
#'   type = "friends/ids" returns remaining limit for friend id
#'   requests.
#' @param parse Logical indicating whether to parse response object
#'   into a data frame.
#' @seealso
#'   \url{https://developer.twitter.com/en/docs/developer-utilities/rate-limit-status/api-reference/get-application-rate_limit_status}
#' @details If multiple tokens are provided, this function will return
#'   the names of the associated [token] applications as new variable
#'   (column) or as a named element (if parse = FALSE).
#' @return Tibble data frame with rate limit information pertaining to
#'   the limit (max allowed), remaining (specific to token), reset
#'   (minutes until reset), and reset_at (time of rate limit
#'   reset). If query is specified, only relevant rows are returned.
#' @examples
#'
#' \dontrun{
#'
#' ## get all rate_limit information for default token
#' rate_limit()
#'
#' ## get rate limit info for API used in lookup_statuses
#' rate_limit("lookup_statuses")
#'
#' ## get rate limit info for specific token
#' token <- get_tokens()
#' rate_limit(token)
#' rate_limit(token, "search_tweets")
#'
#' }
#'
#' @family tokens
#' @export
rate_limit <- function(token = NULL,
                       query = NULL,
                       parse = TRUE) {
  if (is.null(token)) {
    token <- get_tokens()
  }
  UseMethod("rate_limit")
}

#' @export
rate_limit.default <- function(token = NULL, query = NULL, parse = TRUE) {
  if (is.null(token)) {
    token <- get_tokens()
  }
  rate_limit_(token, query, parse)
}

#' @export
rate_limit.function <- function(token = NULL, query = NULL, parse = TRUE) {
  token <- as.character(substitute(token))
  if (is.character(token) && length(token) == 1L &&
      (is.null(query) || inherits(query, "Token") || is.list(query))) {
    fix_query <- token
    token <- query
    query <- fix_query
  }
  rate_limit_(token, query, parse)
}


token_name <- function(x) {
  x$app$appname
}

#' @export
rate_limit.NULL <- function(token = NULL, query = NULL, parse = TRUE) {
  if (is.null(token)) {
    token <- get_tokens()
  }
  if (is.function(query)) {
    query <- as.character(substitute(query))
  }
  rate_limit(token = token, query = query, parse = parse)
}

#' @export
rate_limit.bearer <- function(token = NULL, query = NULL, parse = TRUE) {
  rate_limit_(token, query, parse)
}


#' @export
rate_limit.character <- function(token = NULL, query = NULL, parse = TRUE) {
  if (is.character(token) && length(token) == 1L &&
        (is.null(query) || inherits(query, "Token") || is.list(query))) {
    fix_query <- token
    token <- query
    query <- fix_query
  }
  rate_limit_(token, query, parse)
}

#' @export
rate_limit.list <- function(token = NULL,
                            query = NULL,
                            parse = TRUE) {
  if (is.character(token) && length(token) == 1L &&
        (is.null(query) || inherits(query, c("Token", "list")))) {
    fix_query <- token
    token <- query
    query <- fix_query
  }
  if (is.null(token)) {
    token <- get_tokens()
  }
  rl <- Map(
    "rate_limit_",
    token = token,
    MoreArgs = list(query = query, parse = parse)
  )
  if (!parse) {
    token_names <- go_get_var(
      token, "app", "appname", expect_n = length(rl))
    names(rl) <- token_names
    return(rl)
  }
  do.call("rbind", rl)
}

rate_limit_ <- function(token,
                        query = NULL,
                        parse = TRUE) {
  token <- check_token(token)
  url <- make_url(
    restapi = TRUE,
    query = "application/rate_limit_status")
  if (inherits(token, "bearer")) {
    r <- TWIT(get = TRUE, url, token)
  } else {
    r <- TWIT(get = TRUE, url, config = token)
  }
  warn_for_twitter_status(r)
  r <- from_js(r)
  if (parse) {
    rl_df <- .rl_df(r)
    if (is.null(rl_df) || nrow(rl_df) == 0) {
      return(data.frame())
    }
    rl_df$app <- token_name(token)
    if (!is.null(query)) {
      query <- fun2api(query)
      query2 <- paste0("^", query, "$")
      if (any(grepl(query2, rl_df$query))) {
        rl_df <- rl_df[grep(query2, rl_df$query), ]
      } else {
        rl_df <- rl_df[grep(query, rl_df$query), ]
      }
      row.names(rl_df) <- NULL
    }
    rl_df
  } else {
    r
  }
}


.rl_df <- function(r) {
  if (has_name_(r, "errors")) return(data.frame())
  if (!has_name_(r, "resources")) return(data.frame())
  data <- r$resources
  if (!all(c("lists", "application", "search", "users") %in% names(data))) {
    return(data.frame())
  }
  rl_df <- tryCatch({
    data.frame(
    query = gsub("\\.limit$|\\.remaining$|\\.reset$", "",
                 gsub(".*[.][/]", "",
                      grep("\\.limit$", names(unlist(data)),
                           value = TRUE))),
    limit = unlist(lapply(data, function(y)
      lapply(y, function(x) getElement(x, "limit")))),
    remaining = unlist(lapply(data, function(y)
      lapply(y, function(x) getElement(x, "remaining")))),
    reset = unlist(lapply(data, function(y)
      lapply(y, function(x) getElement(x, "reset")))),
    stringsAsFactors = FALSE,
    row.names = NULL
  )},
    error = function(e) NULL)
  if (is.null(rl_df)) {
    return(data.frame())
  }
  rl_df$reset_at <- format_rate_limit_reset(rl_df$reset)
  if (inherits(rl_df$reset_at, "POSIXt")) {
    rl_df$reset <- difftime(
      rl_df$reset_at, Sys.time() - 1, units = "mins"
    )
  } else {
    rl_df$reset <- structure(NA_character_, class = "difftime", units = "mins")
  }
  rl_df$timestamp <- Sys.time()
  tibble::as_tibble(rl_df)
}

format_rate_limit_reset <- function(x) {
  x <- tryCatch(as.POSIXct(
    x, tz = "",
    origin = "1970-01-01"),
    error = function(e) return(x))
  if (!inherits(x, "POSIXt")) {
    x <- tryCatch(as.POSIXct(
      x, tz = "UTC",
      origin = "1970-01-01"),
      error = function(e) return(x))
  }
  if (!inherits(x, "POSIXt")) {
    x <- format_date(x)
  }
  x
}


funs_and_apis <- function() {
  list(
    `account/verify_credentials` = "authenticating_user_name",
    `application/rate_limit_status` = "rate_limit",

    `favorites/list` = "get_favorites",
    `favorites/list` = "favorites",

    `followers/ids` = "get_followers",
    `followers/ids` = "followers",
    `friends/ids` = "get_friends",
    `friends/ids` = "friends",

    `lists/lists` = "lists_users",
    `lists/members` = "lists_members",
    `lists/memberships` = "lists_memberships",
    `lists/subscribers` = "lists_subscribers",
    `lists/subscriptions` = "lists_subscriptions",
    `lists/statuses` = "lists_statuses",

    `search/tweets` = "search_tweets",
    `search/tweets` = "search_twitter",
    `search/tweets` = "search_tweet",
    `search/tweets` = "search_statuses",
    `search/tweets` = "search_status",

    `statuses/user_timeline` = "timeline",
    `statuses/user_timeline` = "get_timeline",
    `statuses/user_timeline` = "get_timelines",
    `statuses/home_timeline` = "home_timeline",
    `statuses/home_timeline` = "get_home_timeline",
    `statuses/mentions_timeline` = "get_mentions",
    `statuses/mentions_timeline` = "mentions",

    `statuses/retweets/:id` = "get_retweets",
    `statuses/retweeters/ids` = "get_retweeters",

    `statuses/lookup` = "lookup_statuses",
    `statuses/lookup` = "statuses_lookup",
    `statuses/lookup` = "get_statuses",
    `statuses/lookup` = "lookup_tweets",
    `statuses/lookup` = "get_tweets",

    `trends/place` = "get_trends",
    `trends/closest` = "trends_closest",
    `trends/closest` = "closest_trends",
    `trends/available` = "trends_available",
    `trends/closest` = "available_trends",

    `users/lookup` = "lookup_users",
    `users/lookup` = "get_users",
    `users/lookup` = "users_lookup",
    `users/lookup` = "user_lookup",
    `users/lookup` = "lookup_user",
    `users/search` = "search_users",
    `users/search` = "search_user",
    `users/search` = "get_user",

    `direct_messages` = "direct_messages",

    `collections/entries` = "collections_entries",
    `collections/list` = "collections_list",

    `friendships/lookup` = "lookup_friendships",
    `users/suggestions` = "suggested_users",
    `users/suggestions/:slug` = "suggested_slugs",
    `users/suggestions/:slug$|^users/suggestions` = "suggested_users_all"
  )
}

stream_api_funs <- function() {
  list(
    `statuses/sample` = "stream_tweets",
    `statuses/filter` = "stream_tweets"
  )
}

post_api_funs <- function() {
  list(
    ## post status
    `statuses/update` = "post_status",
    `media/upload` = "post_status",
    `statuses/update` = "post_tweet",
    `media/upload` = "post_tweet",
    `statuses/destroy/:id` = "post_tweet",
    `statuses/destroy/:id` = "post_status",

    ## dms
    `direct_messages/new` = "post_direct_message",
    `direct_messages` = "direct_messages_received",
    `direct_messages/events/list` = "direct_messages",
    `direct_messages/events/list` = "direct_messages_received",

    ## mute
    `mutes/users/create` = "post_mute",

    ## friendship status
    `friendships/update` = "post_friendship",
    `friendships/destroy` = "post_unfollow",
    `friendships/create` = "post_follow",

    ## favs
    `favorites/create` = "post_favorite",
    `favorites/destroy` = "post_unfavorite",

    ## lists
    `lists/create` = "post_list",
    `lists/members/create_all` = "post_list",
    `lists/destroy` = "post_list",
    `lists/members/destroy_all` = "post_list"
  )
}

fun2api <- function(x) {
  funs <- funs_and_apis()
  if (x %in% names(funs)) {
    return(x)
  }
  names(funs)[match(x, funs)]
}


#' @export
#' @rdname rate_limit
rate_limits <- function(token = NULL,
                        query = NULL,
                        parse = TRUE) {
  rate_limit(token = token, query = query, parse = parse)
}

#' Get tweets data for given statuses (status IDs).
#'
#' `r lifecycle::badge("deprecated")`
#' @inheritParams lookup_users
#' @inheritParams stream
#' @param statuses User id or screen name of target user.
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/get-statuses-lookup>
#' @return A tibble of tweets data.
#' @family tweets
#' @seealso [tweet_search_recent()], [`rtweet-deprecated`]
#' @export
lookup_tweets <- function(statuses, parse = TRUE, token = NULL,
                          retryonratelimit = NULL, verbose = TRUE) {
  # Prevents problems if someone passes a full table.
  if (is.data.frame(statuses)) {
    statuses <- ids(statuses)
  }
  chunks <- unname(split(statuses, (seq_along(statuses) - 1) %/% 100))
  params_list <- lapply(chunks, function(id) {
    list(
      id = paste(id, collapse = ","),
      tweet_mode = "extended",
      include_ext_alt_text = "true"
    )
  })

  results <- TWIT_paginate_chunked(token, "/1.1/statuses/lookup", params_list,
    retryonratelimit = retryonratelimit,
    verbose = verbose
  )

  if (parse) {
    results <- tweets_with_users(results)
    results$created_at <- format_date(results$created_at)
    # Reorder to match input, skip NA if the status is not found.
    m <- match(statuses, results$id_str)
    m <- m[!is.na(m)]
    results <- results[m, ]
    attr(results, "users") <- users_data(results)[m, ]
  }
  results
}

#' @export
#' @rdname lookup_tweets
#' @usage NULL
lookup_statuses <- function(statuses, parse = TRUE, token = NULL) {
  lifecycle::deprecate_warn("1.0.0", "lookup_statuses()", "lookup_tweets()")

  lookup_tweets(statuses = statuses, parse = parse, token = token)
}

#' Get tweet information
#'
#' Look up tweets up to 100 at the same time.
#' @inheritParams tweet_retweeted_by
#' @inheritParams stream
#' @param token This endpoint accepts a OAuth2.0 authentication (can be
#' created via [rtweet_oauth2()]) or a bearer token (can be created via [rtweet_app()]).
#' @param id At least a tweet id.
#' @seealso [lookup_tweets()]
#' @references
#' One tweet: <https://developer.twitter.com/en/docs/twitter-api/tweets/lookup/api-reference/get-tweets-id>
#'
#' Multiple tweets: <https://developer.twitter.com/en/docs/twitter-api/tweets/lookup/api-reference/get-tweets>
#' @export
#' @examples
#' if (FALSE){
#'  tweet_get("567053242429734913", parse = FALSE)
#'  tweet_get(c("567053242429734913", "567053242429734913"), parse = FALSE)
#'  tweet_get(c("567053242429734913", "567053242429734913"), parse = TRUE)
#' }
tweet_get <- function(id, expansions = NULL, fields = NULL, ..., token = NULL,
                      parse = TRUE, verbose = FALSE) {
  expansions <- check_expansions(
    arg_def(expansions,
            set_expansions(user = NULL, list = NULL)),
    set_expansions(user = NULL, list = NULL))
  fields <- check_fields(arg_def(fields, set_fields(list = NULL)),
                         metrics = NULL, list = NULL)
  expansions_for_fields(expansions, fields)
  parsing(parse, expansions, fields)
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  stopifnot("Requires valid ids." = is_id(id))
  if (length(id) == 1) {
    url <- paste0("tweets/", id)
  } else if (length(id) <= 100 ) {
    data <- c(ids = paste(id, collapse = ","), data)
    url <- "tweets"
  } else {
    abort("Only 100 tweets can be processed at once.")
  }

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 900 / (60 * 15), c("tweet.read", "users.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, 1, length(ids), verbose = verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}


#' Post a tweet
#'
#' This function uses the API v2 to post tweets.
#' @param text Text of the tweet.
#' @param ... Other accepted arguments.
#' @inheritParams tweet_get
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/manage-tweets/api-reference/post-tweets>
#' @examples
#' if (FALSE) {
#'  # It requires the Oauth2.0 Authentication
#'   tp_id <- tweet_post("Posting from #rtweet with the basic plan")
#'   tweet_post()
#' }
tweet_post <- function(text, ..., token = NULL) {
  # To store the token at the right place: see ?httr2::oauth_cache_path
  withr::local_envvar(HTTR2_OAUTH_CACHE = auth_path())
  options <- list(text = text, ...)

  if (sum(c("media", "quote_tweet_id", "poll") %in% names(options) ) > 1) {
    abort(c("media and quoting a tweet are excluse",
            i = "Chose one or the other."))
  }

  if ("for_super_followers_only" %in% names(options) && !is_logical(options[["for_super_followers_only"]])) {
      abort("Provide only TRUE or FALSE for 'for_super_followers_only'")
  }
  if (check_reply_settings(options)) {
    abort(c("Provide a valid reply_setting option:",
          i = "If not provided it is open to everybody, or choose from:",
          "*" = "mentionedUsers",
          "*" = "following"
          ))
  }

  # Rates from the website app and user limits
  req_archive <- endpoint_v2("tweets", 200/(60*15), c("tweet.read", "users.read", "tweet.write"))
  req_final <- httr2::req_body_json(req_archive, options)
  resp <- httr2::req_perform(req_final)
  r <- resp(resp)
  parse(r, NULL, NULL)
}

check_reply_settings <- function(options) {
  included <- "reply_settings" %in% options
  length1 <- length(options[["reply_settings"]]) > 1
  valid <- options[["reply_settings"]] %in% c("mentionedUsers")
  included && length1 && valid
}

#' Delete tweet
#'
#' Will delete a tweet
#' @inheritParams tweet_get
#' @seealso [tweet_post()], [tweet_search_recent()], [user_timeline()]
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/manage-tweets/api-reference/delete-tweets-id>
tweet_delete <- function(id, verbose = FALSE, token = NULL) {
  # To store the token at the right place: see ?httr2::oauth_cache_path
  withr::local_envvar(HTTR2_OAUTH_CACHE = auth_path())
  stopifnot("Requires valid ids." = is_id(id))
  if (length(id) == 1) {
    url <- paste0("tweets/", id)
  }  else {
    abort("Only 1 tweets can be processed at once.")
  }

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 50 / (60 * 15),
                             c("tweet.read", "users.read", "tweet.write"))
  req_final <- httr2::req_url_query(req_archive)

  r <- httr2::req_perform(httr2::req_method(req_final, "DELETE"))
  resp <- resp(r)
  resp$data$deleted
}



#' Get quoted tweet information
#'
#' Look up tweets quoting that tweet id.
#' @inheritParams tweet_get
#' @inheritParams tweet_search_recent
#' @param id At least a tweet id.
#' @references
#' One tweet: <https://developer.twitter.com/en/docs/twitter-api/tweets/quote-tweets/api-reference/get-tweets-id-quote_tweets>
#' @export
#' @examples
#' if (FALSE){
#'  tweet_quoted("1631945769748930561", parse = FALSE)
#' }
tweet_quoted <- function(id, n = 100, expansions = NULL, fields = NULL, ..., token = NULL,
                         parse = TRUE, verbose = FALSE) {
  expansions <- check_expansions(arg_def(expansions, set_expansions(list = NULL)))
  fields <- check_fields(arg_def(fields, set_fields()), metrics = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  stopifnot(is_n(n))
  max_results <- check_interval(n, 10, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- c(max_results = max_results, data)
  data <- data[data != ""]

  stopifnot("Requires valid ids." = is_id(id))
  if (length(id) == 1) {
    url <- paste0("tweets/", id, "/quote_tweets")
  } else {
    abort("Only 1 tweet can be processed at once.")
  }

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 75 / (60 * 15), c("tweet.read", "users.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose = verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

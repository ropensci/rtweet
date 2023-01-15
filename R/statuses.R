#' Get tweets data for given statuses (status IDs).
#'
#' @inheritParams lookup_users
#' @param statuses User id or screen name of target user.
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/get-statuses-lookup>
#' @examples
#'
#' if (auth_has_default()) {
#'   statuses <- c(
#'     "567053242429734913",
#'     "266031293945503744",
#'     "440322224407314432"
#'   )
#'
#'   ## lookup tweets data for given statuses
#'   tw <- lookup_tweets(statuses)
#'   tw
#' }
#' @return A tibble of tweets data.
#' @family tweets
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

#' @references
#' One tweet: <https://developer.twitter.com/en/docs/twitter-api/tweets/lookup/api-reference/get-tweets-id>
#' Multiple tweets: <https://developer.twitter.com/en/docs/twitter-api/tweets/lookup/api-reference/get-tweets>
#' get_tweet("567053242429734913", parse = FALSE)
#' get_tweet(c("567053242429734913", "567053242429734913"), parse = FALSE)
#' get_tweet(c("567053242429734913", "567053242429734913"), expansions = NULL, fields = NULL, parse = FALSE)
get_tweet <- function(id, expansions = NA, fields = NA, ..., token = NULL,
                      parse = TRUE) {
  fields <- check_fields(fields, metrics.fields = NULL)
  expansions <- check_expansions(expansions, tweet_expansions())
  parsing(parse)
  data <- c(expansions, fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  if (length(id) == 1) {
    url <- paste0("tweets/", id)
  } else {
    data <- c(ids = paste(id, collapse = ","), data)
    url <- "tweets"
  }
  # Rates from the website app and user limits
  rate <- max(300/(60*15), 900/(60*15))

  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  resp <- httr2::req_perform(req_final)
  httr2::resp_body_json(resp)
}

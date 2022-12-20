#' Streaming
#'
#' Open a streaming connection with Twitter and stores tweets for as long as you
#'  wish.
#'
#' The connection can be left open as long as you wish, the data is appended to
#' the file provided. Be aware that the stream might have incomplete records
#' (you won't be able to read directly from the json file).
#' One tweet might belong to multiple rules.
#'
#' @param file Path to a file where the raw streaming should be stored.
#' @inheritParams TWIT_paginate_max_id
#' @param timeout time, in seconds, of the recording stream.
#' @param expansions Expansions you want to use see [tweet_expansions()].
#' Use NULL to get all expansions, use NA to not use any field, or a vector
#' with the fields you want.
#' @param fields Fields you want to retrieve see [Fields]. Use NULL to get all
#' allowed fields, use NA to not use any field, pass a list with the fields you want.
#' @param append Append streaming to the file? Default does but it is
#' recommended to have a new file for each call.
#' @param query If `NULL` returns the current rules, else depending:
#'  - In stream_add_rule it should be a list of value and tag.
#'  - In stream_rm_rule it should be a vector of ids of rules to be removed
#' @param dry Check if the addition or removal of the rule works.
#' @param ... Other parameters passed to the body of the request.
#'
#' @return The records in the streaming.
#' @seealso
#' Rules for filtered stream: <https://developer.twitter.com/en/docs/twitter-api/tweets/filtered-stream/integrate/build-a-rule>
#' Sampled stream: <https://developer.twitter.com/en/docs/twitter-api/tweets/volume-streams/api-reference/get-tweets-sample-stream>
#' Filtered stream: <https://developer.twitter.com/en/docs/twitter-api/tweets/filtered-stream/api-reference/get-tweets-search-stream>
#' [ids]
#' @rdname stream
#' @name stream
#' @examples
#' # Requires a bearer token
#' if (FALSE) {
#'   # How many rules do we have
#'   stream_add_rule(NULL)
#'   # Add new rule
#'   new_rule <- stream_add_rule(list(value = "#rstats", tag = "rstats"))
#'   new_rule
#'   # Open filtered streaming connection for 30s
#'   filtered_stream(file = tempfile(), timeout = 30)
#'   # Remove rule
#'   stream_rm_rule(ids(new_rule))
#'   # Open random streaming connection
#'   sample_stream(file = tempfile(), timeout = 3)
#' }
NULL

#' @export
#' @describeIn stream Start a filtered stream according to the rules.
filtered_stream <- function(timeout, file = tempfile(), expansions = NA, fields = NA, ...,
                            token = NULL, append = TRUE, parse = TRUE) {
  allowed_expansions <- c("attachments.poll_ids",  "attachments.media_keys",
                          "author_id", "edit_history_tweet_ids",
                          "entities.mentions.username", "geo.place_id",
                          "in_reply_to_user_id", "referenced_tweets.id",
                          "referenced_tweets.id.author_id")
  parsing(parse)
  fields <- check_fields(fields,
                         media_fields = c("duration_ms", "height", "media_key",
                                          "preview_image_url", "type", "url", "width",
                                          "public_metrics", "alt_text", "variants"),
                         place_fields = c("contained_within", "country", "country_code", "full_name", "geo", "id", "name", "place_type"),
                         poll_fields = c("duration_minutes", "end_datetime", "id", "options", "voting_status"),
                         tweet_fields = c("attachments", "author_id", "context_annotations", "conversation_id", "created_at", "edit_controls", "entities", "geo", "id", "in_reply_to_user_id", "lang", "public_metrics", "possibly_sensitive", "referenced_tweets", "reply_settings", "source", "text", "withheld"),
                         user_fields = c("created_at", "description", "entities", "id", "location", "name", "pinned_tweet_id", "profile_image_url", "protected", "public_metrics", "url", "username", "verified", "withheld"),
                         metrics_fields = NULL)

  expansions <- check_expansions(expansions, allowed_expansions)
  req_stream <- endpoint_v2(token, "tweets/search/stream", 50 / (60*15))
  data <- c(expansions, fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  req_stream <- httr2::req_url_query(req_stream, !!!data)
  if (file.exists(file) && isFALSE(append)) {
    stop("File already exists and append = FALSE", call. = FALSE)
  }
  out <- stream(req_stream, file, timeout = timeout)
  return(out)
}

stream <- function(req, file, timeout) {
  file_tmp <- tempfile(pattern = "rtweet_raw_stream")
  callback <- function(x) {
    chr <- rawToChar(x)
    if (!isTRUE(chr)) {
      cat(chr, file = file_tmp, appends = TRUE)
    }
    TRUE
  }

  resp <- httr2::req_stream(req, callback, timeout_sec = timeout)

  if (!file.exists(file_tmp)) {
    warning("No matching tweets with streaming rules were found in the time provided.",
            call. = FALSE)
    return(NULL)
  }

  # Create a json file but each line is a datapoint in the ndjson format
  # This makes it appendable to other times.
  text_json <- readLines(file_tmp, warn = FALSE)
  # Skip the first and last lines as most probably they are faulty:
  # httr2 doesn't wait for a line to close the connection.
  good_json <- text_json[c(-1, -length(text_json))]
  if (is.null(good_json) || length(good_json) == 0) {
    warning("No matching tweets with streaming rules were found in the time provided.",
            call. = FALSE)
    return(NULL)
  }
  cat(good_json, file = file, append = TRUE, sep = "\n")
  readable_json <- paste(good_json, collapse = "\n")
  text_c <- textConnection(readable_json)
  out <- jsonlite::stream_in(text_c, simplifyVector = FALSE, flatten = FALSE)
  close.connection(text_c)
  out
}

#' @describeIn stream Add rules for the filtered streaming.
#' @export
stream_add_rule <- function(query, dry = FALSE, token = NULL) {
  if (!is.null(query)) {
    query <- list(add = check_stream_add(query))
  }
  streaming <- stream_rules(query, token, auto_unbox = TRUE, pretty = FALSE)
  if (isTRUE(dry)) {
    streaming <- httr2::req_body_form(streaming, dry_run = dry)
  }
  out <- httr2::req_perform(streaming)
  out <- resp(out)
  handle_rules_resp(out)
}

#' @describeIn stream Remove rules from the filtered streaming
#' @export
stream_rm_rule <- function(query, dry = FALSE, token = NULL) {
  if (!is.null(query)) {
    query <- check_stream_remove(query)
  }
  rm_rule <- stream_rules(query, token, pretty = FALSE, auto_unbox = TRUE)
  if (isTRUE(dry)) {
    rm_rule <- httr2::req_body_form(rm_rule, dry_run = dry)
  }
  out <- httr2::req_perform(rm_rule)
  out <- resp(out)
  handle_rules_resp(out)
}

handle_rules_resp <- function(x) {
  if (has_name_(x, "errors")) {
    warning("There are errors in the requests: ",
            x$errors$title,
            "\nCheck the returned object for more details.", call. = FALSE)
    return(x)
  }
  df <- x$meta

  rules <- do.call(rbind, lapply(x$data, list2DF))
  # Ensure that the same order is always used
  if (!is.null(rules)) {
    rules <- rules[, c("id", "value", "tag")]
  }
  attr(df, "rules") <- rules
  class(df) <- c("rules", class(df))
  df
}

stream_rules <- function(query = NULL, token = NULL, ...) {
  req <- endpoint_v2(token, "tweets/search/stream/rules", 450 / (15 * 60))

  if (!is.null(query)) {
    req <- httr2::req_body_json(req, data = query, ...)
  }
  req
}

is_rule <- function(q) {
  if (!has_name(q, "value")) {
    stop("Please add value for filtering and a tag", call. = FALSE)
  }
  nc <- nchar(q[["value"]])
  if (any(nc > 1024)) {
    stop("Value cannot be longer than 1024 characters", call. = FALSE)
  } else if (any(nc > 512)) {
    warning("Requires academic research access.", call. = FALSE)
  }

  if (length(nc) > 1000) {
    stop("Impossible to have more than 1000 rules", call. = FALSE)
  } else if (length(nc) > 5) {
    warning("Requires elevated or academic research access", call. = FALSE)
  }

  if (is.null(q[["tag"]]) || any(nchar(q[["tag"]]) == 0)) {
    stop("Add tags for the rules for better handling of the streaming output",
         call. = FALSE)
  }
  q
}

check_stream_add <- function(q) {
  if (any(lengths(q) == 1)) {
    out <- list(is_rule(q))
  } else {
    out <- lapply(q, is_rule)
  }
  out

}

check_stream_remove <- function(q) {
  if (is.numeric(q)) {
    q <- as.character(q)
  }

  if (!any(grepl("^[0-9]{19}$", q))) {
    stop("Streaming ids should be 19 numbers long", call. = FALSE)
  }
  if (length(q) == 1) {
    list(delete = list(ids = list(q)))
  } else {
    list(delete = list(ids = q))
  }
}


split_stream <- function(file, path) {
  rL <- readLines(file)
  # For each tag write the line to that file
  # Omit last line as an incomplete record (easiest solution to parse it correctly)
  lines <- rL[-length(rL)]
  cat(paste0(lines, "\n"), file = file, fill = FALSE)
  # Fix stream
  json <- jsonlite::stream_in(file(file), pagesize = 1, verbose = FALSE)
  lines <- lines[nzchar(lines)]
  tags <- character(length(lines))
  for (i  in seq_along(tags)) {
    tag <- substring(lines[i], regexpr("\",\"tag\":\"(.+)\"", lines[i]) + 9)
    tag <- substring(tag, first = 1, last = regexpr("\"", tag) - 1)
    tag <- gsub("\\s+", "_", tag)
    tags[i] <- tag
  }
  tweets <- split(lines, tags)
  for (tag in names(tweets)) {
    file <- normalizePath(file.path(path, paste0(tag, ".json")), mustWork = FALSE)
    cat(tweets[[tag]], file = file, append = TRUE, fill = TRUE)
  }
  TRUE
}

#
# gov_url <- "https://api.dummy.gov/destiny/v1/placeholder"
#
# req <- request(gov_url) %>%
#   req_body_json(data = list(
#     param1  = "10",
#     api_key = "abcdefg",
#     param2  = "xyz",
#     param3  = "09/10/2022")
#   )
#
# resp <- req_perform(req)
#
# # Slower
# a <- stream_in(con = file("~/Downloads/rtweet_data/streaming_valid.json"), pagesize = 1)
# # Faster
# b <- stream_in(con = file("~/Downloads/rtweet_data/streaming_valid.json"), pagesize = 344)
# # But they both miss some records (there are 344 lines but just 341 records)


#' @describeIn stream Retrieve a sample of the tweets posted.
#' @export
sample_stream <- function(timeout, file = tempfile(),
                          expansions = NA, fields = NA, ...,
                          token = NULL, parse = TRUE, append = TRUE) {
  fields <- check_fields(fields,
                        media_fields = c("duration_ms", "height", "media_key", "preview_image_url", "type", "url", "width", "public_metrics", "alt_text", "variants"),
                        place_fields = c("contained_within", "country", "country_code", "full_name", "geo", "id", "name", "place_type"),
                        poll_fields = c("duration_minutes", "end_datetime", "id", "options", "voting_status"),
                        tweet_fields = c("attachments", "author_id", "context_annotations", "conversation_id", "created_at", "edit_controls", "entities", "geo", "id", "in_reply_to_user_id", "lang", "public_metrics", "possibly_sensitive", "referenced_tweets", "reply_settings", "source", "text", "withheld"),
                        user_fields = c("created_at", "description", "entities", "id", "location", "name", "pinned_tweet_id", "profile_image_url", "protected", "public_metrics", "url", "username", "verified", "withheld")
  )
  expansions <- check_expansions(expansions, c("attachments.poll_ids", "attachments.media_keys", "author_id", "edit_history_tweet_ids", "entities.mentions.username", "geo.place_id", "in_reply_to_user_id", "referenced_tweets.id", "referenced_tweets.id.author_id"))
  parsing(parse)

  req_stream <- endpoint_v2(token, "tweets/sample/stream", 50 / (60*15))

  data <- c(expansions, fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  req_stream <- httr2::req_url_query(req_stream, !!!data)
  if (file.exists(file) && isFALSE(append)) {
    stop("File already exists and append = FALSE", call. = FALSE)
  }
  out <- stream(req_stream, file, timeout = timeout)
  return(out)
}

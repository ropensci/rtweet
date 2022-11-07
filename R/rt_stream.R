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
#' @param fields Fields you want to retrieve see [Fields].
#' @param append Append streaming to the file? Default does but it is
#' recommended to have a new file for each call.
#' @param query If `NULL` returns the current rules, else depending:
#'  - In stream_add_rule it should be a list of value and tag.
#'  - In stream_rm_rule it should be a vector of ids of rules to be removed
#' @param dry Check if the addition or removal of the rule works.
#' @para ... Other parameters passed to the body of the request.
#'
#' @return The path to the file with the tweets of the streaming.
#' @seealso Filtered stream: <https://developer.twitter.com/en/docs/twitter-api/tweets/filtered-stream/integrate/build-a-rule>
#' @rdname stream
#' @examples
#' # List current rules
#' stream_add_rule(NULL)
#' # Add new rule
#' stream_add_rule(list(value = "testing rules rtweet", tag = "tsrt"))
#' (rm <- stream_add_rule(NULL))
#' # Open filtered streaming connection for 30s
#' filtered_stream(tempfile(), 30)
#' # Remove rule
#' stream_rm_rule(df$id[df$tag == "ts"])
#' # Open random streaming connection
#' sample_stream(tempfile(), 3)
NULL

#' @export
#' @describeIn stream Start a filtered stream according to the rules.
filtered_stream <- function(file, timeout, expansions = c(), fields = c(), ...,
                            token = NULL, append = TRUE) {

  check_fields(fields)
  check_expansions(expansions)
  if (!rlang::is_logical(append)) {
    warning("Appending the data to the file.")
  }

  req_stream <- endpoint_v2(token, "tweets/search/stream", 50 / (60*15))
  data <- list(expansions, fields, ...)
  req_stream <- httr2::req_body_form(req_stream, data)
  stream(req_stream, file, append = append, timeout = timeout)
}

stream <- function(req, file, append, timeout) {
  callback <- function(x) {
    chr <- rawToChar(x)
    if (!isTRUE(chr)) {
      cat(chr, file = file, appends = append)
    }
    TRUE
  }

  a <- req_stream(req, callback, timeout_sec = timeout)
  file
}

#' @describeIn stream Add rules for the filtered streaming.
#' @export
stream_add_rule <- function(query, dry = FALSE, token = NULL) {
  if (!is.null(query)) {
    query <- list(add = list(check_stream_add(query)))
  }
  streaming <- stream_rules(query, token, auto_unbox = TRUE, pretty = FALSE)
  if (isTRUE(dry)) {
    streaming <- httr2::req_body_form(streaming, dry_run = dry)
  }
  out <- httr2::req_perform(streaming)
  out <- httr2::resp_body_json(out)

  df <- do.call(rbind, lapply(out$data, list2DF))
  attr(df, "meta") <- out$meta
  class(df) <- c("rules", class(df))
  df
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
  httr2::resp_body_json(out)
}

stream_rules <- function(query = NULL, token = NULL, ...) {
  req <- endpoint_v2(token, "tweets/search/stream/rules", 450 / (15 * 60))

  if (!is.null(query)) {
    req <- httr2::req_body_json(req, data = query, ...)
  }
  req
}


check_stream_add <- function(q) {

  # Empty rules is asking for existing rules
  if (is.null(q)) {
    return(NULL)
  }
  if (!has_name(q, "value")) {
    stop("Please add streaing for filtering and a tag", call. = FALSE)
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

check_stream_remove <- function(q) {
  if (is.null(q)) {
    return(q)
  }
  if (is.numeric(q)) {
    q <- as.character(q)
  }

  if (!any(grepl("^[0-9]{19}$", q))) {
    stop("Streaming ids should be 19 numbers long", call. = FALSE)
  }
  list(delete = list(ids = list(q)))
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
sample_stream <- function(file,
                          timeout,
                          expansions = c("attachments.poll_ids", "attachments.media_keys", "author_id", "edit_history_tweet_ids", "entities.mentions.username", "geo.place_id", "in_reply_to_user_id", "referenced_tweets.id", "referenced_tweets.id.author_id"),
                          fields = c(),
                          token = NULL, parse = TRUE, append = TRUE) {
  check_fields(fields)
  check_expansions(expansions)
  if (!rlang::is_logical(append)) {
    warning("Appending the data to the file.")
  }
  parsing(parse)
  req_stream <- endpoint_v2(token, "tweets/sample/stream", 50 / (60*15))
  stream(req_stream, file, append = append, timeout = timeout)
}

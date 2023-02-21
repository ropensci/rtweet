#' Collect statuses contained in a thread
#'
#' Return all statuses that are part of a thread (Replies from a user to their
#' own tweets). By default the function traverses first backwards from the
#' origin status_id of the thread up to the root, then checks if there are any
#' child statuses that were posted after the origin status.
#'
#' The backwards method looks up the tweet which is replying to, so it works if
#' starting from the last tweet of the thread.
#'
#' The forwards method looks for newer replies to the tweet provided. If the tweet doesn't have a reply it won't be able to find anything.
#' The forwards method is limited by the timeline API (See [get_timeline()]).
#' @param tw [lookup_tweets()] output containing
#'  at least the last status in the thread or an id of a tweet.
#' @param traverse character, direction to traverse from origin status in `tw`.
#' It is not recommended to change the default if you don't know at which point of a thread you are starting.
#' @param verbose logical, output to console status of traverse.
#' @return Tweets in a structure like [lookup_tweets()].
#' @examples
#' if (auth_has_default()) {
#' tw_thread <- tweet_threading("1461776330584956929")
#' tw_thread
#' }
#' @export
tweet_threading <- function(tw, traverse = c("backwards", "forwards"), verbose = FALSE) {

  # Accept tweets ids
  if (is.character(tw)) {
    tw <- lookup_tweets(tw[1])
  }
  stopifnot(is_logical(verbose))

  # Prevent a forwards, backwards order
  if (length(unique(traverse)) == 2 && all(c("backwards", "forwards") %in% traverse)) {
    traverse <- c("backwards", "forwards")
  }
  for (i in traverse) {
    direction <- switch(i,
      backwards = tweet_threading_backwards,
      forwards = tweet_threading_forwards,
      abort("`traverse` must contain only 'backwards' or 'forwards")
    )

    tw <- direction(tw, verbose)
    if (verbose) cat("\n")
  }

  tw[order(tw$created_at), ]
}

tweet_threading_backwards <- function(tw, verbose = FALSE) {
  last_found <- FALSE

  if (verbose) {
    message("Initializing Backwards Traverse")
  }
  # Not a thread:
  if (nrow(tw) == 0) {
    return(tw)
  }
  counter <- 0L
  ud <- unique(users_data(tw))
  if (nrow(ud) >= 2) {
    stop("Data must be from a single user.", call. = FALSE)
  }
  user_data_tw <- ud
  while (!last_found) {
    nr <- nrow(tw)

    if (!is.na(tw$in_reply_to_status_id[nr])) {
      tw_head <- lookup_tweets(tw$in_reply_to_status_id_str[nr])
      user_data <- users_data(tw_head)

      if (user_data$id_str != unique(ud$id_str)) {
        stop("Reply to a different user.", call. = FALSE)
      }

      last_found <- is.na(tw_head$in_reply_to_status_id[1])
      # Bind replies with the latest reply below
      tw <- rbind(tw, tw_head)
      user_data_tw <- rbind(user_data, user_data_tw)
      counter <- counter + 1L
    } else {
      if (verbose) {
        cat("Initial tweet of thread found.")
      }
      break
    }
  }
  structure(tw, "users" = user_data_tw)
}

tweet_threading_forwards <- function(tw, verbose = FALSE) {
  if (verbose) {
    message(sprintf("Retrieving last statuses from @%s's timeline", tw$screen_name[1]))
  }
  ud <- users_data(tw)

  timeline <- get_timeline(ud$id_str[1], since_id = tw$id_str,
                           verbose = verbose, n = Inf)

  if (verbose) {
    message("Initializing Forwards Traverse")
  }

  while (any(timeline$in_reply_to_status_id_str %in% tw$id_str)) {
    idx <- which(timeline$in_reply_to_status_id_str %in% tw$id_str)
    tw <- rbind(tw, timeline[idx, ])
    timeline <- timeline[-idx, ]
  }

  if (verbose) {
    message("No more tweets found on the thread.")
    message("Either it is before the last 3200 tweets or the thread is finished.")
  }

  ud <- ud[rep(1, nrow(tw)), ]
  rownames(ud) <- NULL
  structure(tw, "users" = ud)
}

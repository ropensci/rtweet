#' Collect statuses contained in a thread
#'
#' Return all statuses that are part of a thread (Replies from a user to their 
#' own tweets). By default the function traverses first backwards from the 
#' origin status_id of the thread up to the root, then checks if there are any 
#' child statuses that were posted after the origin status.
#'
#' @param tw [lookup_tweets()] output containing
#'  at least the last status in the thread or an id of a tweet. 
#' @param traverse character, direction to traverse from origin status in tw,
#'  Default: c('backwards','forwards')
#' @param n numeric, timeline to fetch to start forwards traversing, Default: 10
#' @param verbose logical, Output to console status of traverse, Default: FALSE
#' @return [lookup_tweets()] tibble
#' @examples
#' \dontrun{
#' tw_thread <- tweet_threading("1461776330584956929")
#' tw_thread
#' }
#' @export
tweet_threading <- function(tw, traverse = c("backwards", "forwards"), n = 10, verbose = FALSE) {
  
  # Accept tweets ids
  if (is.character(tw)) {
    tw <- lookup_tweets(tw[1]) 
  }
  
  stopifnot("Provide a number above 0" = n > 0)
  
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

    tw <- direction(tw, n, verbose)
    if (verbose) cat("\n")
  }

  tw
}

tweet_threading_backwards <- function(tw, n = NULL, verbose = FALSE) {
  last_found <- FALSE

  if (verbose) {
    message("Initializing Backwards Traverse")
  }
  # Not a thread:
  if (nrow(tw) == 0) {
    return(tw)
  }
  counter <- 0L
  ud <- users_data(tw)
  user_data_tw <- ud
  while (!last_found) {
    nr <- nrow(tw)
    
    if (!is.na(tw$in_reply_to_status_id[nr])) {
      tw_head <- lookup_tweets(tw$in_reply_to_status_id_str[nr])
      user_data <- users_data(tw_head)
      
      if (user_data$id_str != ud$id_str) {
        stop("Reply to a different user.", call. = FALSE)
      }
      
      last_found <- is.na(tw_head$in_reply_to_status_id[1])
      # Bind replies with the latest reply below
      tw <- rbind(tw_head, tw) 
      user_data_tw <- rbind(user_data_tw, user_data)
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

tweet_threading_forwards <- function(tw, n = 10, verbose = FALSE) {
  if (verbose) {
    message(sprintf("Retrieving last %s statuses from @%s's timeline", n, tw$screen_name[1]))
  }
  ud <- users_data(tw)
  
  # Assumes that the latest reply is last
  # Could bite if someone uses traverse = c("forwards", "backwards")
  from_id <- tw$id_str[nrow(tw)] 
  
  timeline <- get_timeline(ud$id_str[1], since_id = tw)
  idx <- which(timeline$in_reply_to_status_id_str %in% from_id)
  last_found <- length(idx) == 0

  if (verbose) {
    message("Initializing Forwards Traverse")
  }

  counter <- 0

  while (last_found) {
    timeline2 <- get_timeline(ud$id_str[1], since_id = tw, max_id = timeline)
    idx <- which(timeline$in_reply_to_status_id_str %in% from_id)
    last_found <- length(idx) == 0
    counter <- counter + 1
    timeline <- rbind(timeline2, timeline)
    if (last_found && verbose) {
      cat(".")
      
      if (counter %% 80 == 0) {
        cat(sprintf(" %s \n", counter))
      }
    } else if (verbose) {
      cat(sprintf(" %s statuses found \n", counter))
    }
  }
  while (TRUE) {
    last_reply <- timeline$id_str[idx[length(idx)]]
    if (last_reply %in% timeline$in_reply_to_status_id_str) {
      i <- which(timeline$in_reply_to_status_id_str == last_reply)
      idx <- c(idx, i)
    } else {
      break
    }
  }
  rbind(tw, timeline[idx, ])
}

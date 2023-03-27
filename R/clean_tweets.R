#' Clean text of tweets
#'
#' Removes from the text, users mentions, hashtags, urls and media.
#' Some urls or other text might remain if it is not recognized as an entity by
#' the API.
#' @param x Tweets
#' @param clean Type of elements to be removed.
#' @return A vector with the text without the entities selected
#' @export
#' @examples
#' if (auth_has_default()) {
#' tweets <- search_tweets("weather")
#' tweets
#'
#' # tweets
#' clean_tweets(tweets)
#' }
clean_tweets <- function(x, clean = c("users", "hashtags", "urls", "media")) {
  if (is.character(x)) {
    abort("You should provide tweets with all the users and hashtags information")
  }

  tweets <- nrow(x)
  final_text <- vector(mode = "character", length = tweets)
  clean <- match.arg(clean, several.ok = TRUE)
  for (tweet_n in seq_len(tweets)) {
    text <- x$full_text[tweet_n]

    start <- vector("numeric")
    end <- vector("numeric")
    if ("users" %in% clean) {
      i <- x$entities[[tweet_n]][["user_mentions"]][["indices"]]
      if (!is.null(i)) {
        start <- c(start, i_type(i))
        end <- c(end, i_type(i, type = "end"))
      }
    }

    if ("hashtags" %in% clean) {
      hashtags <- x$entities[[tweet_n]][["hashtags"]][["indices"]]
      if (!is.null(hashtags)) {
        i <- as.data.frame(do.call(rbind, hashtags))

        start <- c(start, i_type(i))
        end <- c(end, i_type(i, type = "end"))
      }
    }

    if ("media" %in% clean) {
      media <- x$entities[[tweet_n]][["media"]][["indices"]]
      if (!is.null(media)) {
        i <- as.data.frame(do.call(rbind, media))
        start <- c(start, i_type(i))
        end <- c(end, i_type(i, type = "end"))
      }
    }

    if ("urls" %in% clean) {
      i <- x$entities[[tweet_n]][["urls"]][["indices"]]
      if (!is.null(i)) {
        start <- c(start, i_type(i))
        end <- c(end, i_type(i, type = "end"))
      }
    }
    nothing <- start != 0
    start <- start[nothing]
    end <- end[nothing]

    if (length(start) < 1) {
      final_text[tweet_n] <- text
    } else {
      # message(tweet_n)
      final_text[tweet_n] <- repaste(text, start, end)
    }
  }
  # Remove tags about retweets
  gsub("RT : ", final_text, replacement = "", fixed = TRUE)
}

i_type <- function(x, type = "start") {
  if (length(x) == 1 && is.na(x)) {
    return(0)
  }
  x[[type]]
}

repaste <- function(text, start, stop) {
  stopifnot(length(text) == 1)
  text <- strsplit(text, "")[[1]]
  remove <- vector(mode = "numeric")
  # Account for index starting at 0 in Twitter API output
  start <- start + 1
  for (s_p in seq_along(start)) {
    position_str <- seq(from = start[s_p], to = stop[s_p])
    remove <- c(remove, position_str)
  }
  paste(text[-remove], collapse = "")
}

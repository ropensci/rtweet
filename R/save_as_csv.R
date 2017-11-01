#' Save Twitter data as a comma separated value file.
#'
#' Saves tweets and users data as CSV files.
#'
#' @param x Data table to be saved (tweets or user object) generated
#'   via rtweet function like \code{\link{search_tweets}}. If x is a list
#'   object containing both tweets and users data (which is currently
#'   the output for many of the rtweet functions), then a CSV file is
#'   created and saved for each object using the file_name provided as
#'   a base--e.g, if x is a list object from search_tweets with
#'   file_name = "election", this function will save both the
#'   tweets data ("election.tweets.csv") and the user data
#'   ("election.users.csv"). If not included in file_name, the CSV
#'   extension will be added when writing file to disk.
#' @param file_name Path/file name where object(s) is to be saved.  If
#'   object includes both tweets and users data then provided
#'   file_name will be used as base for the two saved files.  For
#'   example, file_name = "election", would save files as
#'   "election.tweets.csv" and "election.users.csv".
#' @param prepend_ids Logical indicating whether to prepend an "x"
#'   before all Twitter IDs (for users, statuses, lists, etc.). It's
#'   recommended when saving to CSV as these values otherwise get
#'   treated as numeric and as a result the values are often less
#'   precise due to rounding or other class-related quirks. Defaults
#'   to true.
#' @param na Value to be used for missing (NA)s. Defaults to empty
#'   character, "".
#' @param fileEncoding Encoding to be used when saving to
#'   CSV. defaults to "UTF-8".
#' @export
save_as_csv <- function(x, file_name,
                        prepend_ids = TRUE,
                        na = "",
                        fileEncoding = "UTF-8") {
  if (missing(file_name)) {
    stop("must provide file_name.", call. = FALSE)
  }
  tweets_names <- c(
    "reply_to_status_id",
    "quote_status",
    "retweet_count",
    "is_retweet"
  )
  users_names <- c(
    "followers_count",
    "description",
    "statuses_count",
    "friends_count"
  )
  if (any(tweets_names %in% names(x))) {
    write_as_csv(
      x, modify_file_name(file_name, "tweets"),
      prepend_ids = prepend_ids,
      na = na,
      fileEncoding = fileEncoding
    )
    if ("users" %in% names(attributes(x))) {
      write_as_csv(
        users_data(x),
        modify_file_name(file_name, "users"),
        prepend_ids = prepend_ids,
        na = na,
        fileEncoding = fileEncoding
      )
    }
  } else if (any(users_names %in% names(x))) {
    write_as_csv(
      x, modify_file_name(file_name, "users"),
      prepend_ids = prepend_ids,
      na = na,
      fileEncoding = fileEncoding
    )
    if ("tweets" %in% names(attributes(x))) {
      write_as_csv(
        tweets_data(x),
        modify_file_name(file_name, "tweets"),
        prepend_ids = prepend_ids,
        na = na,
        fileEncoding = fileEncoding
      )
    }
  } else {
    write_as_csv(
      x, modify_file_name(file_name),
      prepend_ids = prepend_ids,
      na = na,
      fileEncoding = fileEncoding
    )
  }
}

modify_file_name <- function(file_name, ext = NULL) {
  stopifnot(is.character(file_name),
    length(file_name) == 1)
  file_name <- gsub(".csv$", "", file_name)
  if (is.null(ext)) {
    file_name <- paste0(file_name, ".csv")
  } else {
    file_name <- paste0(file_name, ".", ext, ".csv")
  }
  file_name
}

#' Saves as flattened CSV file of Twitter data.
#'
#' @param x Data frame with tweets and users data.
#' @param file_name Desired name(stem) to save files as (one save for
#'   tweets, one save for users).
#' @param prepend_ids Logical indicating whether to prepend an "x"
#'   before all Twitter IDs (for users, statuses, lists, etc.). It's
#'   recommended when saving to CSV as these values otherwise get
#'   treated as numeric and as a result the values are often less
#'   precise due to rounding or other class-related quirks. Defaults
#'   to true.
#' @param na Value to be used for missing (NA)s. Defaults to empty
#'   character, "".
#' @param fileEncoding Encoding to be used when saving to
#'   CSV. defaults to "UTF-8".
#' @return Saved CSV files in current working directory.
#' @importFrom utils write.csv
#' @export
write_as_csv <- function(x, file_name,
                         prepend_ids = TRUE,
                         na = "",
                         fileEncoding = "UTF-8") {
  stopifnot(is.data.frame(x))
  x <- flatten_rtweet(x)
  if (prepend_ids) {
    x <- prepend_ids(x)
  }
  write.csv(x, file_name, row.names = FALSE, na = na, fileEncoding = fileEncoding)
}

prepend_ids <- function(x) {
  ids <- grepl("\\_id$", names(x))
  x[ids] <- lapply(x[ids], x_ids)
  x
}

x_ids <- function(x) {
  x[!is.na(x)] <- paste0("x", x[!is.na(x)])
  x
}

unprepend_ids <- function(x) {
  ids <- grepl("\\_id$", names(x))
  x[ids] <- lapply(x[ids], unx_ids)
  x
}

unx_ids <- function(x) {
  gsub("^x", "", x)
}

#' Read comma separated value Twitter data.
#'
#' Reads Twitter data that was previously saved as a CSV file.
#'
#' @param file Name of CSV file.
#' @return A tbl data frame of Twitter data
#' @importFrom utils read.csv
#' @examples
#'
#' \dontrun{
#'
#' ## read in data.csv
#' rt <- read_twitter_csv("data.csv")
#'
#' }
#' @export
read_twitter_csv <- function(file) {
  x <- utils::read.csv(
    file = file,
    na.strings = "",
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    encoding = "UTF-8"
  )
  x <- unprepend_ids(x)
  tibble::as_tibble(x, validate = FALSE)
}

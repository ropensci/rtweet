#' save_as_csv
#'
#' @description Converts and saves data table generated from rtweet
#'   package as csv file(s).
#'
#' @param x Data table to be saved (tweets or user object)
#'   generated via rtweet function like \code{search_tweets}. If x
#'   is a list object containing both tweets and users data (which
#'   is currently the output for many of the rtweet functions),
#'   then a CSV file is created and saved for each object using the
#'   file_name provided as a base--e.g, if x is a list object from
#'   search_tweets with \code{file_name = "election"}, this
#'   function will save both the tweets data ("election.tweets.csv")
#'   and the user data ("election.users.csv"). If not included in
#'   file_name, the ".csv" extension will be added when writing file
#'   to disk.
#' @param file_name Path/file name where object(s) is to be saved.
#'   If object includes both tweets and users data then provided
#'   file_name will be used as base for the two saved files.
#'   For example, \code{file_name = "election"} would save files
#'   as "election.tweets.csv" and "election.users.csv".
#' @importFrom utils write.csv
#' @export
save_as_csv <- function(x, file_name) {
    if (missing(file_name)) {
        stop("must provide file_name.", call. = FALSE)
    }
    tweets_names <- c(
        "in_reply_to_status_id",
        "is_quote_status",
        "retweet_count",
        "is_retweet")
    users_names <- c(
        "followers_count",
        "description",
        "statuses count",
        "friends_count")
    if (any(tweets_names %in% names(x))) {
        write_as_csv(
            x, modify_file_name(file_name, "tweets"))

        if ("users" %in% names(attributes(x))) {
            write_as_csv(
                users_data(x),
                modify_file_name(file_name, "users"))
        }
    } else if (any(users_names %in% names(x))) {
        write_as_csv(
            x, modify_file_name(file_name, "users"))

        if ("tweets" %in% names(attributes(x))) {
            write_as_csv(
                tweets_data(x),
                modify_file_name(file_name, "tweets"))
        }

    } else {
        write_as_csv(x, modify_file_name(file_name))
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

write_as_csv <- function(x, file_name) {
    stopifnot(is.data.frame(x))
    write.csv(x, file_name, row.names = FALSE)
}

collapse_list <- function(x) {
    ## collapse recursive columns into strings
    vapply(x, function(x) paste(x, collapse = " "), character(1),
           USE.NAMES = FALSE)
}

clean_nas <- function(x) {
    ## replace NAs with blanks
    x[vapply(x, is.na, logical(1), USE.NAMES = FALSE)] <- ""
    x
}

is_list <- function(x) {
    unlist(apply(x[, ], 2, is.list), use.names = FALSE)
}

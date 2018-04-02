
save_as_csv2 <- function(x, file_name,
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



#' function to flatten IDs vars
#'
#' @param x Data object
#' @return Data with each observation of IDs represented by a single string,
#'   separating each ID with a space.
#' @keywords internal
#' @export
flatten_id <- function(x) UseMethod("flatten_id")

#' @keywords internal
#' @export
flatten_id.character <- function(x) {
  if (all(is.na(x))) return(x)
  x[!is.na(x)] <- paste0("x", x[!is.na(x)])
  x
}

#' @keywords internal
#' @export
flatten_id.list <- function(x) {
  flist <- function(x) {
    if (length(x) == 0 || (length(x) == 1L && is.na(x))) {
      return(NA_character_)
    }
    paste(paste0("x", x), collapse = " ")
  }
  na <- lengths(x) == 1 & vapply(x, function(i) is.na(i[1]), FUN.VALUE = logical(1))
  x[!na] <- lapply(x[!na], flist)
  unlist(x)
}

#' function to flatten data
#'
#' @param x Data
#' @return Flatten data in preparation to be saved as CSV.
#' @keywords internal
#' @export
flatten_var <- function(x) UseMethod("flatten_var")

#' @keywords internal
#' @export
flatten_var.default <- function(x) x

#' @keywords internal
#' @export
flatten_var.list <- function(x) {
  flist <- function(x) {
    if (length(x) == 0 || (length(x) == 1L && is.na(x))) {
      return(NA_character_)
    }
    paste(x, collapse = " ")
  }
  na <- lengths(x) == 1 & vapply(x, function(i) is.na(i[1]), FUN.VALUE = logical(1))
  if (sum(!na) > 0) {
    x[!na] <- lapply(x[!na], flist)
  }
  unlist(x)
}

#' Save Twitter data as a comma separated value file.
#'
#' Saves tweets and users data as CSV files.
#'
#' @param x Data frame returned by rtweet function like
#'   \code{\link{search_tweets}} to be saved as CSV file.
#' @param file_name Path/file name where data file is to be saved. If a ".csv"
#'   extension isn't used, it'll be appending onto the supplied file name.
#' @details Flattens list columns by pasting them into a single string for each
#'   observations. For example, a tweet that mentions four other users, for the
#'   mentions_user_id variable, it will include the four user IDs separated by
#'   a space.
#'
#' It should also be noted that the save_as_csv function prepends all Twitter
#'   IDs (user ID and status ID) with an "x" in order to ensure spreadsheet
#'   programs (cough, excel, cough) don't treat the values as numeric and,
#'   consequently, round the numbers, preventing future identification. For this
#'   reason, it's best to read in data saved this way using the read_twitter_csv
#'   function or taking care to remove the prepended "x"s before using the
#'   captured ID values.
#' @export
save_as_csv <- function(x, file_name) {
  ## validate inputs
  stopifnot(is.data.frame(x), is.character(file_name), length(file_name) == 1L)
  if (!grepl("\\.csv$", file_name)) {
    file_name <- paste0(file_name, ".csv")
  }
  ## prep data
  x <- prep_for_csv(x)
  ## save as CSV file
  write.csv(x, file_name, row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

#' Returns flattened and ID-prepended data frame, as it's done internally via
#' \code{save_as_csv}
#'
#' @param x Data frame (or tibble) returned by rtweet functions.
#'
#' @export
#' @rdname prep_for_csv
prep_for_csv <- function(x) {
  ## join users and tweets
  x <- join_rtweet(x)
  ## collapse recursive columns into single strings (space separated)
  x[grep("_id", names(x))] <- lapply(x[grep("_id", names(x))], flatten_id)
  x[1:ncol(x)] <- lapply(x, flatten_var)
  x
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

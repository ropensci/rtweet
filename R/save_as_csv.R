#' Save Twitter data as a comma separated value file.
#'
#' Saves as flattened CSV file of Twitter data.
#'
#' @param x Data frame returned by an rtweet function.
#' @param file_name Desired name to save file as. If `file_name` does not
#'   include the extension ".csv" it will be added automatically.
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
#' @family datafiles
#' @export
write_as_csv <- function(x, file_name,
                         prepend_ids = TRUE,
                         na = "",
                         fileEncoding = "UTF-8") {
  ## to minimize rounding
  op <- options()
  on.exit(options(op))
  options(scipen = 14, digits = 22)

  ## validate inputs
  stopifnot(is.data.frame(x), is.character(file_name), length(file_name) == 1L)
  if (!grepl("\\.csv$", file_name)) {
    file_name <- paste0(file_name, ".csv")
  }
  ## flatten data
  x <- flatten(x)
  if (prepend_ids) {
    x <- prepend_ids(x)
  }
  utils::write.csv(x, file_name, row.names = FALSE, na = na,
    fileEncoding = fileEncoding)
}

#' @export
#' @rdname write_as_csv
#' @family datafiles
save_as_csv <- function(x, file_name,
                        prepend_ids = TRUE,
                        na = "",
                        fileEncoding = "UTF-8") {
  write_as_csv(x, file_name, prepend_ids, na, fileEncoding)
}

#' flatten/unflatten data frame
#'
#' Converts list columns that containing all atomic elements into
#' character vectors and vice versa (for appropriate named variables
#' according to the rtweet package)
#'
#' @param x Data frame with list columns or converted-to-character (flattened)
#'   columns.
#' @return If flattened, then data frame where non-recursive list
#'   columns---that is, list columns that contain only atomic, or non-list,
#'   elements---have been converted to character vectors. If unflattened,
#'   this function splits on spaces columns originally returned as lists
#'   by functions in rtweet package. See details for more information.
#'
#' @details If recursive list columns are contained within the data frame,
#'   relevant columns will still be converted to atomic types but output
#'   will also be accompanied with a warning message.
#'
#' `flatten` flattens list columns by pasting them into a single string for
#'   each observations. For example, a tweet that mentions four other users,
#'   for the mentions_user_id variable, it will include the four user IDs
#'   separated by a space.
#'
#' `unflatten`` splits on spaces to convert into list columns any
#'   columns with the following names: hashtags, symbols, urls_url,
#'   urls_t.co, urls_expanded_url, media_url, media_t.co,
#'   media_expanded_url, media_type, ext_media_url, ext_media_t.co,
#'   ext_media_expanded_url, mentions_user_id, mentions_screen_name,
#'   geo_coords, coords_coords, bbox_coords, mentions_screen_name
#' @export
#' @rdname flatten
#' @family datafiles
flatten <- function(x) {
  stopifnot(is.data.frame(x))
  lst <- which(vapply(x, is.list,
    FUN.VALUE = logical(1), USE.NAMES = FALSE))
  atom <- which(vapply(x[lst], function(.) all(
    vapply(., is.atomic, FUN.VALUE = logical(1), USE.NAMES = FALSE)
  ), FUN.VALUE = logical(1), USE.NAMES = FALSE))
  la <- lst[atom]
  x[la] <- lapply(x[la], function(a)
    vapply(a, function(b)
      ifelse(length(b) == 0 | (length(b) == 1 && is.na(b)), "",
        paste(b, collapse = " ")),
      FUN.VALUE = character(1), USE.NAMES = FALSE))
  x[la] <- lapply(x[la], function(.) ifelse(. == "", NA, .))
  if (any(vapply(x, is.recursive,
    FUN.VALUE = logical(1), USE.NAMES = FALSE))) {
    warning("data frame still contains recursive columns!")
  }
  x
}

#' @export
#' @rdname flatten
#' @family datafiles
unflatten <- function(x) {
  yes_coords <- c("geo_coords", "coords_coords", "bbox_coords")
  rec_cols <- c("hashtags", "symbols",
    "urls_url", "urls_t.co", "urls_expanded_url", "media_url",
    "media_t.co", "media_expanded_url", "media_type",
    "ext_media_url", "ext_media_t.co", "ext_media_expanded_url",
    "mentions_user_id", "mentions_screen_name", "mentions_screen_name",
    yes_coords)
  rc <- names(x) %in% rec_cols
  lg <- vapply(x[rc], is.logical, FUN.VALUE = logical(1))
  if (any(lg)) {
    kp <- names(x[rc])
    rc <- kp[!lg]
  }
  x[rc] <- lapply(x[rc], strsplit, " ")
  rc <- names(x) %in% rec_cols[!rec_cols %in% yes_coords]
  x[rc] <- lapply(x[rc], function(.) {
    .[lengths(.) == 0] <- NA_character_
    .})
  rc <- names(x) %in% yes_coords
  x[rc] <- lapply(x[rc], function(.) {
    . <- lapply(., function(y) suppressWarnings(as.numeric(y)))
    .[lengths(.) == 0] <- NA_real_
    .})
  x
}

prepend_ids <- function(x) {
  ids <- grepl("\\_id$", names(x))
  x[ids] <- lapply(x[ids], x_ids)
  x
}


x_ids <- function(x) {
  if (is.recursive(x)) {
    x <- lapply(x, function(.)
      ifelse(length(.) == 0 || (length(.) == 1 && is.na(.)),
        list(NA_character_), list(paste0("x", .))))
    x <- lapply(x, unlist, recursive = FALSE)
  } else {
    x[x == ""] <- NA_character_
    x[!is.na(x)] <- paste0("x", x[!is.na(x)])
    x[!is.na(x)] <- gsub(" ", " x", x[!is.na(x)])
  }
  x
}

unprepend_ids <- function(x) {
  ids <- grepl("\\_id$", names(x))
  x[ids] <- lapply(x[ids], unx_ids)
  x
}

unx_ids <- function(x) {
  if (is.recursive(x)) {
    x <- lapply(x, function(.)
      ifelse(length(.) == 0 || (length(.) == 1 && is.na(.)),
        list(NA_character_), list(gsub("x", "", .))))
    x <- lapply(x, unlist, recursive = FALSE)
  } else {
    x <- gsub("x", "", x)
  }
  x
}

#' Read comma separated value Twitter data.
#'
#' Reads Twitter data that was previously saved as a CSV file.
#'
#' @param file Name of CSV file.
#' @param unflatten Logical indicating whether to unflatten (separate hasthags
#'   and mentions columns on space, converting characters to lists), defaults
#'   to FALSE.
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
#' @family datafiles
#' @export
read_twitter_csv <- function(file, unflatten = FALSE) {
  x <- utils::read.csv(
    file = file,
    na.strings = "",
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    encoding = "UTF-8",
    numerals = "no.loss"
  )
  x <- unprepend_ids(x)
  if (unflatten) {
    x <- unflatten(x)
  }
  as_tbl(x)
}

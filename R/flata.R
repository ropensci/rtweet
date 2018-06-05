
#' flatten/unflatten data frame
#'
#' Converts list columns that containing all atomic elements into
#' character vectors and vice versa (for appropriate named variables
#' according to the rtweet package)
#'
#' @param x Data frame with list columns or converted-to-character (flata'd)
#'   columns.
#' @return If flattened, then data frame where non-recursive list
#'   columns---that is, list columns that contain only atomic, or non-list,
#'   elements---have been converted to character vectors. If deflattened,
#'   this function splits on spaces columsn originally returned as lists
#'   by functions in rtweett package. See details for more information.
#'
#' @details If recursive list columns are contained within the data frame,
#'   convertion of relevant columns will still occur but output will also
#'   be accompanied with a warning message.
#'
#' \code{deflata} splits on spaces to convert into list columns any
#'   columns with the following names: hashtags, symbols, urls_url,
#'   urls_t.co, urls_expanded_url, media_url, media_t.co,
#'   media_expanded_url, media_type, ext_media_url, ext_media_t.co,
#'   ext_media_expanded_url, mentions_user_id, mentions_screen_name,
#'   geo_coords, coords_coords, bbox_coords, mentions_screen_name
#' @export
#' @rdname flata
flata <- function(x) {
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
  if (any(vapply(x, is.recursive,
    FUN.VALUE = logical(1), USE.NAMES = FALSE))) {
    warning("data frame still contains recursive columns!")
  }
  x
}

#' @inheritParams flata
#' @export
#' @rdname flata
deflata <- function(x) {
  yes_coords <- c("geo_coords", "coords_coords", "bbox_coords")
  rec_cols <- c("hashtags", "symbols",
    "urls_url", "urls_t.co", "urls_expanded_url", "media_url",
    "media_t.co", "media_expanded_url", "media_type",
    "ext_media_url", "ext_media_t.co", "ext_media_expanded_url",
    "mentions_user_id", "mentions_screen_name", "mentions_screen_name",
    yes_coords)
  rc <- names(x) %in% rec_cols
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

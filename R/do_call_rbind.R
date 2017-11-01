
#' Binds list of data frames while preserving attribute (tweets or users) data.
#'
#' Row bind lists of tweets/users data whilst also preserving and binding
#' users/tweets attribute data.
#'
#' @param x List of parsed tweets data or users data, each of which
#'   presumably contains an attribute of the other (i.e., users data
#'   contains tweets attribute; tweets data contains users attribute).
#' @return A single merged (by row) data frame (tbl) of tweets or
#'   users data that also contains as an attribute a merged (by row)
#'   data frame (tbl) of its counterpart, making it accessible via the
#'   \code{\link{users_data}} or \code{\link{tweets_data}} extractor
#'   functions.
#' @family parsing
#' @examples
#'
#' \dontrun{
#'
#' ## lapply through three different search queries
#' lrt <- lapply(
#'   c("rstats OR tidyverse", "data science", "python"),
#'   search_tweets,
#'   n = 5000
#' )
#'
#' ## convert list object into single parsed data rame
#' rt <- do_call_rbind(lrt)
#'
#' ## preview tweets data
#' rt
#'
#' ## preview users data
#' users_data(rt)
#'
#' }
#'
#' @export
do_call_rbind <- function(x) {
  att_df <- lapply(x, extract_att_data)
  att_df <- do.call("rbind", att_df)
  if (all(c("text", "hashtags") %in% names(att_df))) {
    att <- "tweets"
  } else {
    att <- "users"
  }
  x <- do.call("rbind", x)
  attr(x, att) <- att_df
  x
}

extract_att_data <- function(df) {
  if (has_name_(attributes(df), "users") || has_name_(attributes(df), "usr")) {
    users_data(df)
  } else if (has_name_(attributes(df), "tweets") || has_name_(attributes(df), "tw")) {
    tweets_data(df)
  } else {
    data.frame()
  }
}

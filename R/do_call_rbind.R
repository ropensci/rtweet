
#' Binds list of data frames while preserving attribute (tweets or users) data.
#'
#' Row bind lists of tweets/users data whilst also preserving and binding
#' users/tweets attribute data. `r lifecycle::badge("deprecated")`
#'
#' @param x List of parsed tweets data or users data, each of which
#'   presumably contains an attribute of the other (i.e., users data
#'   contains tweets attribute; tweets data contains users attribute).
#' @return A single merged (by row) data frame (tbl) of tweets or
#'   users data that also contains as an attribute a merged (by row)
#'   data frame (tbl) of its counterpart, making it accessible via the
#'   [users_data()] or [tweets_data()] extractor
#'   functions.
#' @family parsing
#' @seealso [`rtweet-deprecated`]
#' @export
do_call_rbind <- function(x) {
  do.call(rbind, x)
}

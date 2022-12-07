#' Extract the streaming rules
#'
#' Provides the information about the rules
#' @param x An object returned by `stream_*_rule`
#' @param ... Other arguments currently ignored.
#' @seealso `stream_add_rule()` and `stream_rm_rule()`.
#' @export
rules <- function(x, ...) {
  UseMethod("rules")
}

#' @export
rules.rules <- function(x, ...) {
  attr(x, "rules")
}

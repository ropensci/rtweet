#' Collections API
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `get_collections()` and `lookup_collections()` have been deprecated
#' since the underlying Twitter API has been deprecated.
#'
#' @keywords internal
#' @export
lookup_collections <- function(id, n = 200,
                               parse = TRUE,
                               token = NULL,
                               ...) {
  lifecycle::deprecate_stop("1.0.0", function_call())
}

#' @export
#' @rdname lookup_collections
get_collections <- function(user = NULL,
                            status_id = NULL,
                            n = 200,
                            cursor = NULL,
                            parse = TRUE,
                            token = NULL) {

  lifecycle::deprecate_stop("1.0.0", function_call())
}

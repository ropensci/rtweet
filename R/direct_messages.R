#' Get direct messages sent to and received by the authenticating user from the
#' past 30 days
#'
#' Returns all Direct Message events (both sent and received) within the last 30
#' days. Sorted in reverse-chronological order. Includes detailed information
#' about the sender and recipient.
#'
#' @inheritParams TWIT_paginate_cursor
#' @inheritParams stream
#' @param next_cursor `r lifecycle::badge("deprecated")` Use `cursor` instead.
#' @return A list with one element for each page of results.
#' @examples
#' \dontrun{
#'
#' ## get my direct messages
#' dms <- direct_messages()
#'
#' ## inspect data structure
#' str(dms)
#'
#' }
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/direct-messages/sending-and-receiving/api-reference/list-events>
direct_messages <- function(n = 50,
                            cursor = NULL,
                            next_cursor = NULL,
                            parse = TRUE,
                            token = NULL,
                            retryonratelimit = NULL,
                            verbose = TRUE) {

  if (!is.null(next_cursor)) {
    lifecycle::deprecate_warn("1.0.0",
      "direct_messages(next_cursor)",
      "direct_messages(cursor)"
    )
    cursor <- next_cursor
  }

  TWIT_paginate_cursor(token, "/1.1/direct_messages/events/list", list(),
    n = n,
    cursor = cursor,
    retryonratelimit = retryonratelimit,
    verbose = verbose,
    page_size = if (n >= 50) 50 else n,
    get_id = function(x) x$events$id
  )
}

#' (DEPRECATED) Get the most recent direct messages sent to the authenticating user.
#'
#' Retrieves up to 200 of the most recently received direct messages
#' by the authenticating (home) user. This function requires access
#' token with read, write, and direct messages access.
#'
#' @return Return object converted to nested list. If status code of
#'   response object is not 200, the response object is returned
#'   directly.
#' @examples
#'
#' \dontrun{
#'
#' ## get my direct messages
#' dms <- direct_messages_received()
#'
#' ## inspect data structure
#' str(dms)
#'
#' ## get direct messages I've sent
#' sdms <- direct_messages_sent()
#'
#' ## inspect data structure
#' str(dms)
#'
#' }
#'
#' @details Includes detailed information about the sender and
#'   recipient user. You can request up to 200 direct messages per
#'   call, and only the most recent 200 direct messages will be available using
#'   this endpoint.
#' @export
#' @keywords internal
direct_messages_received <- function(since_id = NULL,
                                     max_id = NULL,
                                     n = 200,
                                     parse = TRUE,
                                     token = NULL) {
  abort("The endpoint for `direct_messages_received()` no longer exists. ",
    "Please use `direct_messages()` instead.")
}

#' @export
#' @rdname direct_messages_received
direct_messages_sent <- function(since_id = NULL,
                                 max_id = NULL,
                                 n = 200,
                                 parse = TRUE,
                                 token = NULL) {
  abort("The endpoint for `direct_messages_received()` no longer exists. ",
    "Please use `direct_messages()` instead.")
}

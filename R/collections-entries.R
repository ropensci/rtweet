#' GET collections/entries¶
#'
#' Retrieve the identified Collection, presented as a list of the
#'   Tweets curated within. The response structure of this method
#'   differs significantly from timelines you may be used to
#'   working with in the Twitter REST API. To navigate a
#'   Collection, use the position object of a response, which includes
#'   attributes for max_position, min_position, and was_truncated.
#'   was_truncated indicates whether additional Tweets exist in
#'   the collection outside of the range of the current request.
#'   To retrieve Tweets further back in time, use the value of
#'   min_position found in the current response as the max_position
#'   parameter in your next call to this endpoint.

#' @param id required The identifier of the Collection to return results for. "539487832448843776"
#' @param count optional Specifies the maximum number of results to include in the response. Specify a
#'   count between 1 and 200. A next_cursor value will be provided in the response
#'   if additional results are available.
#' @param max_position optional Returns results with a position value less than or equal to the specified
#'   position.
#' @param min_position optional Returns results with a position greater than the specified position.
#' @return data
f <- function(id, count = 200, max_position = NULL, min_position = NULL) {
  1L
}



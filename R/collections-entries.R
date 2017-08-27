#' GET collections/entries
#'
#' Retrieve the identified Collection, presented as a list of the Tweets curated
#' within. The response structure of this method differs significantly from
#' timelines you may be used to working with in the Twitter REST API. To
#' navigate a Collection, use the position object of a response, which includes
#' attributes for max_position, min_position, and was_truncated. was_truncated
#' indicates whether additional Tweets exist in the collection outside of the
#' range of the current request. To retrieve Tweets further back in time, use
#' the value of min_position found in the current response as the max_position
#' parameter in your next call to this endpoint.
#'
#' @param id required The identifier of the Collection to return results for
#'   e.g., "custom-539487832448843776"
#' @param count optional Specifies the maximum number of results to include in
#'   the response. Specify count between 1 and 200. A next_cursor value will be
#'   provided in the response if additional results are available.
#' @param max_position optional Returns results with a position value less than
#'   or equal to the specified position.
#' @param min_position optional Returns results with a position greater than the
#'   specified position.
#' @param parse Logical indicating whether to convert response object into
#'   nested list. Defaults to true.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions on how
#'   to create tokens and setup an environment variable in the tokens vignette
#'   (in r, send \code{?tokens} to console).
#' @return Return object converted to nested list. If status code of response
#'   object is not 200, the response object is returned directly.
#' @export
get_collections <- function(id, count = 200,
                            max_position = NULL,
                            min_position = NULL,
                            parse = TRUE,
                            token = NULL) {
  stopifnot(is.character(id), is_n(count))
  query <- "collections/entries"
  params <- list(
    id = id,
    count = count,
    max_position = max_position,
    min_position = min_position
  )
  url <- make_url(query = query, param = params)
  token <- check_token(token)
  r <- httr::GET(url, token)
  httr::warn_for_status(r)
  if (r$status_code == 200L && parse) {
    r <- jsonlite::fromJSON(httr::content(r, "text", "UTF-8"))
  }
  r
}

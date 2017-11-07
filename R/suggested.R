#' Get user [account] suggestions for authenticating user
#'
#' Returns Twitter's list of suggested user categories.
#'
#' @inheritParams suggested_users
#' @return List of recommended categories which can be passed along as
#'   the "slug" parameter in \code{\link{suggested_users}}
#' @export
#' @rdname suggested_users
suggested_slugs <- function(lang = NULL, token = NULL) {
  query <- "users/suggestions"
  token <- check_token(token, query)
  params <- list(lang = lang)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  from_js(r)
}


#' Returns users from a specific, suggested category
#'
#' @param slug required The short name of list or a category
#' @param lang optional Restricts the suggested categories to the
#'   requested language. The language must be specified by the
#'   appropriate two letter ISO 639-1 representation.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return Recommended users
#' @export
#' @rdname suggested_users
#' @examples
#'
#' \dontrun{
#'
#' ## get slugs
#' slugs <- suggested_slugs()
#'
#' ## use slugs to get suggested users
#' suggested_users(slugs$slug[1])
#'
#' }
#'
#' @rdname suggested
suggested_users <- function(slug, lang = NULL, token = NULL) {
  if (missing(slug)) {
    stop("Must provide slug. See: suggested_slugs for list of possible values",
         call. = FALSE)
  }
  query <- "users/suggestions/:slug"
  token <- check_token(token, query)
  params <- list(slug = slug, lang = lang)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  from_js(r)
}

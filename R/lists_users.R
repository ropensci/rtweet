#' Get all lists a specified user subscribes to, including their own.
#'
#' @param user The ID of the user or screen name for whom to return results.
#'   Helpful for disambiguating when a valid user ID is also a valid screen name.
#' @param reverse optional Set this to true if you would like owned lists
#'   to be returned first. See description above for information on
#'   how this parameter works.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param parse Logical indicating whether to convert the response object into
#'   an R list. Defaults to TRUE.
#' @return data
#' @examples
#' \dontrun{
#'
#' ## get lists subsribed to by Nate Silver
#' lists_users("NateSilver538")
#'
#' }
#'
#' @family lists
#' @export
lists_users <- function(user, reverse = FALSE, token = NULL, parse = TRUE) {
  if (missing(user)) {
    user <- home_user()
  }
  query <- "lists/list"
  params <- list(
    user = user,
    reverse = reverse
  )
  names(params)[1] <- .id_type(user)
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  if (parse) {
    r <- from_js(r)
    r <- as_lists_users(r)
    r <- as.data.frame(r)
  }
  r
}

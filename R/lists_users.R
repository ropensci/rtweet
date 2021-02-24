#' Get all lists a specified user subscribes to, including their own.
#'
#' @param user The ID of the user or screen name for whom to return results.
#' @param reverse optional Set this to true if you would like owned lists
#'   to be returned first. See description above for information on
#'   how this parameter works.
#' @inheritParams lookup_users
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
lists_users <- function(user = NULL, reverse = FALSE, token = NULL, parse = TRUE) {

  query <- "lists/list"
  params <- list(
    user = user,
    reverse = reverse
  )
  names(params)[1] <- .id_type(user)
  token <- check_token(token)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  if (parse) {
    r <- from_js(r)
    r <- as_lists_users(r)
    r <- as.data.frame(r)
  }
  r
}

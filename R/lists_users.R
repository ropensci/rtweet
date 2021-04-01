#' Get all lists a specified user subscribes to, including their own.
#'
#' @inheritParams get_timeline
#' @param reverse optional Set this to true if you would like owned lists
#'   to be returned first. See description above for information on
#'   how this parameter works.
#' @return data
#' @examples
#' if (auth_has_default()) {
#'   lists_users("NateSilver538")
#' }
#' @family lists
#' @export
lists_users <- function(user = NULL, reverse = FALSE, token = NULL, parse = TRUE) {
  params <- list(
    reverse = reverse
  )
  params[[user_type(user)]] <- user

  r <- TWIT_get(token, "/1.1/lists/list", params, parse = parse)
  if (parse) {
    r <- as_lists_users(r)
    r <- as.data.frame(r)
  }
  r
}

#' as screen name
#'
#' Forces user of type screen name.
#'
#' @param x Vector of users (screen names).
#' @return Vector of class screen_name.
#' @examples
#' \dontrun{
#' get_friends(as_screen_name("1234"))
#' as_screen_name(c("kearneymw", "1234"))
#' }
#' @export
as_screen_name <- function(x) {
  stopifnot(is.atomic(x))
  x <- as.character(x)
  class(x) <- c("screen_name", class(x))
  x
}

`[.screen_name` <- function(x, i) {
  x <- as.character(x)
  x <- x[i]
  as_screen_name(x)
}

print.screen_name <- function(x) {
  cat("Twitter user type: screen name\nUsers:", fill = TRUE)
  print(as.character(x))
}

is_screen_name <- function(x) {
  inherits(x, "screen_name")
}


#' as user ID
#'
#' Forces user of type user ID.
#'
#' @param x Vector of users (user IDs).
#' @return Vector of class user_Id.
#' @export
as_user_id <- function(x) {
  stopifnot(is.atomic(x))
  x <- as.character(x)
  class(x) <- c("user_id", class(x))
  x
}

`[.user_id` <- function(x, i) {
  x <- as.character(x)
  x <- x[i]
  as_screen_name(x)
}

print.user_id <- function(x) {
  cat("Twitter user type: user id\nUsers:", fill = TRUE)
  print(as.character(x))
}

is_user_id <- function(x) {
  inherits(x, "user_id")
}

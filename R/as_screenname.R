#' Coerces user identifier(s) to be evaluated as a screen name(s).
#'
#' @param x A vector consisting of one or more Twitter user
#'   identifiers (i.e., screen names or user IDs).
#' @return A vector of class screen_name or class user_id
#' @details Default rtweet function behaviors will treat "1234" as a
#'   user ID, but the inverse (i.e., treating "2973406683" as a screen
#'   name) should rarely be an issue. However, in those cases, users
#'   may need to mix both screen names and user IDs. To do so, make
#'   sure to combine them as a list (and not a character vector, which
#'   will override conflicting user identifier classes). See examples
#'   code for example of mixing user IDs with screen names. Note: this
#'   only works with certain functions, e.g., get_friends,
#'   get_followers.
#' @examples
#' \dontrun{
#' ## get friends list for user with the handle "1234"
#' get_friends(as_screenname("1234"))
#'
#' ## as_screenname coerces all elements to class "screen_name"
#' sns <- as_screenname(c("kearneymw", "1234", "jack"))
#' class(sns)
#'
#' ## print will display user class type
#' sns
#'
#' ## BAD: combine user id and screen name using c()
#' users <- c(as_userid("2973406683"), as_screenname("1234"))
#' class(users)
#'
#' ## GOOD: combine user id and screen name using list()
#' users <- list(as_userid("2973406683"), as_screenname("1234"))
#' users
#'
#' ## get friend networks for each user
#' get_friends(users)
#'
#' }
#' @family users
#' @rdname as_screenname
#' @export
as_screenname <- function(x) {
  stopifnot(is.atomic(x))
  x <- as.character(x)
  set_class(x, "screen_name")
}

set_class <- function(x, value) `class<-`(x, value)

`[.screen_name` <- function(x, i) {
  x <- as.character(x)
  x <- x[i]
  as_screenname(x)
}

#' @export
print.screen_name <- function(x, ...) {
  cat("Twitter user type: screen name\nUsers:", fill = TRUE)
  x <- as.character(x)
  print(x, ...)
}

is_screen_name <- function(x) {
  inherits(x, "screen_name")
}


#' @inheritParams as_screenname
#' @rdname as_screenname
#' @export
as_userid <- function(x) {
  stopifnot(is.atomic(x))
  x <- as.character(x)
  set_class(x, "user_id")
}

`[.user_id` <- function(x, i) {
  x <- as.character(x)
  x <- x[i]
  as_userid(x)
}

#' @export
print.user_id <- function(x, ...) {
  cat("Twitter user type: user id\nUsers:", fill = TRUE)
  x <- as.character(x)
  print(x, ...)
}

is_user_id <- function(x) {
  inherits(x, "user_id")
}

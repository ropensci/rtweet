#' Mark a user id as a screen name
#' 
#' @description 
#' There are two ways to identify a Twitter user: a screen name (e.g.
#' "justinbieber") or a user identifier (e.g. "27260086"). User identifiers
#' look like regular numbers, but are actually 64-bit integers which can't be 
#' stored in R's numeric vectors. For this reason, rtweet always returns ids as 
#' strings.
#' 
#' Unfortunately this introduces an ambiguity, because user names can 
#' also consist solely of numbers (e.g. "123456") so it's not obvious whether
#' a string consisting only of numbers is a screen name or a user id. By 
#' default, rtweet will assume its a user id, so if you have a screen name
#' that consists only of numbers, you'll need to use `as_screenname()` to
#' tell rtweet that it's actually a screen name.
#' 
#' Note that in general, you are best off using user ids; screen names are
#' not static and may change over longer periods of time. 
#' 
#' @param x A character vector of Twitter screen names.
#' @examples
#' if (auth_has_default()) {
#' # Look up user with id 
#' lookup_users("25594077") 
#' 
#' # Look up user with name 25594077
#' lookup_users(as_screenname("123456"))
#' }
#' @family users
#' @rdname as_screenname
#' @export
as_screenname <- function(x) {
  stopifnot(is.character(x))
  structure(x, class = "rtweet_screen_name")
}

#' @export
`[.rtweet_screen_name` <- function(x, i) {
  as_screenname(NextMethod())
}

#' @export
print.rtweet_screen_name <- function(x, ...) {
  cat("<rwteet_screen_name>\n")
  class(x) <- NULL
  print(x, ...)
  invisible(x)
}

#' @rdname as_screenname
#' @export
#' @usage NULL
as_userid <- function(x) {
  lifecycle::deprecate_warn("1.0.0", "as_userid()")
  x
}

user_type <- function(x, arg_name = "user") {
  if (is.numeric(x)) {
    "user_id"
  } else if (is.character(x)) {
    if (inherits(x, "rtweet_screen_name")) {
      # needed for purely numeric screen names
      "screen_name"
    } else if (all(grepl("^[0-9]+$", x))) {
      "user_id"
    } else {
      "screen_name"
    }
  } else {
    stop("`", arg_name, "` must be a screen name or user id", call. = FALSE)
  }
}

is_int64 <- function(x) {
  all(!is.na(bit64::as.integer64(x)))
}

api_screen_name <- function(token = NULL) {
  params <- list(
    include_entities = FALSE,
    skip_status = TRUE,
    include_email = FALSE
  )
  r <- TWIT_get(token, "/1.1/account/verify_credentials", params)
  r$screen_name
}

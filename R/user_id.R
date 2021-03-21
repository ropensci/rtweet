#' Mark a user id as a screen name
#' 
#' @description 
#' There are two ways to identify a twitter user: a screen name (e.g.
#' "justinbieber") or a user identifier (e.g. "27260086"). User identifiers
#' are 64-bit integers which can't be represented natively by any base R data
#' structures, so in most cases rtweet returns them as strings. This introduces
#' an ambiguity, because user names can also consist solely of numbers 
#' (e.g. "123456"). You can resolve this ambiguity by using `as_screenname()` 
#' to a character vector a screen name.
#' 
#' In general, you are best off using user ids where possible; screen names are
#' not static and may change. 
#' 
#' @param x A character vector of twitter screen names.
#' @examples
#' \dontrun{
#' # Look up user with id 123456 (screen name harperreed)
#' lookup_users("123456") 
#' 
#' # Look up user with name 123456
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

api_screen_name <- function(token = NULL) {
  params <- list(
    include_entities = FALSE,
    skip_status = TRUE,
    include_email = FALSE
  )
  r <- TWIT_get(token, "/1.1/account/verify_credentials", params)
  r$screen_name
}

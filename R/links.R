#' Create the links
#'
#' create the links from the rtweet data present.
#' Depending on the object type it returns either users links,  tweet links or rules links.
#' @param x An object of the rtweet package.
#' @param ... Other arguments currently unused.
#' @export
links <- function(x, ...) {
  UseMethod("links")
}

#' @export
links.default <- function(x, ...) {
  out <- x[["id_str"]]
  if (is.null(out)) {
    stop("links are not present. Are you sure this is a rtweet object?",
         call. = FALSE)
  }
  paste0("https://twitter.com/", out)
}


#' @export
links.rules <- function(x, ...) {
  abort("rules are not provided in a website.")
}

#' @export
links.users <- function(x, ...) {
  paste0("https://twitter.com/", x$screen_name)
}

#' @export
links.tweets <- function(x, ...) {
  u <- users_data(x)
  paste0("https://twitter.com/", u$screen_name, "/status/", x$id_str)
}

#' @export
links.followers <- function(x, ...) {
  paste0("https://twitter.com/", x$to_id)
}

#' @export
links.friends <- function(x, ...) {
  paste0("https://twitter.com/", x$from_id)
}

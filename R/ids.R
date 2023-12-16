#' Extract the ids
#'
#' Extract the ids of the rtweet data if present.
#' Depending on the object type it returns either users ids,  tweet ids or rules ids.
#' @param x An object of the rtweet package.
#' @param ... Other arguments currently unused.
#' @export
ids <- function(x, ...) {
  UseMethod("ids")
}

#' @export
ids.default <- function(x, ...) {
  out <- x[["id_str"]]
  if (is.null(out)) {
    stop("Ids are not present. Are you sure this is a rtweet object?",
         call = caller_call())
  }
  out
}


#' @export
ids.rules <- function(x, ...) {
  rules(x)$id
}

#' @export
ids.users <- function(x, ...) {
  x$id_str
}

#' @export
ids.tweets <- function(x, ...) {
  x$id_str
}

#' @export
ids.followers <- function(x, ...) {
  x$from_id
}

#' @export
ids.friends <- function(x, ...) {
  x$to_id
}

#' @export
ids.page <- function(x, ...) {
  x$id
}

#' @export
ids.post_tweet <- function(x, ...) {
  if (httr::status_code(x) != 200L) {
    abort("Your message has not been posted!", call = caller_call())
  }
  cpt <- httr::content(x)
  cpt$id_str
}

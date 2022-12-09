#' Extract the ids
#'
#' Extract the ids of the rtweet data if present.
#' Depending on the object type it returns either users ids,  tweet ids or rules ids.
#' @param x An object of the rtweet package.
#' @param ... Other arguments currently unused.
#' @export
#' @examples
#' if (auth_has_default()) {
#'   users <- lookup_users(c("twitter", "rladiesglobal", "_R_Foundation"))
#'   ids(users)
#'   followers <- get_followers("_R_Foundation")
#'   head(ids(followers))
#'   friends <- get_friends("_R_Foundation")
#'   head(ids(friends))
#' }
ids <- function(x, ...) {
  UseMethod("ids")
}

#' @export
ids.default <- function(x, ...) {
  out <- x[["id_str"]]
  if (is.null(out)) {
    stop("Ids are not present. Are you sure this is a rtweet object?",
         call. = FALSE)
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
ids.post_tweet <- function(x, ...) {
  if (httr::status_code(x) != 200L) {
    stop("Your message has not been posted!", call. = FALSE)
  }
  cpt <- httr::content(x)
  cpt$id_str
}

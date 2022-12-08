#' Extract methods
#'
#' Extract entities of the tweets.
#' @param x A tweets object of the rtweet package.
#' @param ... Other arguments currently unused.
#' @return Some information about those entities and the tweet id it cames from.
#' @name helpers
#' @examples
#' if (auth_has_default()) {
#'   statuses <- c(
#'     "567053242429734913",
#'     "266031293945503744",
#'     "440322224407314432"
#'   )
#'
#'   ## lookup tweets data for given statuses
#'   tw <- lookup_tweets(statuses)
#'   urls(tw)
#'
#' }
NULL



entity <- function(x, name = "urls") {
  ids <- ids(x)
  ent <- lapply(x$entities, function(x) {x[[name]]})
  n_ent <- vapply(ent, NROW, numeric(1L))
  ids <- rep(ids, n_ent)
  ent <- do.call(rbind, ent)
  cbind.data.frame(id_str = ids, ent)
}

#' @export
#' @rdname helpers
urls <- function(x, ...) {
  UseMethod("urls")
}

#' @export
urls.tweets <- function(x, ...) {
  out <- entity(x, "urls")
  out[, c("url", "expanded_url", "display_url")]
}

#' @export
#' @rdname helpers
hashtags <- function(x, ...) {
  UseMethod("hashtags")
}

#' @export
hashtags.tweets <- function(x, ...) {
  out <- entity(x, "hashtags")
  out[, c("id_str", "text")]
}

#' @export
#' @rdname helpers
symbols <- function(x, ...) {
  UseMethod("symbols")
}

#' @export
symbols.tweets <- function(x, ...) {
  out <- entity(x, "symbols")
  out[, c("id_str", "text")]
}

#' @export
#' @rdname helpers
user_mentions <- function(x, ...) {
  UseMethod("user_mentions")
}

#' @export
user_mentions.tweets <- function(x, ...) {
  out <- entity(x, "user_mentions")
  out[, c("id_str", "screen_name", "name", "id", "id_str")]
  colnames(out) <- c("id_str", "screen_name", "name", "user_id", "user_id_str")
  out
}

#' @export
#' @rdname helpers
media <- function(x, ...) {
  UseMethod("media")
}
#' @export
media.tweets <- function(x, ...) {
  out <- entity(x, "media")
  out[, c("id_str", "media_url", "media_url_https", "url", "display_url",
          "expanded_url", "type", "ext_alt_text")]
}

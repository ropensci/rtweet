#' Extract methods
#'
#' Extract entities of the tweets linked to a tweet id.
#' @param x A tweets object of the rtweet package.
#' @return Some information about those entities and the tweet id it comes from.
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
#' @describeIn helpers Extract urls.
urls <- function(x, ...) {
  UseMethod("urls")
}

#' @export
#' @method urls tweets
urls.tweets <- function(x, ...) {
  out <- entity(x, "urls")
  out[, c("url", "expanded_url", "display_url")]
}

#' @export
#' @describeIn helpers Extract hashtags.
hashtags <- function(x, ...) {
  UseMethod("hashtags")
}

#' @export
#' @method hashtags tweets
hashtags.tweets <- function(x, ...) {
  out <- entity(x, "hashtags")
  out[, c("id_str", "text")]
}

#' @export
#' @describeIn helpers Extract symbols.
symbols <- function(x, ...) {
  UseMethod("symbols")
}

#' @export
#' @method symbols tweets
symbols.tweets <- function(x, ...) {
  out <- entity(x, "symbols")
  out[, c("id_str", "text")]
}

#' @export
#' @describeIn helpers Extract user_mentions.
user_mentions <- function(x, ...) {
  UseMethod("user_mentions")
}

#' @export
#' @method user_mentions tweets
user_mentions.tweets <- function(x, ...) {
  out <- entity(x, "user_mentions")
  out[, c("id_str", "screen_name", "name", "id", "id_str")]
  colnames(out) <- c("id_str", "screen_name", "name", "user_id", "user_id_str")
  out
}

#' @export
#' @describeIn helpers Extract media.
media <- function(x, ...) {
  UseMethod("media")
}

#' @export
#' @method media tweets
media.tweets <- function(x, ...) {
  out <- entity(x, "media")
  out[, c("id_str", "media_url", "media_url_https", "url", "display_url",
          "expanded_url", "type", "ext_alt_text")]
}

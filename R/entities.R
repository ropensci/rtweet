#' Extract methods
#'
#' Extract entities of the tweets linked to a tweet id.
#'
#' The position of where does this occur is not provided.
#' @param x A tweets object of the rtweet package.
#' @param entity A entity to extract data from.
#' @param ... Other possible arguments currently ignored.
#' @return Some information about those entities and the tweet id it comes from.
#' for users mentions the ids of the mentioned users are "user_id", "user_id_str" (not id_str)
#' @export
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
#'   entity(tw, "urls")
#'
#' }
entity <- function(x, entity, ...) {
  UseMethod("entity")
}

#' @export
entity.default <- function(x, entity, ...) {
  is(x, "tweets")
  entity <- match.arg(entity, c("urls", "hashtags", "symbols",
                                "user_mentions", "media"))
  l <- lapply(x, function(x){x[[entity]]})
  l
}


#' @export
entity.tweets <- function(x, entity, ...) {
  entity <- match.arg(entity, c("urls", "hashtags", "symbols",
                                "user_mentions", "media"))

  out <- .entity(x, entity)

  if (entity == "user_mentions") {
    colnames(out) <- c("id_str", "screen_name", "name", "user_id", "user_id_str")
  }
  out
}

.entity <- function(x, name = "urls") {
  ids <- ids(x)
  ent <- lapply(x$entities, function(x) {l_minus(x[[name]], "indices")})
  n_ent <- vapply(ent, NROW, numeric(1L))
  ids <- rep(ids, n_ent)
  ent <- do.call(rbind, ent)
  cbind.data.frame(id_str = ids, ent)
}

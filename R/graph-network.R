#' Network data
#'
#' Retrieve data to know which users are connected to which users.
#' `r lifecycle::badge("deprecated")`
#'
#' @description
#' * `network_data()` returns a data frame that can easily be converted to
#'    various network classes.
#' * `network_graph()` returns a igraph object
#'
#' @param x Data frame returned by rtweet function
#' @param e Type of edge/link–i.e., "mention", "retweet", "quote", "reply".
#'   This must be a character vector of length one or more. This value will be
#'   split on punctuation and space (so you can include multiple types in the
#'   same string separated by a comma or space). The values "all" and
#'   "semantic" are assumed to mean all edge types, which is equivalent to the
#'   default value of `c("mention", "retweet", "reply", "quote")`
#' @return A from/to data edge data frame
#' @seealso [network_graph()] [`rtweet-deprecated`]
#' @export
network_data <- function(x, e = c("mention", "retweet", "reply", "quote")) {
  if (isTRUE(e) || (length(e) == 1 && e %in% c("semantics", "all"))) {
    e <- c("mention", "retweet", "reply", "quote")
  }

  stopifnot(is.character(e))
  y <- users_data(x)

  ids <- character()
  screen_names <- character()
  if ("mention" %in% e) {
    user_mentions <- lapply(x$entities, function(x){
      if (has_name_(x, "user_mentions")) {
        y <- x$user_mentions
        if (length(y$id_str) > 1 || !is.na(y$id_str)) {
          return(y[, c("screen_name", "id_str")])
        }
      }
    NULL
    })
    k <- vapply(user_mentions, is.null, logical(1L))
    # If no mention skip
    if (!all(k)) {


    r <- do.call(rbind, user_mentions[!k])


    ids <- c(ids, r$id_str, y[!k, "id_str", drop = TRUE])
    screen_names <- c(screen_names, r$screen_name, y[!k, "screen_name", drop = TRUE])

    mention <- data.frame(from = rep(y[!k, "id_str", drop = TRUE], times = vapply(user_mentions[!k], nrow, numeric(1L))),
                        to = r$id_str,
                        type = "mention")
    } else {
      mention <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
    }
  } else {
    mention <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
  }

  if ("retweet" %in% e) {
    retweet0 <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
    # Retweets are those that the text start with RT and a mention but are not quoted
    retweets <- startsWith(x$text, "RT @")
    if (any(retweets)) {
      r <- x[retweets, ]
      yr <- y[retweets, ]

      user_mentions <- lapply(r$entities, function(x){
        y <- x$user_mentions
        # Pick the first mention that is the one the tweet is quoting
        # Example: 1390785143615467524
        return(y[y$indices$start == 3, c("screen_name", "id_str")])
      })

      um <- do.call(rbind, user_mentions)
      ur <- yr[, c("screen_name", "id_str")]

      # remove content from deleted users
      um_r <- vapply(user_mentions, nrow, numeric(1L))
      removed_users <- which(um_r == 0)
      for (i in seq_along(ur$id_str)) {
        if (i %in% removed_users) {
          to_id <- NA_character_
          to_screen_name <- NA_character_
        } else {
          to_id <- um$id_str[i]
          to_screen_name <- um$screen_name[i]
        }

        ids <- c(ids, ur$id_str[i], to_id)
        screen_names <- c(screen_names, ur$screen_name[i], to_screen_name)

        retweet <- data.frame(from = ur$id_str[i],
                              to = to_id,
                              type = "retweet")
        retweet0 <- rbind(retweet0, retweet)
      }
    }
    retweet <- retweet0
  } else {
    retweet <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
  }

  if ("reply" %in% e && !all(is.na(x$in_reply_to_user_id_str))) {
    reply_keep <- !is.na(x$in_reply_to_user_id_str)

    ids <- c(ids, y[["id_str"]][reply_keep], x[["in_reply_to_user_id_str"]][reply_keep])
    screen_names <- c(screen_names, y[["screen_name"]][reply_keep], x[["in_reply_to_screen_name"]][reply_keep])

    reply <- data.frame(from = y[["id_str"]][reply_keep],
                        to = x[["in_reply_to_user_id_str"]][reply_keep],
                        type = "reply")

  } else {
    reply <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
  }
  if ("quote" %in% e && !all(is.na(x$is_quote_status))) {
    r <- x[x$is_quote_status, ]
    yr <- y[x$is_quote_status, c("screen_name", "id_str")]
    # Quotes are from users on entities$user_mentions whose indices start at 3

    if (is.data.frame(r$quoted_status$user)) {
      um <- r$quoted_status$user[, c("screen_name", "id_str")]
    } else {
      user_mentions <- lapply(r$quoted_status$user, function(x){
        # Pick the first mention that is the one the tweet is quoting
        # Example: 1390785143615467524
        return(x[, c("screen_name", "id_str")])
      })
      um <- do.call(rbind, user_mentions)
    }
    ums <- is.na(um[, 1])
    if (!is.null(nrow(ums))) {
      um <- um[!ums, ]
      yr <- yr[!ums, ]
      ids <- c(ids, um$id_str, yr$id_str)
      screen_names <- c(screen_names, um$screen_name, yr$screen_name)

      quote <- data.frame(from = um$id_str,
                          to = yr$id_str,
                          type = "quote")
    } else {
      quote <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
    }
  } else {
    quote <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
  }

  out <- rbind(mention, retweet, reply, quote)
  out <- out[!is.na(out$type), ]

  idsn <- data.frame(id = ids, sn = screen_names)
  idsn <- unique(idsn)
  stopifnot(all(out$from %in% idsn$id))
  stopifnot(all(out$to %in% idsn$id))
  attr(out, "idsn") <- as.list(idsn)
  out
}


#' @return An igraph object
#' @rdname network_data
#' @export
network_graph <- function(x, e = c("mention", "retweet", "reply", "quote")) {
  check_installed("igraph")
  nd <- network_data(x = x, e = e)
  idsn <- attr(nd, "idsn")
  g <- igraph::make_empty_graph(n = 0, directed = TRUE)
  g <- igraph::add_vertices(g, length(idsn$id),
    attr = list(id = idsn$id, name = idsn$sn))
  edges <- rbind(match(nd[[1]], idsn$id), match(nd[[2]], idsn$id))
  igraph::add_edges(g, edges, attr = list(type = nd[[3]]))
}

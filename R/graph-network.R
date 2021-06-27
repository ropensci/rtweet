id_sn_index <- function(x) {
  id <- character()
  sn <- character()
  if ("mentions_user_id" %in% names(x)) {
    id <- unroll_users(x$mentions_user_id)
    sn <- unroll_users(x$mentions_screen_name)
    x$mentions_user_id <- NULL
    x$mentions_screen_name <- NULL
  }
  id_vars <- grep("user_id$", names(x), value = TRUE)
  sn_vars <- grep("screen_name$", names(x), value = TRUE)
  id <- c(id, unlist(x[id_vars], use.names = FALSE))
  sn <- c(sn, unlist(x[sn_vars], use.names = FALSE))
  kp <- !duplicated(id) & !is.na(id)
  list(id = id[kp], sn = sn[kp])
}


unroll_users <- function(x) {
  x <- unlist(x, use.names = FALSE)
  x[!is.na(x)]
}

id_sn_join <- function(x, ref) {
  m <- match(x, ref$id)
  ref$sn[m]
}

unroll_connections <- function(x) {
  ## initialize logical (TRUE) vector
  kp <- !logical(nrow(x))
  
  ## measure [and record] length of each 'to' field (list of character vector)
  n <- lengths(x[[2]])
  n1 <- which(n == 1)
  
  ## if length == 1 & is.na(x[1])
  kp[n1[vapply(x[[2]][n1], is.na, logical(1))]] <- FALSE
  
  ## create 'from' and 'to' vectors
  from <- unlist(mapply(rep, x[[1]][kp], n[kp]), use.names = FALSE)
  to <- unlist(x[[2]][kp], use.names = FALSE)
  
  ## return as data frame
  data.frame(
    from = from,
    to = to,
    stringsAsFactors = FALSE
  )
}

prep_from_to <- function(x, from, to) {
  if (is.list(x[[to]])) {
    unroll_connections(x[c(from, to)])
  } else {
    x <- x[c(from, to)]
    names(x) <- c("from", "to")
    x <- x[!is.na(x[[2]]), ]
    x
  }
}

#' Network data
#' 
#' See which users are connected to which users. 
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
#' @seealso network_graph
#' @examples
#'
#' \dontrun{
#'   ## search for #rstats tweets
#'   rstats <- search_tweets("#rstats", n = 200)
#'
#'   ## create from-to data frame representing retweet/mention/reply connections
#'   rstats_net <- network_data(rstats, c("retweet","mention","reply"))
#'
#'   ## view edge data frame
#'   rstats_net
#'
#'   ## view user_id->screen_name index
#'   attr(rstats_net, "idsn")
#'
#'   ## if igraph is installed...
#'   if (requireNamespace("igraph", quietly = TRUE)) {
#'
#'     ## (1) convert directly to graph object representing semantic network
#'     rstats_net <- network_graph(rstats)
#'
#'     ## (2) plot graph via igraph.plotting
#'     plot(rstats_net)
#'   }
#' }
#' @export
network_data <- function(x, e = c("mention", "retweet","reply", "quote")) {
  if (isTRUE(e) || length(e) == 1 && (e == "semantics" || e == "all")) {
    e <- c("mention", "retweet","reply", "quote")
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
      
    
    r <- do.call("rbind", user_mentions[!k])
    
    
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
    # Retweets are those that the text start with RT and a mention but are not quoted
    retweets <- startsWith(x$text, "RT @")
    r <- x[retweets, ]
    yr <- y[retweets, ]
    
    user_mentions <- lapply(r$entities, function(x){
      y <- x$user_mentions
      # Pick the first mention that is the one the tweet is quoting
      # Example: 1390785143615467524
      return(y[y$indices$start == 3, c("screen_name", "id_str")])
    })
    um <- do.call("rbind", user_mentions)
    ur <- yr[, c("screen_name", "id_str")]

    ids <- c(ids, ur$id_str, um$id_str)
    screen_names <- c(screen_names, ur$screen_name, um$screen_name)
    
    retweet <- data.frame(from = um$id_str,
                          to = ur$id_str,
                          type = "retweet")
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
    
    if (is(r$quoted_status$user, "data.frame")) {
      um <- r$quoted_status$user[, c("screen_name", "id_str")]
    } else {
      user_mentions <- lapply(r$quoted_status$user, function(x){
        # Pick the first mention that is the one the tweet is quoting
        # Example: 1390785143615467524
        return(x[, c("screen_name", "id_str")])
      })
      um <- do.call("rbind", user_mentions)
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
network_graph <- function(.x, .e = c("mention", "retweet", "reply", "quote")) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(
      "Please install the {igraph} package to use this function",
      call. = FALSE
    )
  }
  .x <- network_data(x = .x, e = .e)
  idsn <- attr(.x, "idsn")
  g <- igraph::make_empty_graph(n = 0, directed = TRUE)
  g <- igraph::add_vertices(g, length(idsn$id),
    attr = list(id = idsn$id, name = idsn$sn))
  edges <- rbind(match(.x[[1]], idsn$id), match(.x[[2]], idsn$id))
  igraph::add_edges(g, edges, attr = list(type = .x[[3]]))
}

# user_vars <- c("user_id", "screen_name", "name", "location", "description",
#   "url", "protected", "followers_count", "friends_count", "listed_count",
#   "statuses_count", "favourites_count", "account_created_at", "verified",
#   "profile_url", "profile_expanded_url", "account_lang",
#   "profile_banner_url", "profile_background_url", "profile_image_url")

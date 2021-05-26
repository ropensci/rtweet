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
        if (length(y$id) > 1 || !is.na(y$id)) {
          return(y[, c("screen_name", "id")])
        }
      }
    NULL
    })
    k <- vapply(user_mentions, is.null, logical(1L))
    r <- do.call("rbind", user_mentions[!k])
    
    
    ids <- c(ids, r$id, y[!k, "id", drop = TRUE])
    screen_names <- c(screen_names, r$screen_name, y[!k, "screen_name", drop = TRUE])
    
    mention <- data.frame(from = rep(y[!k, "id", drop = TRUE], times = vapply(user_mentions[!k], nrow, numeric(1L))),
                        to = r$id,
                        type = "mention")
  } else {
    mention <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
  }
  
  if ("retweet" %in% e) {
    # Retweets are those that the text start with RT and a mention but are not quoted
    r <- x[!x$is_quote_status & startsWith(r$text, "RT @"), ]
    
    user_mentions <- lapply(r$entities, function(x){
      y <- x$user_mentions
      # Pick the first mention that is the one the tweet is quoting
      # Example: 1390785143615467524
      return(y[y$indices$start == 3, c("screen_name", "id")])
    })
    um <- do.call("rbind", user_mentions)
    
    ids <- c(ids, r$id, y[x$retweeted, "id", drop = TRUE])
    screen_names <- c(screen_names, r$screen_name, y[x$retweeted, "screen_name", drop = TRUE])
    
    retweet <- data.frame(from = r$id,
                        to = y[x$retweeted, "id"],
                        type = "retweet")
    
  } else {
    retweet <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
  }
  
  if ("reply" %in% e && !all(is.na(x$in_reply_to_user_id))) {
    reply_keep <- x$in_reply_to_user_id & !is.na(x$in_reply_to_user_id)
    
    ids <- c(ids, y[["id"]][reply_keep], x[["in_reply_to_user_id"]][reply_keep])
    screen_names <- c(screen_names, y[["screen_name"]][reply_keep], x[["in_reply_to_screen_name"]][reply_keep])
    
    reply <- data.frame(from = y[["id"]][reply_keep],
                        to = x[["in_reply_to_user_id"]][reply_keep],
                        type = "reply")
    
  } else {
    reply <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
  }
  if ("quote" %in% e && !all(is.na(x$is_quote_status))) {
    r <- x[x$is_quote_status, ]
    # Quotes are from users on entities$user_mentions whose indices start at 3
    user_mentions <- lapply(r$entities, function(x){
      y <- x$user_mentions
      # Pick the first mention that is the one the tweet is quoting
      # Example: 1390785143615467524
      return(y[y$indices$start == 3, c("screen_name", "id")])
    })
    um <- do.call("rbind", user_mentions)
    ids <- c(ids, y[x$is_quote_status, "id", drop = TRUE], um$id)
    screen_names <- c(screen_names, y[x$is_quote_status, "screen_name", drop = TRUE], um$screen_name)
    
    quote <- data.frame(from = y[x$is_quote_status, "id", drop = TRUE],
                        to = um$id,
                        type = "quote")
  } else {
    quote <- data.frame(from = NA, to = NA, type = NA)[0, , drop = FALSE]
  }
  
  out <- rbind(mention, retweet, reply, quote)
  
  
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

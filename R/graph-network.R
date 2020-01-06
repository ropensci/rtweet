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
#' Convert Twitter data into a network-friendly data frame
#'
#' @return A from/to data edge data frame
#' @details \code{network_data} returns a data frame that can easily be converted to
#'   various network classes. For direct conversion to a network object, see
#'  \code{\link{network_graph}}.
#' @seealso network_graph
#' @examples
#'
#' \dontrun{
#'   ## search for #rstats tweets
#'   rstats <- search_tweets("#rstats", n = 200)
#'
#'   ## create from-to data frame representing retweet/mention/reply connections
#'   rstats_net <- network_data(rstats, "retweet,mention,reply")
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
#' @rdname network_graph
#' @export
network_data <- function(.x, .e = c("mention,retweet,reply,quote")) {
  if (isTRUE(.e)) {
    .e <- "all"
  }
  stopifnot(is.character(.e))
  .e <- sub("d$|s$", "", tolower(unlist(strsplit(.e, "[[:punct:] ]+"))))
  if (length(.e) == 1 &&  grepl("^all$|^semantic$", .e, ignore.case = TRUE)) {
    .e <- c("mention", "retweet", "reply", "quote")
  }
  .x <- lapply(.e, network_data_one, .x)
  idsn <- lapply(.x, attr, "idsn")
  idsn <- list(
    id = unlist(lapply(idsn, `[[`, "id"), use.names = FALSE),
    sn = unlist(lapply(idsn, `[[`, "sn"), use.names = FALSE)
  )
  idsn$sn <- idsn$sn[!duplicated(idsn$id)]
  idsn$id <- idsn$id[!duplicated(idsn$id)]
  .x <- do.call(rbind, .x)
  attr(.x, "idsn") <- idsn
  .x
}

network_data_one <- function(.e, .x) {
  stopifnot(.e %in% c("mention", "retweet", "reply", "quote"))
  vars <- c("user_id", "screen_name", switch(.e,
    mention = c("mentions_user_id", "mentions_screen_name"),
    retweet = c("retweet_user_id",  "retweet_screen_name"),
    reply =   c("reply_to_user_id", "reply_to_screen_name"),
    quote =   c("quoted_user_id",   "quoted_screen_name")))
  .x <- .x[, vars]
  idsn <- id_sn_index(.x)
  v <- names(.x)
  .x <- prep_from_to(.x, v[1], v[3])
  if (nrow(.x) > 0) {
    .x$type <- .e
  }
  attr(.x, "idsn") <- idsn
  .x
}

#' Network graph
#'
#' Convert Twitter data into network graph object (igraph)
#'
#' @param .x Data frame returned by rtweet function
#' @param .e Type of edge/link–i.e., "mention", "retweet", "quote", "reply".
#'   This must be a character vector of length one or more. This value will be
#'   split on punctuation and space (so you can include multiple types in the
#'   same string separated by a comma or space). The values "all" and
#'   "semantic" are assumed to mean all edge types, which is equivalent to the
#'   default value of \code{c("mention,retweet,reply,quote")}
#' @return An igraph object
#' @details \code{network_graph} requires previous installation of the igraph package.
#'   To return a network-friendly data frame, see \code{\link{network_data}}
#' @seealso network_data
#' @export
network_graph <- function(.x, .e = c("mention,retweet,reply,quote")) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(
      "Please install the {igraph} package to use this function",
      call. = FALSE
    )
  }
  if (isTRUE(.e)) {
    .e <- "all"
  }
  stopifnot(is.character(.e))
  .e <- sub("d$|s$", "", tolower(unlist(strsplit(.e, "[[:punct:] ]+"))))
  if (length(.e) == 1 &&  grepl("^all$|^semantic$", .e, ignore.case = TRUE)) {
    .e <- c("mention", "retweet", "reply", "quote")
  }
  .x <- network_data(.x, .e)
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

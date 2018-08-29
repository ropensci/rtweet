#' Get tweets data for statuses favorited by one or more target users.
#'
#' Returns up to 3,000 statuses favorited by each of one or more
#' specific Twitter users.
#'
#' @param user Vector of user names, user IDs, or a mixture of both.
#' @param n Specifies the number of records to retrieve. Defaults to
#'   200. 3000 is the max number of favorites returned per token. Due
#'   to suspended or deleted content, this function may return fewer
#'   tweets than the desired (n) number. Must be of length 1 or of
#'   length equal to the provided number of users.
#' @param since_id Returns results with an status_id greater than
#'   (that is, more recent than) the specified status_id.  There are
#'   limits to the number of tweets returned by the REST API. If the
#'   limit is hit, since_id is adjusted (by Twitter) to the oldest ID
#'   available.
#' @param max_id Character, returns results with an ID less than (that is,
#'   older than) or equal to `max_id`.
#' @param parse Logical, indicating whether to return parsed vector or
#'   nested list object. By default, \code{parse = TRUE}
#'   saves you the time [and frustrations] associated with
#'   disentangling the Twitter API return objects.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @return A tbl data frame of tweets data with users data attribute.
#' @examples
#'
#' \dontrun{
#'
#' ## get max number of statuses favorited by KFC
#' kfc <- get_favorites("KFC", n = 3000)
#' kfc
#'
#' ## get 400 statuses favorited by each of three users
#' favs <- get_favorites(c("Lesdoggg", "pattonoswalt", "meganamram"))
#' favs
#'
#' }
#'
#' @family tweets
#' @seealso
#' \url{https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-favorites-list}
#' @export
get_favorites <- function(user,
                          n = 200,
                          since_id = NULL,
                          max_id = NULL,
                          parse = TRUE,
                          token = NULL) {
  args <- list(
    user = user,
    n = n,
    since_id = since_id,
    max_id = max_id,
    parse = parse,
    token = token
  )
  do.call("get_favorites_call", args)
}

get_favorites_call <- function(user,
                               n = 200,
                               since_id = NULL,
                               max_id = NULL,
                               parse = TRUE,
                               token = NULL) {
  if (is.null(token)) {
    token <- check_token(token)
  }
  ## check inputs
  stopifnot(is.atomic(user), is.numeric(n), sapply(user, is.valid.username))
  if (length(user) == 0L) {
    stop("No user found", call. = FALSE)
  }

  if (!is.null(max_id) && !is.null(since_id)) {
    rt <- Map(
      get_favorites_,
      user = user,
      n = n,
      since_id = since_id,
      max_id = max_id,
      parse = parse,
      token = list(token)
    )
  } else if (!is.null(since_id)) {
    rt <- Map(
      get_favorites_,
      user = user,
      n = n,
      since_id = since_id,
      parse = parse,
      token = list(token)
    )
  } else if (!is.null(max_id)) {
    rt <- Map(
      get_favorites_,
      user = user,
      n = n,
      max_id = max_id,
      parse = parse,
      token = list(token)
    )
  } else {
    rt <- Map(
      get_favorites_,
      user = user,
      n = n,
      parse = parse,
      token = list(token)
    )
  }
  if (parse) {
    nms <- all_uq_names(rt)
    for (i in seq_along(rt)) {
      for (j in nms) {
        if (!has_name_(rt[[i]], j) && nrow(rt[[i]]) > 0L) {
          rt[[i]][[j]] <- NA
          if (j == "created_at") {
            rt[[i]][[j]] <- as.POSIXct(rt[[i]][[j]])
          }
        } else if (!has_name_(rt[[i]], j) && nrow(rt[[i]]) == 0L) {
          rt[[i]] <- tibble::as_tibble(data.frame(j = NA))
          names(rt[[i]]) <- j
          if (j == "created_at") {
            rt[[i]][[j]] <- as.POSIXct(rt[[i]][[j]])
          }
        }
      }
    }
    ## add favoriter variable to data frames
    for (i in seq_along(user)) {
      if (nrow(rt[[i]]) > 0) {
        rt[[i]]$favorited_by <- user[i]
      }
    }
    rt <- do_call_rbind(rt)
  }
  rt
}

get_favorites_ <- function(user,
                           n = 200,
                           since_id = NULL,
                           max_id = NULL,
                           parse = TRUE,
                           token = NULL) {
  query <- "favorites/list"
  if (n > 3000) {
    warning(paste0("n exceeds max favs returned per ",
      "token. Setting n to 3000..."),
      call. = FALSE)
    n <- 3000
    count <- 200
  } else if (n < 200) {
    count <- n
  } else {
    count <- 200
  }
  n.times <- rate_limit(token, query)[["remaining"]]
  if (n.times == 0L) stop("rate limit exceeded", call. = FALSE)
  params <- list(
    user_type = user,
    count = count,
    tweet_mode = "extended",
    max_id = max_id,
    since_id = since_id
  )
  names(params)[1] <- .id_type(user)
  url <- make_url(
    query = query,
    param = params)
  fav <- scroller(url, n, n.times, type = "timeline", token)
  if (parse) {
    fav <- tweets_with_users(fav)
  }
  fav
}

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
#' @param max_id Returns results with status_id less (older) than or
#'   equal to (if hit limit) the specified status_id.
#' @param parse Logical, indicating whether to return parsed vector or
#'   nested list object. By default, \code{parse = TRUE}
#'   saves you the time [and frustrations] associated with
#'   disentangling the Twitter API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
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
  stopifnot(is.atomic(user), is.numeric(n))
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
          rt[[i]] <- tibble::as_tibble(data.frame(j = NA), validate = FALSE)
          names(rt[[i]]) <- j
          if (j == "created_at") {
            rt[[i]][[j]] <- as.POSIXct(rt[[i]][[j]])
          }
        }
      }
    }
    ## add favoriter variable to data frames
    for (i in seq_along(user)) {
      rt[[i]]$favorited_by <- user[i]
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
  }
  n.times <- rate_limit(token, query)[["remaining"]]
  if (n.times == 0L) stop("rate limit exceeded", call. = FALSE)
  params <- list(
    user_type = user,
    count = count,
    tweet_mode = "extended"
  )
  names(params)[1] <- .id_type(user)
  url <- make_url(
    query = query,
    param = params)
  fav <- scroller(url, n, n.times, type = "timeline", token)
  if (parse) {
    fav <- tweets_with_users(fav)
    usr <- users_data(fav)
    if (nrow(usr) > 0L) {
      uq <- !duplicated(usr$user_id)
      attr(fav, "users") <- usr[uq, ]
    }
  }
  fav
}

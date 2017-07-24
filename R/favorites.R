#' GET favorites/list
#'
#' Returns favorite tweets data for one or more target users.
#' 
#' @param user Vector of user names, user IDs, or a mixture of both.
#' @param n Specifies the number of records to retrieve. Defaults to
#'   200. 3000 is the max number of favorites returned per token.  Due
#'   to suspended or deleted content, this function may return fewer
#'   tweets than the desired (n) number. Must be of length 1 or of
#'   length equal to users
#' @param ... To see other possible arguments see
#'   \code{\link{get_favorites.default}}.
#' @return A tbl data frame of tweets data with users data attribute.
#' @export
get_favorites <- function(user, n, ...) {
  UseMethod("get_favorites")
}

#' get_favorites.default
#'
#' Returns favorite tweets data for one or more target users.
#' 
#' @param user Screen names and/or user IDs of target users.
#' @param n Specifies the number of records to retrieve. Defaults
#'   to 200. 3000 is the max number of favorites returned per token.
#'   Due to suspended or deleted content, this function may return
#'   fewer tweets than the desired (n) number.
#' @param since_id Returns results with an status_id greater
#'   than (that is, more recent than) the specified status_id.
#'   There are limits to the number of tweets returned by the REST
#'   API. If the limit is hit, since_id is adjusted (by Twitter) to
#'   the oldest ID available.
#' @param max_id Returns results with status_id less (older) than or
#'   equal to (if hit limit) the specified status_id.
#' @param parse Logical, indicating whether to return parsed
#'   vector or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves you the time [and frustrations]
#'   associated with disentangling the Twitter API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @return Tweets data frame.
#' @examples
#' \dontrun{
#' # get favorites of the president of the US
#' pres <- get_favorites(user = "potus")
#' pres
#'
#' # get favorites of the Environmental Protection Agency
#' epa <- get_favorites(user = "epa")
#' epa
#' }
#' @family tweets
#' @export
get_favorites.default <- function(user,
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
  ## add favoriter variable to data frames
  rt <- Map(cbind, rt, favoriter = user, stringsAsFactors = FALSE)
  ## merge users data into one data frame
  rt_users <- do.call("rbind", lapply(rt, users_data))
  ## merge tweets data into one data frame
  rt <- do.call("rbind", rt)
  ## set users attribute
  attr(rt, "users") <- rt_users
  ## return tibble (validate = FALSE makes it a bit faster)
  tibble::as_tibble(rt, validate = FALSE)
}


get_favorites_ <- function(user,
                           n = 300,
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
  }
  stopifnot(is_n(n),
    is.atomic(user),
    isTRUE(length(user) == 1))
  token <- check_token(token, query)
  n.times <- rate_limit(token, query)[["remaining"]]
  if (n.times == 0L) stop("rate limit exceeded", call. = FALSE)
  params <- list(
    user_type = user,
    count = 200
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
      usr <- usr[uq, ]
      attr(fav, "users") <- usr[uq, ]
    }
  }
  fav
}

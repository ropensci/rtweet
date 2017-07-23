#' get_friends
#'
#' @description Requests information from Twitter's REST API
#'   regarding a user's friend network (i.e., accounts followed
#'   by a user). To request information on followers of accounts
#'
#' @param user Screen name or user id of target user.
#' @param page Default \code{page = -1} specifies first page of json
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object.
#' @param parse Logical, indicating whether to return parsed
#'   vector or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves you the time [and frustrations]
#'   associated with disentangling the Twitter API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # get ids of users followed by the president of the US
#' pres <- get_friends(user = "potus")
#' pres
#'
#' # get ids of users followed by the Environmental Protection Agency
#' epa <- get_friends(user = "epa")
#' epa
#' }
#'
#' @return friends User ids for everyone a user follows.
#' @family ids
#' @export
get_friends <- function(user, ...) {
  UseMethod("get_friends")
}

get_friends.default <- function(user, page = "-1", parse = TRUE, token = NULL) {

  stopifnot(is.atomic(user),
            is.atomic(page),
            isTRUE(length(user) == 1))

  token <- check_token(token)

  query <- "friends/ids"

  params <- list(
    user_type = user,
    count = 5000,
    cursor = page,
    stringify = TRUE
  )

  names(params)[1] <- .id_type(user)

  url <- make_url(
    query = query,
    param = params)

  f <- tryCatch(
    TWIT(get = TRUE, url, token),
    error = function(e) return(NULL))

  if (is.null(f)) {
    missing <- NA_character_
    f[["ids"]] <- missing
  } else {
    if (parse) f <- parse.piper.fnd(f)
  }
  f
}

#' @importFrom jsonlite fromJSON
parse.piper.fnd <- function(r) {
  if (isTRUE("content" %in% names(r))) {
    r <- tryCatch(
      jsonlite::fromJSON(rawToChar(r[["content"]])),
      error = function(e) return(NULL))
    r <- fnd_internal(r)
  } else {
    if (length(r) > 1L) {
      r <- lapply(r, fnd_internal)
      r <- do.call("rbind", r)

    } else {
      r <- fnd_internal(r)
    }
  }
  if (requireNamespace("tibble", quietly = TRUE)) {
    r <- tibble::as_tibble(r, validate = FALSE)
  }
  r
}

fnd_internal <- function(r) {
  if (is.null(r)) return(data.frame(user_id = NA_character_))
  next_cursor <- as.character(r[["next_cursor_str"]])
  usrs <- data.frame(as.character(r[["ids"]]),
                     stringsAsFactors = FALSE)
  names(usrs) <- "user_id"
  if (is.null(next_cursor)) next_cursor <- NA_character_
  attr(usrs, "next_cursor") <- next_cursor
  usrs
}

ply_friends <- function(users, token = NULL, ...) {
    n.times <- rate_limit(token, "friends/ids")[["remaining"]]
    if (n.times == 0L) stop("rate limit exceeded", call. = FALSE)
    lapply(users, get_friends, token = token, ...)
}


#' lookup_friendships
#'
#' Look up informaiton on friendship between authenticated
#'   user and up to 100 users.
#'
#' @param user Screen name or user id of target user.
#' @param parse Logical indicating whether to return parsed data frame.
#'   Defaults to true.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @export
lookup_friendships <- function(user, parse = TRUE,
                               token = NULL) {

    stopifnot(is.atomic(user))

    token <- check_token(token)

    query <- "friendships/lookup"

    params <- list(
        user_type = paste(user, collapse = ","))

    names(params)[1] <- .id_type(user)

    url <- make_url(
        query = query,
        param = params)

    f <- tryCatch(
  	TWIT(get = TRUE, url, token),
  	error = function(e) return(NULL))

    if (parse) {
        parse_fndshp(f)
    } else {
        f
    }
}


#' @importFrom jsonlite fromJSON
parse_fndshp <- function(fndshp) {
    fndshp <- fndshp[["content"]] %>%
        rawToChar() %>%
        jsonlite::fromJSON() %>%
        lapply("[[[", "connections")
    fndshp$followed_by <- fndshp %>%
        plyget(function(x) "followed_by" %in% x) %>%
        unlist(use.names = FALSE)
    fndshp$following <- fndshp %>%
        plyget(function(x) "following" %in% x) %>%
        unlist(use.names = FALSE)
    fndshp$blocking <- fndshp %>%
        plyget(function(x) "blocking" %in% x) %>%
        unlist(use.names = FALSE)
    fndshp$muting <- fndshp %>%
        plyget(function(x) "muting" %in% x) %>%
        unlist(use.names = FALSE)
    fndshp$none <- fndshp %>%
        plyget(function(x) "none" %in% x) %>%
        unlist(use.names = FALSE)
    fndshp[, c(1:2, 4, 6:10)]
}


plyget <- function(x, f, ...) {
    if (!is.function(f)) {
        if (any(is.data.frame(x),
                f %in% names(x))) return(x[[f]])
        lapply(x, function(x) x[[f]])
    } else if (is.data.frame(x)) {
        f(x, ...)
    } else {
        lapply(x, f, ...)
    }
}

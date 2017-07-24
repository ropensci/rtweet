#' GET friends/ids
#'
#' Returns user IDs of accounts followed by target user.
#'
#' @param users Screen name or user ID of target user from which the user IDs
#'   of friends (accounts followed BY target user) will be retrieved.
#' @param retryonratelimit If you'd like to retrieve 5,000 or fewer friends for
#'   more than 15 target users, then set \code{retryonratelimit = TRUE} and
#'   this function will use \code{\link{Sys.sleep}} until rate limits reset and
#'   the desired number of friend networks is retrieved. This defaults
#'   to FALSE. See details for more info regarding possible issues with timing
#'   misfires.
#' @param ... For other possible args see \code{\link{get_friends.default}}.
#' @details When \code{retryonratelimit = TRUE} this function internally
#'   makes a rate limit API call to get information on (a) the number of requests
#'   remaining and (b) the amount of time until the rate limit resets. So, in
#'   theory, the sleep call should only be called once between waves of data
#'   collection. However, as a fail safe, if a system's time is calibrated such
#'   that it expires before the rate limit reset, or if, in another session, the
#'   user dips into the rate limit, then this function will wait (use Sys.sleep
#'   for a second time) until the next rate limit reset. Users should monitor
#'   and test this before making especially large calls as any systematic issues
#'   could create sizable inefficiencies.
#' @return A tibble data frame with two columns, "user" for name or ID of target
#'   user and "user_id" for follower IDs.
#' @export
get_friends <- function(users, retryonratelimit = FALSE, ...) {
  UseMethod("get_friends")
}

#' get_friends
#'
#' @description Requests information from Twitter's REST API
#'   regarding a user's friend network (i.e., accounts followed
#'   by a user). To request information on followers of accounts
#'
#' @param users Screen name or user ID of target user from which the user IDs
#'   of friends (accounts followed BY target user) will be retrieved.
#' @param retryonratelimit If you'd like to retrieve 5,000 or fewer friends for
#'   more than 15 target users, then set \code{retryonratelimit = TRUE} and
#'   this function will use \code{\link{Sys.sleep}} until rate limits reset and
#'   the desired number of friend networks is retrieved. This defaults
#'   to FALSE. See details for more info regarding possible issues with timing
#'   misfires.
#' @param page Default \code{page = -1} specifies first page of json
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object.
#' @param parse Logical, indicating whether to return parsed
#'   vector or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves you the time [and frustrations]
#'   associated with disentangling the Twitter API return objects.
#' @param verbose Logical indicating whether or not to include output messages.
#'   Defaults to TRUE.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # get ids of users followed by the president of the US
#' pres <- get_friends(users = "potus")
#' pres
#'
#' # get friend networks of multiple users
#' epa <- get_friends(users = c("jack", "epa"))
#' epa
#' }
#'
#' @return A tibble data frame of follower IDs (one column named "user_id").
#' @family ids
#' @export
get_friends.default <- function(users,
                                retryonratelimit = FALSE,
                                page = "-1",
                                parse = TRUE,
                                verbose = TRUE,
                                token = NULL) {
  stopifnot(is.atomic(users))
  if (any(is.na(users))) {
    warning("Missing users omitted", call. = FALSE)
    users <- na_omit(users)
  }
  ## number of users to return
  n <- length(users)
  ## build URL
  query <- "friends/ids"
  token <- check_token(token, query)
  ## for larger requests implement Sys.sleep
  if (n > 1) {
    ## initialize output object
    f <- vector("list", n)
    ## default (counter) values
    more <- TRUE
    i <- 0L
    ## until friends of n users have been retrieved
    while (more) {
      rl <- rate_limit(token, query)
      n.times <- rl[["remaining"]]
      i <- i + 1L
      params <- list(
        user_type = users[i],
        count = 5000,
        cursor = page,
        stringify = TRUE
      )
      names(params)[1] <- .id_type(users[i])
      url <- make_url(
        query = query,
        param = params
      )
      if (retryonratelimit) {
        ## if no calls remaining then sleep until no longer rate limited
        rate_limited <- isTRUE(n.times == 0)
        while (rate_limited) {
          if (verbose) {
            message(
              paste("Waiting about",
                    round(as.numeric(rl$reset, "secs") / 60, 1),
                    "minutes",
                    "for rate limit reset...")
            )
          }
          Sys.sleep(as.numeric(rl$reset, "secs") + 2)
          rl <- rate_limit(token, query)
          n.times <- rl$remaining
          rate_limited <- isTRUE(n.times == 0)
        }
      }
      ## make call
      f[[i]] <- get_friend(url, token = token)
      if (has_name(f[[i]], "errors")) {
        warning(f[[i]]$errors[["message"]], call. = FALSE)
        return(list(data.frame()))
      } else if (parse) {
        nextcursor <- f[["next_cursor"]]
        f[[i]] <- tibble::as_tibble(
          list(user = users[i], user_id = f[[i]][["ids"]])
        )
        attr(f[[i]], "next_cursor") <- nextcursor
      }
      if (verbose) {
        message(paste(i, "friend networks collected!"))
      }
      ## update more (logical)
      more <- isTRUE(i < n)
    }
    ## i don't think this line is needed anymore but just in case
    f <- f[!vapply(f, is.null, logical(1))]
    ## parse into data frame
    if (parse) {
      nextcursors <- lapply(f, attr, "next_cursor")
      f <- do.call("rbind", f)
      attr(f, "next_cursor") <- nextcursors
    }
  } else {
    ## compose query
    params <- list(
      user_type = users,
      count = 5000,
      cursor = page,
      stringify = TRUE
    )
    names(params)[1] <- .id_type(users)
    url <- make_url(
      query = query,
      param = params
    )
    ## if !retryonratelimit then if necessary exhaust what can with token
    f <- get_friend(url, token = token)
      if (has_name(f, "errors")) {
        warning(f$errors[["message"]], call. = FALSE)
        return(list(data.frame()))
      } else if (parse) {
        nextcursor <- f[["next_cursor"]]
        f <- tibble::as_tibble(
          list(user = users, user_id = f[["ids"]])
        )
        attr(f, "next_cursor") <- nextcursor
      }
  }
  f
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
get_friend <- function(url, token = NULL) {
  jsonlite::fromJSON(httr::content(httr::GET(url, token), "text"))
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

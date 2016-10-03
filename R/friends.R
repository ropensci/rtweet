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
get_friends <- function(user, page = "-1", parse = TRUE,
  token = NULL) {

  query <- "friends/ids"

  stopifnot(is.atomic(user), is.atomic(page),
    isTRUE(length(user) == 1))

  token <- check_token(token)

  params <- list(
    user_type = user,
    count = 5000,
    cursor = page,
    stringify = TRUE)

  names(params)[1] <- .id_type(user)

  url <- make_url(
    query = query,
    param = params)

  f <- tryCatch(
  	TWIT(get = TRUE, url, token),
  	error = function(e) NULL)
  	#tryCatch(scroller(url, 1, 15, token),
  	#error = function(e) return(NULL))

  remaining <- f$headers$`x-rate-limit-remaining`

  if (is.null(f)) {
  	f[["ids"]] <- NA_real_
  } else {
  	if (parse) {
  		f <- parse_fs(f)
  	}
  }
  f
}



#' ply_friends
#'
#' @param users Screen names and/or user ids of target user. Since
#'   friend networks are requested one at a time, users can be a mixture of
#'   user_ids and screen_names.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param \dots Arguments passed on to \code{\link{get_friends}}.
#'
#' @return List of friend networks where \code{n}th element contains
#'   user ids of \code{n}th users.
#' @export
#'
#' @examples
#' \dontrun{
#' get friend networks for up to 15 users per token
#' users <- c("inside_R", "rtweet_package", "RLangTip", "Rbloggers", "rstudio")
#' friend.networks <- ply_friends(users)
#' friend.networks[[1]]
#' str(friend.networks)
#' }
ply_friends <- function(users, token = NULL, ...) {
	n.times <- rate_limit(token, "friends/ids")[["remaining"]]
	if (n.times == 0L) stop("rate limit exceeded", call. = FALSE)
	lapply(users, get_friends, token = token, ...)
}
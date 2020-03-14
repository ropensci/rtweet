#' Get user [account] suggestions for authenticating user
#'
#' Returns Twitter's list of suggested user categories.
#'
#' @return List of recommended categories which can be passed along as
#'   the "slug" parameter in \code{\link{suggested_users}}
#' @export
#' @rdname suggested_users
suggested_slugs <- function(lang = NULL, token = NULL) {
  query <- "users/suggestions"
  token <- check_token(token)
  params <- list(lang = lang)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  from_js(r)
}


#' Returns users from a specific, suggested category
#'
#' @param slug required The short name of list or a category
#' @param lang optional Restricts the suggested categories to the
#'   requested language. The language must be specified by the
#'   appropriate two letter ISO 639-1 representation.
#' @param parse Logical indicating whether to parse the returned data into
#'   a tibble data frame. See details for more on the returned users data.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @details Currently, this parsing process drops all
#'   recursive (list) columns, which mostly means you are shorted some
#'   entities data. To maximize users data, however, it is recommended to
#'   make an additional \code{\link{lookup_users}} call using the user IDs
#'   returned by this function.
#' @return Recommended users
#' @export
#' @rdname suggested_users
#' @examples
#'
#' \dontrun{
#'
#' ## get slugs
#' slugs <- suggested_slugs()
#'
#' ## use slugs to get suggested users
#' suggested_users(slugs$slug[1])
#'
#' ## alternatively, get all users from all slugs in one function
#' sugs <- all_suggested_users()
#'
#' ## print data
#' sugs
#'
#' ## for complete users data, lookup user IDs
#' sugs_usr <- lookup_users(sugs$user_id)
#'
#' ## view users data
#' sugs_usr
#'
#' }
#'
#' @family suggested
suggested_users <- function(slug, lang = NULL, parse = TRUE, token = NULL) {
  if (missing(slug)) {
    stop("Must provide slug. See: suggested_slugs for list of possible values",
         call. = FALSE)
  }
  stopifnot(is.character(slug), length(slug) == 1L)
  query <- sprintf("users/suggestions/%s", slug)
  token <- check_token(token)
  params <- list(slug = slug, lang = lang)
  url <- make_url(query = query, param = params)
  d <- httr::GET(url, token)
  warn_for_twitter_status(d)
  d <- from_js(d)
  if (parse) {
    d <- slug_tbl(d)
  }
  d
}

#' Get all user [account] suggestions for authenticating user
#'
#' Returns users data for all users in Twitter's suggested categories.
#'
#' @param slugs Optional, one or more slugs returned by
#'   \code{\link{suggested_slugs}}. API rate limits this to 15 max (function
#'   will return warnings for slugs provided beyond the remaining limit).
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @export
#' @rdname suggested_users
suggested_users_all <- function(slugs = NULL, parse = TRUE, token = NULL) {
  if (is.null(slugs)) {
    slugs <- suggested_slugs(token = token)
    stopifnot(is.data.frame(slugs), nrow(slugs) > 0L)
  }
  if (is.data.frame(slugs)) {
    slugs <- slugs$slug
  }
  stopifnot(is.character(slugs))
  d <- vector('list', length(slugs))
  for (i in seq_along(d)) {
    d[[i]] <- suggested_users(slugs[i], parse = parse, token = token)
  }
  if (parse) {
    d <- do.call("rbind", d)
  }
  d
}

slug_tbl <- function(x) {
  slug_tbl_ <- function(x) {
    ## validate else return empty df
    if (!is.list(x)) return(data.frame())
    if (!"users" %in% names(x)) return(data.frame())
    if (!is.data.frame(x$users)) return(data.frame())
    ## create slug variable
    x$users$slug <- x$slug
    ## drop dbl id
    x$users <- x$users[names(x$users) != "id"]
    ## rename chr id to user_id
    names(x$users)[names(x$users) == "id_str"] <- "user_id"
    ## format date
    x$users$created_at <- format_date(x$users$created_at)
    ## rename to account_created_at
    names(x$users)[names(x$users) == "created_at"] <- "account_created_at"
    ## convert to tibble
    x <- tibble::as_tibble(x$users[vapply(x$users, is.atomic, FUN.VALUE = logical(1))])
    ## rearrange so slug is first
    x[c(ncol(x), 1:(ncol(x) - 1L))]
  }
  ## use slug_tbl_ on data (or apply to list of data)
  if ("users" %in% names(x)) {
    slug_tbl_(x)
  } else {
    do.call("rbind", lapply(x, slug_tbl_))
  }
}


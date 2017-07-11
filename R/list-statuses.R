#' GET lists/statuses¶
#'
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of its numerical id. If you decide
#'   to do so, note that you’ll also have to specify the list owner using the
#'   owner_id or owner_screen_name parameters.
#' @param owner_user optional The screen name or user ID of the user who owns the list being requested by a slug.
#' @param since_id optional Returns results with an ID greater than (that is, more recent than) the
#'   specified ID. There are limits to the number of Tweets which can be accessed
#'   through the API. If the limit of Tweets has occured since the since_id, the
#'   since_id will be forced to the oldest ID available.
#' @param max_id optional Returns results with an ID less than (that is, older than) or equal to the
#'   specified ID.
#' @param count optional Specifies the number of results to retrieve per “page.”
#' @param include_rts optional When set to either true , t or 1 , the list timeline will contain native
#'   retweets (if they exist) in addition to the standard stream of tweets. The
#'   output format of retweeted tweets is identical to the representation you see in
#'   home_timeline.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return data
f <- function(list_id = NULL,
              slug = NULL,
              owner_user = NULL,
              since_id = NULL,
              max_id = NULL,
              count = 5000,
              include_rts = TRUE,
              token = NULL) {
  query <- "lists/statuses"
  params <- list(list_id = list_id,
                 slug = slug,
                 owner_user = owner_user,
                 cursor = cursor,
                 stringify_ids = TRUE)
  names(params)[3] <- paste0("owner_", .id_type(owner_user))
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  from_js(r)
}



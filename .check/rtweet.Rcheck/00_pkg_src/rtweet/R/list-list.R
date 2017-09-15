#' GET lists/list
#'
#' Returns all lists the authenticating or specified user subscribes to,
#'   including their own. The user is specified using the user_id or
#'   screen_name parameters. If no user is given, the authenticating
#'   user is used.
#' 
#' @param user The ID of the user or screen name for whom to return results.
#'   Helpful for disambiguating when a valid user ID is also a valid screen name.
#' @param reverse optional Set this to true if you would like owned lists
#'   to be returned first. See description above for information on
#'   how this parameter works.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return data
list_list <- function(user, reverse = FALSE, token = NULL) {
  query <- "lists/list"
  params <- list(user = user,
                 reverse = reverse)
  names(params)[1] <- .id_type(user)
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  from_js(r)
}

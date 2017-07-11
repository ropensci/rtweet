#' GET users/suggestions¶
#'
#' @param lang optional Restricts the suggested categories to the requested language. The language must
#'   be specified by the appropriate two letter ISO 639-1 representation. Currently
#'   supported languages are provided by the GET help /
#'   languages API request. Unsupported
#'   language codes will receive English (en) results. If you use lang in this
#'   request, ensure you also include it when requesting the GET users / suggestions
#'   / :slug list.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return List of categories which should be passed on to slug.
.user_suggestions <- function(lang = NULL, token = NULL) {
  query <- "users/suggestions"
  token <- check_token(token, query)
  params <- list(lang = lang)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  from_js(r)
}


#' GET users/suggestions/:slug
#'
#' @param slug required The short name of list or a category
#' @param lang optional Restricts the suggested categories to the requested language. The language must
#'   be specified by the appropriate two letter ISO 639-1 representation. Currently
#'   supported languages are provided by the GET help /
#'   languages API request. Unsupported
#'   language codes will receive English (en) results. If you use lang in this
#'   request, ensure you also include it when requesting the GET users / suggestions
#'   / :slug list.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return data
user_suggestions <- function(slug, lang = NULL, token = NULL) {
  query <- "users/suggestions"
  token <- check_token(token, query)
  params <- list(slug = slug, lang = lang)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  from_js(r)
}

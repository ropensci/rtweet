#' get_tokens
#'
#' @description Call function used to load Twitter oauth tokens.
#'   Since Twitter app key should be stored private, you are encouraged
#'   to create and save an R user profile declearing the path to your
#'   Twitter tokens. This allows Tokens to be instantly  [re]loaded
#'   for future sessions. It also makes it easier to write teh card -
#'   allowing internals of the functions t call your tokens for you.
#' @return path
#' @export
get_tokens <- function() {
  if (is.null(.state$twitter_tokens)) {
    load_tokens(twitter_pat())
  }
  .state$twitter_tokens
}

#' create_token
#'
#' @description Sends request to generate oauth 1.0 tokens.Twitter
#'    also allows uers to create user-only (oauth 2.0) access token.
#'    Unlike the 1.0 tokens, oath 2.0 tokens are not at all centered
#'    on a host user. Which means these tokens cannot be used to
#'    send information (follow requests, Twitter statuses, etc.).
#'    If you have no interest in those capabilities, then 2.0 oauth
#'    tokens do offer some higher rate limits. At the time of
#'    writing this, the difference seemed trivial, so I haven't taken
#'    verified oauth 2.0 token method. Consquently, I encourage you
#'    to use 1.0 tokens.
#' @param app Name of user created Twitter application
#' @param consumer_key Application API key
#' @param consumer_secret Application API secret User-owned
#'   app must have \code{Read and write} access level
#'   and \code{Callback URL} of \code{http://127.0.0.1:1410}.
#' @param oauth1.0 logical defaults to TRUE such that requests
#'   are made for 1.0 access tokens. For 2.0 tokens, set to FALSE.
#' @seealso See \url{https://dev.twitter.com/overview/
#'   documentation} for more information on using Twitter's
#'   API.
#' @return twitter oauth 1.0 token
#' @details dependencies: httr
#' @import httr
#' @export
create_token <- function(app, consumer_key, consumer_secret) {
  token <- oauth_app(appname = app,
                   key = consumer_key,
                   secret = consumer_secret)
  token <- oauth1.0_token(oauth_endpoints("twitter"), token)
  token
}

#' twitter_pat
#'
#' @description Write .Rprofile file with path to saved tokens object
#' @return path character vector with path to tokens
#' @export
twitter_pat <- function() {
  pat <- Sys.getenv("TWITTER_PAT")

  if (identical(pat, "")) {
    stop("Please set env var TWITTER_PAT to your
         Twitter personal access token(s)",
         call. = FALSE)
  }
  pat
}

#' load_tokens
#'
#' @description Load twitter tokens from previously saved tokens
#'    (directed internally by previously created path locator files)
#' @return path character vector with path to tokens
load_tokens <- function(pat) {
  load(pat, .state)
}


#' fetch_tokens
#'
#' @description Fetch tokens based on remaining rate limit. Use this
#'   function to cycle through multiple tokens until rate limit
#'   remaining is greater than 0.
#' @param tokens list of oauth tokens
#' @param query character vector, Twitter API query of interest
#' @return token with non-exhausted rate limit
#' @export
fetch_tokens <- function(tokens, query) {
  if (length(tokens) == 0) return(tokens)

  for (i in 1:length(tokens)) {
    token <- tokens[[i]]
    remain <- rate_limit(token, query)
    if (remain[[2]] > 0) break
  }

  token
}



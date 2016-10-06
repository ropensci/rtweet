#' get_tokens
#'
#' @description Call function used to load Twitter oauth tokens.
#'   Since Twitter app key should be stored private, you are encouraged
#'   to create and save an R user profile declaring the path to your
#'   Twitter tokens. This allows Tokens to be instantly  [re]loaded
#'   for future sessions. It also makes it easier to write teh card -
#'   allowing internals of the functions t call your tokens for you.
#' @return path
#' @family tokens
#' @export
get_tokens <- function() {
	if (all(is.null(.state$twitter_tokens), !is.null(.state$twitter_token))) {
		.state$twitter_tokens <- .state$twitter_token
	}
  if (is.null(.state$twitter_tokens)) {
    .state$twitter_tokens <- load_tokens(twitter_pat())
  }
  .state$twitter_tokens
}


#' create_token
#'
#' @description Sends request to generate oauth 1.0 tokens. Twitter
#'   also allows uers to create user-only (oauth 2.0) access token.
#'   Unlike the 1.0 tokens, oath 2.0 tokens are not at all centered
#'   on a host user. Which means these tokens cannot be used to
#'   send information (follow requests, Twitter statuses, etc.).
#'   If you have no interest in those capabilities, then 2.0 oauth
#'   tokens do offer some higher rate limits. At the current time,
#'   the difference given the functions in this package is trivial,
#'   so I have yet to  verified oauth 2.0 token method.
#'   Consequently, I encourage you to use 1.0 tokens.
#' @param app Name of user created Twitter application
#' @param consumer_key Application API key
#' @param consumer_secret Application API secret User-owned
#'   app must have \code{Read and write} access level
#'   and \code{Callback URL} of \code{http://127.0.0.1:1410}.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#'
#' @return Twitter personal access token object
#' @import httr
#' @family tokens
#' @export
create_token <- function(app, consumer_key, consumer_secret) {
  token <- oauth_app(
    appname = app,
    key = consumer_key,
    secret = consumer_secret)

  token <- oauth1.0_token(
    oauth_endpoints("twitter"),
    token)

  token
}

#' fetch_tokens
#'
#' @description Fetches tokens based on remaining rate limit.
#'   Use this function to cycle through multiple access tokens
#'   until rate limit remaining is greater than 0.
#'
#' @param tokens List of oauth tokens
#' @param query character vector, Twitter API query of interest
#' @param sleep logical indicating whether to force system sleep if
#'   rate limit is exhausted. defaults to \code{sleep = FALSE}.
#' @return token with non-exhausted rate limit
#' @keywords internal
#' @noRd
fetch_tokens <- function(tokens, query, sleep = FALSE) {

  if (missing(query)) {
    stop("Must specify Twitter API query of interest",
    	call. = FALSE)
  }

  for (i in tokens) {
    remaining <- rate_limit(i, query)[["remaining"]]

    if (remaining > 0) return(i)
  }

  if (sleep) {
    token <- tokens[[1]]

    reset <- rate_limit(token, query)[["reset"]]

    Sys.sleep(reset[[1]] * 60)

    return(token)

  } else if (remaining == 0) {
    stop("Rate limit exceeded - please wait!",
    	call. = FALSE)
  }

  token
}

is.token <- function(x) {
  any(class(x) == "Token", class(x) == "Token1.0")
}


check_token <- function(token, query = NULL) {

  if (is.null(token)) {
    token <- get_tokens()

    if (!is.null(query)) {
      token <- fetch_tokens(token, query)
    }
  }

  if (is.list(token)) {
    token <- token[[1]]
  }

  if (identical(class(token), "OAuth")) {
    token <- create_token(
      sample(letters, 8),
      token$consumerKey,
      token$consumerSecret)
  }

  if (!is.token(token)) {
    stop("Not a valid access token.", call. = FALSE)
  }

  token
}



twitter_pat <- function() {
  pat <- Sys.getenv("TWITTER_PAT")

  if (identical(pat, "")) {
    if (file.exists(".httr-oauth")) {
      pat <- ".httr-oauth"
    } else {
    	pat <- "system"
      warning(
        paste0("Please set env var TWITTER_PAT to your ",
        	"Twitter personal access token(s)"),
        call. = FALSE)
    }
  }
  pat
}

if_load <- function(x) {
	suppressWarnings(
		tryCatch(load(x),
			error = function(e) (return(FALSE))))
}

#' @importFrom openssl rsa_decrypt
system_tokens <- function() {
  y <- sysdat
	x <- y$tokens
	x[[1]]$app$secret <- rawToChar(rsa_decrypt(y$cipher_appsecret[[1]],
		y$cipher_key))
	x[[2]]$app$secret <- rawToChar(rsa_decrypt(y$cipher_appsecret[[2]],
		y$cipher_key))
	x[[3]]$app$secret <- rawToChar(rsa_decrypt(y$cipher_appsecret[[3]],
		y$cipher_key))
	x[[1]]$credentials$oauth_token_secret <- rawToChar(rsa_decrypt(
		y$cipher_tknsecret[[1]], y$cipher_key))
	x[[2]]$credentials$oauth_token_secret <- rawToChar(rsa_decrypt(
		y$cipher_tknsecret[[2]], y$cipher_key))
	x[[3]]$credentials$oauth_token_secret <- rawToChar(rsa_decrypt(
		y$cipher_tknsecret[[3]], y$cipher_key))
	x
}

load_tokens <- function(pat) {
  if (identical(pat, ".httr-oauth")) {
    .state$twitter_tokens <- readRDS(pat)
  } else if (identical(pat, "system")) {
  	.state$twitter_tokens <- system_tokens()
  } else if (if_load(pat)) {
  	x <- load(pat)
  	.state$twitter_tokens <- get(x)
  } else {
  	.state$twitter_tokens <- readRDS(pat)
  }
	.state$twitter_tokens
}

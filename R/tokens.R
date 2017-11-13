#' Fetching Twitter authorization token(s).
#'
#' Call function used to fetch and load Twitter OAuth tokens.
#' Since Twitter application key should be stored privately, users should save
#' the path to token(s) as an environment variable. This allows Tokens
#' to be instantly [re]loaded in future sessions. See the "tokens"
#' vignette for instructions on obtaining and using access tokens.
#'
#' @return Twitter OAuth token(s) (Token1.0).
#' @details This function will search for tokens using R, internal,
#'   and global environment variables (in that order).
#' @examples
#'
#' \dontrun{
#' ## fetch default token(s)
#' token <- get_tokens()
#'
#' ## print token
#' token
#'
#' }
#'
#' @family tokens
#' @export
get_tokens <- function() {
  if (all(is.null(.state$twitter_tokens),
    !is.null(.state$twitter_token))) {
    .state$twitter_tokens <- .state$twitter_token
  }
  if (is.null(.state$twitter_tokens)) {
    .state$twitter_tokens <- load_tokens(twitter_pat())
  }
  if (is.null(.state$twitter_tokens)) {
    .state$twitter_tokens <- rtweet_token()
  }
  .state$twitter_tokens
}

#' @export
#' @rdname get_tokens
get_token <- function() get_tokens()

#' Creating Twitter authorization token(s).
#'
#' @description Sends request to generate OAuth 1.0 tokens. Twitter
#'   also allows users to create user-only (OAuth 2.0) access token.
#'   Unlike the 1.0 tokens, OAuth 2.0 tokens are not at all centered
#'   on a host user. Which means these tokens cannot be used to send
#'   information (follow requests, Twitter statuses, etc.).  If you
#'   have no interest in those capabilities, then 2.0 OAuth tokens do
#'   offer some higher rate limits. At the current time, the
#'   difference given the functions in this package is trivial, so I
#'   have yet to verified OAuth 2.0 token method.  Consequently, I
#'   encourage you to use 1.0 tokens.
#' @param app Name of user created Twitter application
#' @param consumer_key Application API key
#' @param consumer_secret Application API secret User-owned
#'   application must have \code{Read and write} access level and
#'   \code{Callback URL} of \code{http://127.0.0.1:1410}.
#' @param set_renv Logical indicating whether to save the created token
#'   as the default environment twitter token variable. Defaults to FALSE.
#'   If TRUE, the token is saved to user's home directory as
#'   ".rtweet_token.rds" (or, if that already exists, then
#'   .rtweet_token1.rds or .rtweet_token2.rds, etc.) and the path to the
#'   token to said token is then set in the user's .Renviron file and re-
#'   read to start being used in current active session.
#' @seealso
#'   \url{https://developer.twitter.com/en/docs/basics/authentication/overview/oauth}
#'
#' @return Twitter OAuth token(s) (Token1.0).
#' @importFrom httr oauth_app oauth1.0_token oauth_endpoints
#' @family tokens
#' @export
create_token <- function(app = "mytwitterapp",
                         consumer_key,
                         consumer_secret,
                         set_renv = TRUE) {
  token <- oauth_app(
    appname = app,
    key = gsub(" ", "", consumer_key),
    secret = gsub(" ", "", consumer_secret)
  )
  token <- oauth1.0_token(
    oauth_endpoints("twitter"),
    token,
    cache = FALSE
  )
  if (set_renv) {
    pathtotoken <- uq_filename(file.path(home(), ".rtweet_token.rds"))
    saveRDS(token, file = pathtotoken)
    set_renv("TWITTER_PAT" = pathtotoken)
  }
  token
}


has_ext <- function(x) {
  stopifnot(length(x) == 1L)
  x <- basename(x)
  grepl("[[:alnum:]]{1,}\\.[[:alpha:]]{1,}$", x)
}

only_ext <- function(x) {
  if (has_ext(x)) {
    gsub(".*(?=\\.)", "", x, perl = TRUE)
  } else {
    ""
  }
}

no_ext <- function(x) {
  if (has_ext(x)) {
    gsub("(?<=[[:alnum:]]{1})\\..*(?!=\\.)", "", x, perl = TRUE)
  } else {
    x
  }
}

paste_before_ext <- function(x, p) {
  paste0(no_ext(x), p, only_ext(x))
}


uq_filename <- function(file_name) {
  stopifnot(is.character(file_name) && length(file_name) == 1L)
  if (file.exists(file_name)) {
    files <- list.files(dirname(file_name), all.files = TRUE, full.names = TRUE)
    file_name <- paste_before_ext(file_name, 1:1000)
    file_name <- file_name[!file_name %in% files][1]
  }
  file_name
}


fetch_tokens <- function(tokens, query, sleep = FALSE) {
  remaining <- NA_real_
  if (missing(query)) {
    stop("Must specify Twitter API query of interest",
      call. = FALSE)
  }
  if (is.list(tokens)) {
    for (i in seq_along(tokens)) {
      remaining <- rate_limit(token = tokens[[i]],
        query)[["remaining"]]
      if (isTRUE(remaining > 0)) return(tokens[[i]])
    }
  } else {
    remaining <- rate_limit(token = tokens,
      query)[["remaining"]]
    if (isTRUE(remaining > 0)) return(tokens)
  }

  if (sleep) {
    token <- tokens[[1]]
    reset <- rate_limit(token, query)[["reset"]]
    Sys.sleep(reset[[1]] * 60)
    return(token)
  } else {
    stop("Rate limit exceeded - please wait!",
      call. = FALSE)
  }
  token
}

is.token <- function(x) {
  lgl <- FALSE
  lgl <- all(
    any(c("token", "token1.0", "R6") %in% class(x)),
    grepl("api.twitter", x[['endpoint']][['request']], ignore.case = TRUE)
  )
  lgl
}

rate_limit_used <- function(x) {
  x$used <- x$limit - x$remaining
  x <- x[, c("query", "limit", "remaining", "used", "reset", "reset_at")]
  x[order(x$used, decreasing = TRUE), ]
}


check_token <- function(token, query = NULL) {
  if (is.null(token)) {
    token <- get_tokens()
  }
  if (is.token(token)) {
    return(token)
  }
  if (identical(class(token), "OAuth")) {
    token <- create_token(
      sample(letters, 8),
      token$consumerKey,
      token$consumerSecret)
  }
  if (!is.null(query)) {
    token <- fetch_tokens(token, query)
  }
  if (is.list(token)) {
    token <- token[[1]]
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
    } else if (file.exists("twitter_tokens")) {
      pat <- "twitter_tokens"
    } else if (file.exists("twitter_token")) {
      pat <- "twitter_token"
    } else if (file.exists("tokens")) {
      pat <- "tokens"
    } else if (file.exists("token")) {
      pat <- "token"
    } else {
      pat <- "system"
    }
  }
  pat
}

if_load <- function(x) {
  lgl <- TRUE
  lgl <- suppressWarnings(
    tryCatch(load(x),
      error = function(e) (return(FALSE))))
  if (is.null(lgl)) return(FALSE)
  if (identical(length(lgl), 0L)) return(FALSE)
  if (identical(lgl, FALSE)) return(FALSE)
  TRUE
}

if_rds <- function(x) {
  lgl <- TRUE
  lgl <- suppressWarnings(
    tryCatch(readRDS(x),
      error = function(e) (return(FALSE))))
  if (is.null(lgl)) return(FALSE)
  if (identical(length(lgl), 0L)) return(FALSE)
  if (identical(lgl, FALSE)) return(FALSE)
  TRUE
}

#' @importFrom openssl rsa_decrypt
system_tokens <- function() {
  y <- sysdat
  x <- y$tokens
  x[[1]]$app$secret <- rawToChar(
    openssl::rsa_decrypt(y$cipher_appsecret[[1]],
      y$cipher_key))
  x[[2]]$app$secret <- rawToChar(
    openssl::rsa_decrypt(y$cipher_appsecret[[2]],
      y$cipher_key))
  x[[3]]$app$secret <- rawToChar(
    openssl::rsa_decrypt(y$cipher_appsecret[[3]],
      y$cipher_key))
  x[[1]]$credentials$oauth_token_secret <- rawToChar(
    openssl::rsa_decrypt(
      y$cipher_tknsecret[[1]], y$cipher_key))
  x[[2]]$credentials$oauth_token_secret <- rawToChar(
    openssl::rsa_decrypt(
      y$cipher_tknsecret[[2]], y$cipher_key))
  x[[3]]$credentials$oauth_token_secret <- rawToChar(
    openssl::rsa_decrypt(
      y$cipher_tknsecret[[3]], y$cipher_key))
  x
}

global_token_finder <- function(env = globalenv()) {
  x <- NULL
  objs <- ls(envir = env)
  if ("twitter_tokens" %in% objs) {
    x <- get_tokens_global("twitter_tokens")
  } else if ("twitter_token" %in% objs) {
    x <- get_tokens_global("twitter_token")
  } else if ("tokens" %in% objs) {
    x <- get_tokens_global("tokens")
  } else if ("token" %in% objs) {
    x <- get_tokens_global("token")
  } else if (any(grepl("token", objs, ignore.case = TRUE))) {
    tkn <- grep("token", objs, ignore.case = TRUE, value = TRUE)
    if (identical(length(tkn), 1L)) {
      x <- get_tokens_global(tkn)
    } else if ("twitter_tokens" %in% tolower(tkn)) {
      x <- get_tokens_global(tkn[tolower(tkn) == "twitter_tokens"])
    } else if ("twitter_token" %in% tolower(tkn)) {
      x <- get_tokens_global(tkn[tolower(tkn) == "twitter_tokens"])
    } else if ("tokens" %in% tolower(tkn)) {
      x <- get_tokens_global(tkn[tolower(tkn) == "tokens"])
    } else if ("token" %in% tolower(tkn)) {
      x <- get_tokens_global(tkn[tolower(tkn) == "token"])
    }
  }
  x
}

get_tokens_global <- function(x, env = globalenv()) {
  if (is.character(x)) {
    eval(parse(text = x), envir = env)
  } else {
    eval(x, envir = env)
  }
}

load_tokens <- function(pat, env = globalenv()) {
  pat <- strsplit(pat, "\\,|\\;")[[1]]
  x <- Map("load_tokens_", pat = pat, MoreArgs = list(env = env))
  if (is.list(x) && length(x) == 1L) {
    x <- x[[1]]
  }
  .state$twitter_tokens <- x
  .state$twitter_tokens
}

load_tokens_ <- function(pat, env = globalenv()) {
  if (identical(pat, ".httr-oauth")) {
    readRDS(pat)
  } else if (identical(pat, "system")) {
    rtweet_token()
  } else if (if_load(pat)) {
    x <- load(pat)
    get(x)
  } else if (if_rds(pat)) {
    readRDS(pat)
  } else if (any(grepl("token",
    ls(envir = env), ignore.case = TRUE))) {
    global_token_finder()
  } else {
    rtweet_token()
  }
}

## home user
home_user <- function() {
  eval(call("home_user_"))
}

home_user_ <- function() {
  if (!exists(".state")) {
    .state <- new.env()
  }
  if (exists(".user", envir = .state)) {
    return(get(".user", envir = .state))
  }
  user <- Sys.getenv("TWITTER_SCREEN_NAME")
  if (!identical(user, "")) {
    assign(".user", user, envir = .state)
    return(user)
  }
  ## ask user for screen name
  user <- readline("What is your screen name on Twitter?")
  ## remove at sign
  user <- gsub("@", "", user)
  ## save as environment variable
  message("Saving your Twitter screen name as environment variable")
  cat(
    paste0("TWITTER_SCREEN_NAME=", user),
    fill = TRUE,
    file = file.path(normalizePath("~"), ".Renviron"),
    append = TRUE
  )
  ## store in pkg environment
  assign(".user", user, envir = .state)
  ## return screen name
  invisible(user)
}


## validate user token
validate_token <- function() {
  token <- tryCatch(get_tokens(), error = function(e) NULL)
  if (is.null(token) || !inherits(token, "Token")) {
    stop(paste0(
      "Could not find token. Please save path to token associated with @",
      home_user(), "'s account as the \"TWITTER_PAT\" environment variable."),
      call. = FALSE)
  }
  token_user <- token[["credentials"]][["screen_name"]]
  if (!identical(home_user(), token_user)) {
    stop(paste0(
      "Invalid token. This token belongs to @",
      token_user, " and not @", home_user(), ".\n",
      "Please save path to token associated with @", home_user(),
      "'s account as the \"TWITTER_PAT\" environment variable."),
      call. = FALSE)
  }
  TRUE
}


rtweet_app_ <- function() {
  tkn <- system_tokens()[[3]]
  httr::oauth_app(
    tkn$app$appname, tkn$app$key, tkn$app$secret
  )
}


rtweet_app <- function() {
  if (exists(".rtweet_token") &&
        exists("app", envir = get(".rtweet_token"))) {
    app <- get("app", envir = get(".rtweet_token"))
  } else {
    .rtweet_token  <- new.env()
    app <- rtweet_app_()
    assign("app", app, envir = .rtweet_token)
  }
  app
}

rtweet_token_ <- function() {
  if (interactive()) {
    app <- rtweet_app()
    httr::oauth1.0_token(
      httr::oauth_endpoints("twitter"),
      app, cache = FALSE
    )
  } else {
    system_tokens()[[1]]
  }
}

rtweet_token <- function() {
  if (exists(".rtweet_token") &&
        exists("token", envir = get(".rtweet_token"))) {
    token <- get("token", envir = get(".rtweet_token"))
  } else {
    token <- rtweet_token_()
    .rtweet_token  <- new.env()
    assign("token", token, envir = .rtweet_token)
  }
  if (identical(Sys.getenv("TWITTER_PAT"), "")) {
    pathtotoken <- uq_filename(file.path(home(), ".rtweet_token.rds"))
    saveRDS(token, file = pathtotoken)
    set_renv("TWITTER_PAT" = pathtotoken)
  }
  token
}

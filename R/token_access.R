rtweet_app_ <- function() {
  tkn <- system_tokens()[[3]]
  httr::oauth_app(
    tkn$app$appname, tkn$app$key, tkn$app$secret
  )
}


rtweet_app <- function() {
  if (exists(".rtweet_token") && exists("app", envir = get(".rtweet_token"))) {
    app <- get("app", envir = get(".rtweet_token"))
  } else {
    .rtweet_token  <<- new.env()
    app <- rtweet_app_()
    assign("app", app, envir = get(".rtweet_token"))
  }
  app
}

rtweet_token_ <- function() {
  app <- rtweet_app()
  httr::oauth1.0_token(
    httr::oauth_endpoints("twitter"),
    app, cache = FALSE
  )
}

rtweet_token <- function() {
  if (exists(".rtweet_token") && exists("token", envir = get(".rtweet_token"))) {
    token <- get("token", envir = get(".rtweet_token"))
  } else {
    token <- rtweet_token_()
    assign("token", token, envir = get(".rtweet_token"))
  }
  token
}

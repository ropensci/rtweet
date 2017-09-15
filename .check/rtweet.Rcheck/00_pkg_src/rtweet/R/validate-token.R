## home user
home_user <- function() {
  eval(call("home_user_"))
}

home_user_ <- function() {
  if (!exists(".state")) {
    .state <<- new.env()
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

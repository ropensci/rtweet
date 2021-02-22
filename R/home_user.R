

# home_user ---------------------------------------------------------------

## home user
home_user <- function() {
  eval(call("home_user_"))
}

authenticating_user_name <- function(token = NULL) {
  ## check token
  token <- check_token(token)
  ## API path
  query <- "account/verify_credentials"
  ## set params
  params <- list(
    include_entities = FALSE,
    skip_status = TRUE,
    include_email = FALSE
  )
  ## build req
  url <- make_url(query = query, param = params)
  ## send request
  r <- TWIT(get = TRUE, url = url, token)
  ## if not 200 then return NULL
  if (r$status_code != 200L) {
    return(NULL)
  }
  httr::content(r)$screen_name
}

home_user_ <- function() {
  ## environment
  if (!exists(".state")) {
    .state <- new.env()
  }
  ## if user name has already been established
  if (exists(".user", envir = .state)) {
    return(get(".user", envir = .state))
  }
  ## look in environment vars
  user <- Sys.getenv("TWITTER_SCREEN_NAME")
  if (!identical(user, "")) {
    assign(".user", user, envir = .state)
    return(user)
  }
  ## look up via creds request
  user <- authenticating_user_name()

  ## if that returned a valid screen name, set it and return
  if (length(user) > 0 && !identical(user, "")) {
    assign(".user", user, envir = .state)
    Sys.setenv(TWITTER_SCREEN_NAME = user)
    return(user)
  }

  ## if interactive: ask user for screen name
  if (interactive()) {
    user <- readline("What is your screen name on Twitter?")
    ## remove at sign, spaces, or quotes
    user <- gsub("@|\\s|\"|'", "", user)
    ## save as environment variable
    message("Saving your Twitter screen name as environment variable")
    Sys.setenv(TWITTER_SCREEN_NAME = user)
    ## store in pkg environment
    assign(".user", user, envir = .state)
    ## return screen name
    return(user)
  }
  stop(paste0("cannot determine authenticating user.\nTo set manually, ",
    "enter {screen_name} and append this to your .Renviron file:",
    "\nTWITTER_SCREEN_NAME={screen_name} "))
}



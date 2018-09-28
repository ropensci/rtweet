
api_access_level <- function(token = NULL) {
  token <- check_token(token)
  r <- TWIT(get = TRUE, make_url(restapi = TRUE, "account/settings"), token)
  warn_for_twitter_status(r)
  if ("headers" %in% names(r) && "x-access-level" %in% names(r$headers)) {
    r$headers$`x-access-level`
  } else {
    r
  }
}

is_read_write_directmessages <- function(token = NULL) {
  a <- api_access_level(token)
  identical("read-write-directmessages", a)
}

is_read_write <- function(token = NULL) {
  a <- api_access_level(token)
  identical("read-write", a)
}

is_read_only <- function(token = NULL) {
  a <- api_access_level(token)
  identical("read", a)
}


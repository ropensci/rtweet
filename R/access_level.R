api_access_level <- function(token = NULL) {
  r <- TWIT_get(token, "/1.1/account/settings", parse = FALSE)

  if ("headers" %in% names(r) && "x-access-level" %in% names(r$headers)) {
    r$headers$`x-access-level`
  } else {
    r
  }
}

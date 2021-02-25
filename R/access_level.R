api_access_level <- function(token = NULL) {
  r <- TWIT_get(token, "account/settings", parse = FALSE)

  if ("headers" %in% names(r) && "x-access-level" %in% names(r$headers)) {
    r$headers$`x-access-level`
  } else {
    r
  }
}

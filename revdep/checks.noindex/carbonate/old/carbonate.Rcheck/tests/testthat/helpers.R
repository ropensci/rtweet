# taken from https://github.com/tidyverse/reprex/blob/master/tests/testthat/helper.R

## used to make clipboard unavailable locally
# Sys.setenv("CLIPBOARD_AVAILABLE" = TRUE)
# Sys.setenv("CLIPBOARD_AVAILABLE" = FALSE)

NOT_CRAN <- Sys.getenv("NOT_CRAN", unset = "")

ON_CRAN <- identical(NOT_CRAN, "") || identical(tolower(NOT_CRAN), "false")

if (ON_CRAN) {
  Sys.setenv("CLIPBOARD_AVAILABLE" = FALSE)
}

clipboard_available <- function() {

  if (Sys.getenv("CLIPBOARD_AVAILABLE", unset = TRUE)) {
    return(clipr::clipr_available())
  }
  FALSE
}

skip_if_no_clipboard <- function() {
  if (!clipboard_available()) {
    testthat::skip("System clipboard is not available - skipping test.")
  }
  return(invisible(TRUE))
}
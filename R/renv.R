
#' Saves Twitter token as an R environment variable
#'
#' @param token Token object created via \code{\link{create_token}}.
#' @param home_dir Path to user's home directory.
#' @examples
#' \dontrun{
#'
#' ## name of app you created (replace rtweet_token with name of your app)
#' appname <- "rtweet_token"
#'
#' ## api key (example below is not a real key)
#' key <- "XYznzPFOFZR2a39FwWKN1Jp41"
#'
#' ## api secret (example below is not a real key)
#' secret <- "CtkGEWmSevZqJuKl6HHrBxbCybxI1xGLqrD5ynPd9jG0SoHZbD"
#'
#' ## create a token, storing it as object 'twitter_token'
#' twitter_token <- create_token(
#'   app = appname,
#'   consumer_key = key,
#'   consumer_secret = secret
#' )
#'
#' ## save as R environment variable
#' save_token2renv(twitter_token)
#'
#' }
#'
#' @export
save_token2renv <- function(token, home_dir = "~/") {
  ## check token
  stopifnot(is.token(token))
  ## save token
  home_dir <- normalizePath("~")
  tokenpath <- file.path(home_dir, ".twitter_token.rds")
  saveRDS(token, tokenpath)
  ## check and update .Renviron file
  renv <- file.path(home_dir, ".Renviron")
  check_renv(renv)
  set_renv(`TWITTER_PAT` = tokenpath)
}

check_renv <- function(path) {
  if (!file.exists(path)) {
    return(invisible())
  }
  con <- file(path)
  x <- readLines(con, warn = FALSE)
  close(con)
  x <- clean_renv(x)
  x <- paste(x, collapse = "\n")
  cat(x, file = path, fill = TRUE)
  invisible()
}

clean_renv <- function(x) {
  stopifnot(is.character(x))
  ## remove incomplete vars
  x <- grep("=$", x, value = TRUE, invert = TRUE)
  ## split lines with double entries and fix into new vars
  xs <- strsplit(x, "=")
  vals <- sub("[^=]*=", "", x)
  kp <- !grepl("[[:upper:]]{1,}=", vals)
  if (sum(!kp) > 0L) {
    m <- regexpr("[[:upper:]_]{1,}(?==)", x[!kp], perl = TRUE)
    newlines <- paste0(regmatches(x[!kp], m), "=", sub(".*=", "", x[!kp]))
    x <- x[kp]
    x[(length(x) + 1):(length(x) + length(newlines))] <- newlines
  }
  ## remove double entries
  xs <- strsplit(x, "=")
  kp <- !duplicated(sapply(xs, "[[", 1))
  x <- x[kp]
  x
}

set_renv <- function(..., path) {
  dots <- list(...)
  nms <- names(dots)
  stopifnot(length(nms) > 0L)
  stopifnot(length(dots) == length(nms))
  x <- paste0(nms, "=", dots, collapse = "\n")
  check_renv(path)
  cat(x, file = path, fill = TRUE, append = TRUE)
  readRenviron(path)
}

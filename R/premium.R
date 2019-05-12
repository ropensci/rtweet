
premium_api <- function(...) {
  dots <- c(...)
  if (length(dots) == 1) {
    dots <- dots[[1]]
  }
  path <- grep("30day|fullarchive", dots, value = TRUE)
  env_name <- grep("30day|fullarchive", dots, value = TRUE, invert = TRUE)
  list(path = path, env_name = env_name)
}

format_from_to_date <- function(x = NULL) {
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) > 1L) {
    stop("Can only provide one value to fromDate/toDate", call. = FALSE)
  }
  if (is.character(x) && grepl("-", x) && nchar(x) > 11) {
    x <- as.POSIXct(x)
  }
  if (is.character(x) && grepl("-", x)) {
    x <- as.Date(x)
  }
  if (inherits(x, "Date")) {
    x <- as.POSIXct(x)
  }
  if (inherits(x, "POSIXct")) {
    x <- format(x, "%Y%m%d%H%M")
  }
  x
}


search_fullarchive <- function(q, n = 100, fromDate = NULL, toDate = NULL,
  env_name = NULL, token = NULL) {
  token <- check_token(token)
  if (!length(get_app_secret(token))) {
    stop(paste0("This token does not have an app secret and therefore cannot ",
    "create a bearer token"), call. = FALSE)
  }
  token <- bearer_token(token)
  fromDate <- format_from_to_date(fromDate)
  toDate <- format_from_to_date(toDate)
  if (is.null(env_name)) {
    stop("Must provide dev environment name")
  }
  search_tweets(q,
    fromDate = fromDate,
    toDate = toDate,
    premium = premium_api("fullarchive", env_name),
    parse = TRUE, n = n,
    token = token)
}

#rstats <- search_fullarchive("rstats", n = 1000,
#  fromDate = "201511010000", toDate = "201512220000")


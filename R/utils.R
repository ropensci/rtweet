
##----------------------------------------------------------------------------##
##                                  HTTP GET                                  ##
##----------------------------------------------------------------------------##

#' TWIT
#'
#' @description Base function responsible for formulating GET and
#'   POST requests to Twitter API's.
#'
#' @param get Logical with the default, `get = TRUE`,
#'   indicating whether the provided url should be passed along via
#'   a GET or POST request.
#' @param url Character vector designed to operate like
#'   parse_url and build_url functions in the httr package.
#'   The easiest way to do this is to work through
#'   the call-specific functions as they are designed to simplify
#'   the process. However, if one were interested in reverse-
#'   engingeering such a thing, I would recommend checking out
#'   `make_url`.
#' @param \dots Further named parameters, such as config, token,
#'   etc, passed on to modify_url in the httr package.
#' @note Occasionally Twitter does recommend using POST requests
#'   for data retrieval calls. This is usually the case when requests
#'   can involve long strings (containing up to 100 user_ids). For
#'   the most part, or at least for any function-specific requests
#'   (e.g., `get_friends`, take reflect these changes.
#' @return json response object
#' @importFrom httr GET POST timeout write_disk progress
#' @keywords internal
#' @noRd
TWIT <- function(get = TRUE, url, ...) {
  if (get) {
    GET(url, ...)
  } else {
    POST(url, ...)
  }
}

TWIT_get <- function(token, api, params = NULL, parse = TRUE, ..., host = "api.twitter.com") {
  resp <- TWIT_method("GET", 
    token = token, 
    api = api,
    params = params,
    ...,
    host = host
  )
  
  if (parse) {
    from_js(resp)
  } else {
    resp
  }
}

TWIT_post <- function(token, api, params = NULL, ..., host = "api.twitter.com") {
  TWIT_method("POST", 
    token = token, 
    api = api,
    params = params,
    ...,
    host = host
  )
}

TWIT_method <- function(method, token, api, 
                        params = NULL, 
                        host = "api.twiter.com",
                        ...) {
  # need scipen to ensure large IDs are not displayed in scientific notation
  # need ut8-encoding for the comma separated IDs
  withr::local_options(scipen = 14, encoding = "UTF-8")

  token <- check_token(token)
  url <- paste0("https://", host, "/1.1/", api, ".json")
  
  resp <- switch(method,
    GET = httr::GET(url, query = params, token, ...),
    POST = httr::POST(url, body = params, token, ...),
    stop("Unsupported method", call. = FALSE)
  )
  check_status(resp)
  resp
}

# https://developer.twitter.com/en/support/twitter-api/error-troubleshooting
check_status <- function(x) {
  if (!httr::http_error(x)) {
    return()
  }
  
  parsed <- from_js(x)
  
  stop(
    "Twitter API failed [", x$status_code, "]\n",
    paste0(" * ", parsed$errors$message, " (", parsed$errors$code, ")"),
    call. = FALSE
  )
}

#' make_url
#'
#' @param restapi logical Default `restapi = TRUE`
#'   indicates the provided URL components should be
#'   specify Twitter's REST API. Set this to FALSE if you wish
#'   to make a request URL designed for Twitter's streaming api.
#' @param query Twitter's subsetting/topic identifiers.
#'   Although the httr package refers to this as "path",
#'   query is used here to maintain consistency with
#'   Twitter API's excellent documentation.
#' @param param Additional parameters (arguments) passed
#'   along. If none, NULL (default).
#' @return URL used in httr call.
#' @keywords internal
#' @noRd
make_url <- function(restapi = TRUE, query, param = NULL) {
  if (restapi) {
    hostname <- "api.twitter.com"
  } else {
    hostname <- "stream.twitter.com"
  }
  structure(
    list(
      scheme = "https",
      hostname = hostname,
      port = NULL,
      path = paste0("1.1/", query, ".json"),
      query = param,
      params = NULL,
      fragment = NULL,
      username = NULL,
      password = NULL),
    class = "url")
}

##----------------------------------------------------------------------------##
##                                 format_date                                ##
##----------------------------------------------------------------------------#

format_date <- function(x, tz = "UTC") {
  o <- tryCatch(
    as.POSIXct(
      x,
      format = "%a %b %d %T %z %Y",
      tz = tz
    ),
    error = function(e) return(NULL)
  )
  if (any(is.null(o), all(is.na.quiet(o)))) {
    o <- tryCatch(as.POSIXct(
      x,
      format = "%a %b %d %H:%M:%S %z %Y",
      tz = tz,
      origin = "1970-01-01"),
      error = function(e) return(NULL))
  }
  if (any(is.null(o), all(is.na.quiet(o)))) {
    o <- tryCatch(as.POSIXct(
      x,
      format = "%a %b %d %H:%M:%S %z %Y"),
      error = function(e) return(NULL))
  }
  if (any(is.null(o), all(is.na.quiet(o)))) {
    curLocale <- Sys.getlocale("LC_TIME")
    on.exit(
      Sys.setlocale("LC_TIME", curLocale)
      ##add = TRUE
    )
    Sys.setlocale("LC_TIME", "C")

    o <- tryCatch(as.POSIXct(
      x,
      tz = tz,
      format = "%a, %d %b %Y %H:%M:%S +0000"),
      error = function(e) return(NULL)
    )
  }
  if (any(is.null(o), all(is.na.quiet(o)))) {
    o <- tryCatch(as.POSIXct(
      x, tz = tz,
      format = "%a %b %d %H:%M:%S +0000 %Y"),
      error = function(e) return(NULL))
  }
  if (any(is.null(o), all(is.na.quiet(o)))) {
    o <- tryCatch(as.POSIXct(
      x, format = "%a %b %d %H:%M:%S %z %Y"),
      error = function(e) return(NULL))
  }
  if (any(is.null(o), all(is.na.quiet(o)))) {
    o <- x
  }
  o
}


##----------------------------------------------------------------------------##
##                            fetch/return features                           ##
##----------------------------------------------------------------------------##

go_get_var <- function(x, ..., expect_n = NULL) {
  vars <- c(...)
  success <- FALSE
  for (i in vars) {
    if (!is.recursive(x)) break
    if (has_name_(x, i)) {
      x <- x[[i]]
      if (i == vars[length(vars)]) {
        success <- TRUE
      }
    } else if (any_recursive(x) && any(sapply(x, has_name_, i))) {
      kp <- sapply(x, has_name_, i)
      x <- x[kp]
      x <- lapply(x, "[[", i)
      if (i == vars[length(vars)]) {
        success <- TRUE
      }
    }
  }
  if (!success && is.null(expect_n)) {
    return(NULL)
  }
  if (any_recursive(x) && is.null(expect_n)) {
    return(x)
  }
  x <- unlist(x)
  if (!is.null(expect_n) && length(x) < expect_n) {
      x <- c(x, rep(NA, expect_n - length(x)))
  }
  x
}

all_uq_names <- function(x) {
  unique(unlist(lapply(x, names)))
}

return_last <- function(x, n = 1) {
  x[length(x) - seq_len(n) + 1]
}

last <- function(x) {
  x[[length(x)]]
}

##----------------------------------------------------------------------------##
##                                 check data                                 ##
##----------------------------------------------------------------------------##

has_name_ <- function(x, name) isTRUE(name %in% names(x))

any_recursive <- function(x) {
  if (!is.recursive(x)) {
    return(FALSE)
  }
  any(vapply(x, is.recursive, logical(1)))
}

is_response <- function(x) {
  inherits(x, "response")
}

is_json <- function(x) {
  grepl("application/json", x$headers[["content-type"]])
}

is.na.quiet <- function(x) {
  suppressWarnings(is.na(x))
}

is_n <- function(n) {
  if (is.character(n)) {
    n <- suppressWarnings(as.numeric(n))
  }
  length(n) == 1L && is.numeric(n) && !is.na(n) && n > 0L
}

maybe_n <- function(x) {
  if (is.character(x)) {
    x <- suppressWarnings(as.numeric(x))
  }
  length(x) == 1L && is.numeric(x) && !is.na(x)
}

is_url <- function(url) {
  url_names <- c("scheme", "hostname",
    "port", "path", "query")
  if (all(length(url) > 1, is.list(url),
    url_names %in% names(url))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


##----------------------------------------------------------------------------##
##                                  wranglers                                 ##
##----------------------------------------------------------------------------##



#' @importFrom jsonlite fromJSON
from_js <- function(rsp) {
  stopifnot(is_response(rsp))
  if (!is_json(rsp)) {
    stop("API did not return json", call. = FALSE)
  }
  rsp <- httr::content(rsp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(rsp)
}

na_omit <- function(x) {
  if (is.atomic(x)) {
    x[!is.na(x)]
  } else {
    x[!vapply(x, function(x) isTRUE(is.na(x)), FUN.VALUE = logical(1))]
  }
}


##----------------------------------------------------------------------------##
##                            user type classifers                            ##
##----------------------------------------------------------------------------##

.ids_type <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- .id_type(x)
  if (length(unique(x)) > 1) {
    stop("users must be user_ids OR screen_names, not both.")
  }
  unique(x)
}

.id_type <- function(x) {
  if (is_screen_name(x)) {
    return("screen_name")
  }
  if (is_user_id(x)) {
    return("user_id")
  }
  x <- suppressWarnings(is.na(as.numeric(x)))
  if (length(unique(x)) > 1) {
    return("screen_name")
  }
  x <- unique(x)
  if (x) {
    return("screen_name")
  } else {
    return("user_id")
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a

##----------------------------------------------------------------------------##
##                                require pkgs                                ##
##----------------------------------------------------------------------------##

try_require <- function(pkg, f = NULL) {
  if (is.null(f)) {
    f <- "this action"
  } else {
    f <- paste0("`", f, "`")
  }

  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible())
  }

  stop(paste0("Package `", pkg, "` required for ", f , ".\n",
    "Please install and try again."), call. = FALSE)
}


is_installed <- function(pkg, warn = NULL, stop = NULL) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(TRUE))
  }
  if (!is.null(warn)) {
    warning(warn, immediate. = TRUE, call. = FALSE)
  }
  if (!is.null(stop)) {
    stop(stop, call. = FALSE)
  }
  invisible(FALSE)
}



r_t_c <- function(x) {
  httpuv::rawToBase64(x)
}

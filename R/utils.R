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

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")  
}
is_dev_mode <- function() {
  exists(".__DEVTOOLS__", .getNamespace("rtweet"))
}

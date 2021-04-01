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

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")  
}
is_dev_mode <- function() {
  exists(".__DEVTOOLS__", .getNamespace("rtweet"))
}
is_rcmd_check <- function() {
  identical(Sys.getenv("RTESTS"), "true")  
}

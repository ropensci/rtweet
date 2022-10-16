##----------------------------------------------------------------------------##
##                                 format_date                                ##
##----------------------------------------------------------------------------#

format_date <- function(x, format = "%a %b %d %T %z %Y") {
  locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  as.POSIXct(x, format = format)
}


convert_tz <- function(x, tz) {
  as.POSIXct(as.POSIXlt(x, tz = tz))
}
##----------------------------------------------------------------------------##
##                            fetch/return features                           ##
##----------------------------------------------------------------------------##

last <- function(x) {
  x[[length(x)]]
}

##----------------------------------------------------------------------------##
##                                 check data                                 ##
##----------------------------------------------------------------------------##

has_name_ <- function(x, name) isTRUE(name %in% names(x))

has_name_children <- function(x, name, children) {
  has_name_(x, name) && has_name_(x[[name]], children)
}

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
  identical(Sys.getenv("TESTTHAT"), "true") && requireNamespace("testthat", quietly = TRUE)
}
is_dev_mode <- function() {
  exists(".__DEVTOOLS__", .getNamespace("rtweet"))
}

is_rcmd_check <- function() {
  identical(Sys.getenv("RTESTS"), "true")
}

is_developing <- function() {
  is_testing() || (is_dev_mode() %||% is_rcmd_check())
}


release_bullets <- function() {
  c("Run vignette/precompute.R and move images")
}

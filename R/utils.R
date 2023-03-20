# format_date ####

format_date <- function(x, format = "%a %b %d %T %z %Y") {
  locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  as.POSIXct(x, format = format)
}

format_date_precison <- function(x) {
  strptime(x, tz = "UTC", format = "%FT%H:%M:%OS")
}

convert_tz <- function(x, tz) {
  as.POSIXct(as.POSIXlt(x, tz = tz))
}

format_iso_date <- function(x) {
  # YYYY-MM-DDTHH:mm:ssZ (ISO 8601/RFC 3339)
  # %F        T%T      Z
  format_date(x, "%FT%TZ")
}

# check data ####

has_name_ <- function(x, name) isTRUE(name %in% names(x))

has_name_children <- function(x, name, children) {
  has_name_(x, name) && has_name_(x[[name]], children)
}

is_n <- function(n) {
  if (!is.numeric(n)) {
    n <- suppressWarnings(as.numeric(n))
  }
  length(n) == 1L && !is.na(n) && n > 0L && (is.infinite(n) || n %% 1 == 0)
}

maybe_n <- function(n) {
  if (is.character(n)) {
    n <- suppressWarnings(as.numeric(n))
  }
  length(n) == 1L && is.numeric(n) && !is.na(n)
}

is_logical <- function(x) {
  isFALSE(x) || isTRUE(x)
}

check_interval <- function(value, min, max) {
  if (value < min) {
    return(min)
  }
  if (value > max) {
    return(max)
  }
  value
}


is_id <- function(x) {
  is.character(x) && all(nchar(x) >= 18) && all(grepl("[0-9]{18,}", x)) || is.numeric(x)
}

is_user_id <- function(x, call = caller_env()) {
    is.character(x) && all(nchar(x) >= 8) && all(grepl("[0-9]{8,}", x)) || is.numeric(x)
}

is_list_id <- function(x, call = caller_env()) {
    is.character(x) && all(nchar(x) >= 17) && all(grepl("[0-9]{17,}", x)) || is.numeric(x)
}

# check environment ####

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
  c("Run vignette/precompute.R",
    "Check spelling with: `spelling::spell_check_package()`",
    "Run manual tests.")
}

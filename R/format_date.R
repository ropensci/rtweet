#' converts created_at character vector to class posixct
#'
#' @param created_at Character vector of date times returned by Twitter.
#' @param tz Time zone.
#' @return Vector of class POSIXct.
#' @noRd
format_date <- function(created_at, tz = "UTC") {
  stopifnot(is.character(created_at))
  as.POSIXct(created_at,
             format = "%a %b %d %T %z %Y",
             tz = tz)
}

format_date2 <- function(x, date = FALSE) {
    o <- tryCatch(as.POSIXct(
        x, format = "%a %b %d %H:%M:%S %z %Y",
        tz = "UTC", origin = "1970-01-01"),
        error = function(e) return(NULL))
    if (any(is.null(o), all(is.na.quiet(o)))) {
  	o <- tryCatch(as.POSIXct(
            x, format = "%a %b %d %H:%M:%S %z %Y"),
            error = function(e) return(NULL))
    }
    if (any(is.null(o), all(is.na.quiet(o)))) {
  	curLocale <- Sys.getlocale("LC_TIME")
  	on.exit(Sys.setlocale("LC_TIME", curLocale),
                add = TRUE)
  	Sys.setlocale("LC_TIME", "C")

  	o <- tryCatch(as.POSIXct(
            x, tz = 'UTC',
            format = "%a, %d %b %Y %H:%M:%S +0000"),
            error = function(e) return(NULL))
    }
    if (any(is.null(o), all(is.na.quiet(o)))) {
  	o <- tryCatch(as.POSIXct(
            x, tz = 'UTC',
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
    if (date) {
        o <- as.Date(o)
    }
    o
}

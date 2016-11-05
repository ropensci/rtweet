#' xiny
#'
#' Returns logical value indicating whether named
#'   object includes var name. Functions do the following:
#'
#' \itemize{
#' 		\item \code{\%xy\%} returns logical for each value of x
#' 		\item \code{\%any\%} returns TRUE if y contains any of x
#' 		\item \code{\%all\%} returns TRUE if y contains all of x
#' }
#' @name xiny
#' @param x Character, name of variable of interest.
#' @param y Named object on which to search for \code{x}.
#'
#' @examples
#' # mpg in mtcars
#' "mpg" %xy% mtcars
#'
#' # year not in mtcars
#' "year" %xy% mtcars
#'
#' # check each name
#' c("mpg", "year") %xy% mtcars
#'
#' # check for any
#' c("mpg", "year") %any% mtcars
#'
#' # check for all
#' c("mpg", "year") %all% mtcars
#'
#' @return Logical vector of length \code{length(x)}.
#' @export
`%xy%` <- function(x, y) {
  if (is.null(names(x))) {
    var <- x
    dat <- y
  } else {
    dat <- x
    var <- y
  }
  var %in% names(dat)
}

#' xanyy
#'
#' Returns logical value indicating whether named
#'   object includes var name. Functions do the following:
#'
#' \itemize{
#' 		\item \code{\%xy\%} returns logical for each value of x
#' 		\item \code{\%any\%} returns TRUE if y contains any of x
#' 		\item \code{\%all\%} returns TRUE if y contains all of x
#' }
#' @name xanyy
#' @param x Character, name of variable of interest.
#' @param y Named object on which to search for \code{x}.
#' @export
`%any%` <- function(x, y) {
  if (is.null(names(x))) {
    var <- x
    dat <- y
  } else {
    dat <- x
    var <- y
  }
  any(var %in% names(dat))
}

#' xally
#'
#' Returns logical value indicating whether named
#'   object includes var name. Functions do the following:
#'
#' \itemize{
#' 		\item \code{\%xy\%} returns logical for each value of x
#' 		\item \code{\%any\%} returns TRUE if y contains any of x
#' 		\item \code{\%all\%} returns TRUE if y contains all of x
#' }
#' @name xally
#' @param x Character, name of variable of interest.
#' @param y Named object on which to search for \code{x}.
#' @export
#' @export
`%all%` <- function(x, y) {
  if (is.null(names(x))) {
    var <- x
    dat <- y
  } else {
    dat <- x
    var <- y
  }
  all(var %in% names(dat))
}

return_last <- function(x, n = 1) {
	x <- rev(x)
  x[seq_len(n)]
}

bply <- function(x, f, ...) {
  do.call("rbind", lapply(x, f, ...))
}

return_n_rows <- function(x, n = NULL) {
	if (is.data.frame(x)) {
    if (is.null(n)) return(x)
    if (nrow(x) > n) {
      x <- x[seq_n_rows(n), ]
    }
    rownames(x) <- NULL
  }
  return(x)
}

seq_n_rows <- function(n) {
  if (is.null(n)) {
    return(TRUE)
  } else {
    seq_len(n)
  }
}

nanull <- function(x) {
  if (is.null(x)) return(NA)
  if (identical(x, "")) return(NA)
  if (length(x) == 0) return(NA)
  x[x == ""] <- NA
  x[is.null(x)] <- NA
  x
}

is_response <- function(x) {
  any(identical(class(x), "response"),
  	all(c("content", "headers") %xy% x))
}
is_json <- function (x) {
  stopifnot(is_response(x))
  grepl("application/json", x$headers[["content-type"]])
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr content
from_js <- function(rsp, check_rate_limit = TRUE) {
  if (!is_json(rsp)) {
    stop("API did not return json", call. = FALSE)
  }
  rsp <- fromJSON(content(rsp, as = "text", encoding = "UTF-8"))
  if (check_rate_limit) {
    if (all(
      identical(names(rsp), "errors"),
      identical(rsp$errors[["message"]], "Rate limit exceeded"))) {
      stop("rate limit exceeded.", call. = FALSE)
    }
  }
  rsp
}


.ids_type <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- .id_type(x)
  if (length(unique(x)) > 1) {
    stop("users must be user_ids OR screen_names, not both.")
  }
  unique(x)
}

.id_type <- function(x) {
	x <- suppressWarnings(is.na(as.numeric(x)))
	if (length(unique(x)) > 1) {
		stop("users must be user_ids OR screen_names, not both.")
	}
	x <- unique(x)
	if (x) {
		return("screen_name")
	} else {
		return("user_id")
	}
}

is.na.quiet <- function(x) {
	suppressWarnings(is.na(x))
}


unlister <- function(x) {
  unlist(x, use.names = FALSE, recursive = TRUE)
}

flatten <- function(x) {
  vapply(x, function(i) paste0(unlister(i), collapse = ","),
    vector("character", 1), USE.NAMES = FALSE)
}

#' format_date
#'
#' @param x Date or datetime vector returned by a Twitter API.
#' @param date Logical, indicating whether to return object of
#'   class date. Defaults to FALSE, thus returning datetime
#'   object (class = POSIXct).
#'
#' @return Date/datetime vector.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- get_timeline("potus", n = 3500)
#' p <- format_date(p$created_at)
#' head(p)
#' }
format_date <- function(x, date = FALSE) {
  o <- tryCatch(as.POSIXct(x,
    format = "%a %b %d %H:%M:%S %z %Y",
    tz = "UTC", origin = "1970-01-01"),
  	error = function(e) return(NULL))
  if (any(is.null(o), all(is.na.quiet(o)))) {
  	o <- tryCatch(as.POSIXct(x,
  		format = "%a %b %d %H:%M:%S %z %Y"),
  		error = function(e) return(NULL))
  }
  if (any(is.null(o), all(is.na.quiet(o)))) {
  	curLocale <- Sys.getlocale("LC_TIME")
  	on.exit(Sys.setlocale("LC_TIME", curLocale), add = TRUE)
  	Sys.setlocale("LC_TIME", "C")

  	o <- tryCatch(as.POSIXct(x, tz = 'UTC',
  		format="%a, %d %b %Y %H:%M:%S +0000"),
  		error = function(e) return(NULL))
  }
  if (any(is.null(o), all(is.na.quiet(o)))) {
  	o <- tryCatch(as.POSIXct(x, tz = 'UTC',
  		format="%a %b %d %H:%M:%S +0000 %Y"),
  		error = function(e) return(NULL))
  }
  if (any(is.null(o), all(is.na.quiet(o)))) {
  	o <- tryCatch(as.POSIXct(x,
  		format = "%a %b %d %H:%M:%S %z %Y"),
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

check_user_id <- function(dat, n = NULL) {

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  user_id <- rep(NA_character_, n)

  if ("user" %xy% dat) {
    user <- dat[["user"]]

    if ("id_str" %xy% user) {
      user_id <- user[["id_str"]]
    }
  }
  user_id
}

check_screen_name <- function(dat, n = NULL) {

	dat <- check_response_obj(dat)

	if (is.null(n)) n <- length(dat[["id_str"]])

	screen_name <- rep(NA_character_, n)

	if ("user" %xy% dat) {
		user <- dat[["user"]]

		if ("screen_name" %xy% user) {
			screen_name <- user[["screen_name"]]
		}
	}
	screen_name
}


return_with_NA <- function(x, n) {
  if (is.character(x)) {
    myNA <- NA_character_
  } else if (is.logical(x)) {
    myNA <- NA
  } else if (is.integer(x)) {
    myNA <- NA_integer_
  } else if (is.numeric(x)) {
    myNA <- NA_real_
  } else {
  	myNA <- NA_character_
  }
  if (any(is.null(x), identical(x, ""), identical(length(x), 0L))) {
    x <- rep(NA, n)
  } else {
  	for (i in seq_along(x)) {
  		if (any(is.null(x[[i]]), identical(x[[i]], ""),
  			identical(length(x), 0L))) {
  			x[[i]] <- NA
  		}
  	}
  }
  x
}


is_empty_list <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.list(x)) {
    return(identical(length(unlist(x, use.names = FALSE)), 0))
  } else if (identical(length(x), 0)) {
    return(TRUE)
  }
  FALSE
}

is_na <- function(x) {
  if (is.list(x)) {
    x <- unlist(lapply(x, function(x)
      any(is.null(x), suppressWarnings(is.na(x)), is_empty_list(x))),
      use.names = FALSE)
  } else {
    x <- is.na(x)
  }
  x
}

filter_na_rows <- function(x) {
  foo <- function(x) all(is_na(x))
  if (is.data.frame(x)) {
    x[!unlist(apply(x, 1, foo), use.names = FALSE), ]
  }

}

is_n <- function(n) {
  if (is.character(n)) {
    n <- suppressWarnings(as.numeric(n))
  }
  if (all(
    length(n) == 1,
    is.numeric(n),
#    identical(n %% 1, 0),
    n > 0)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_url <- function(url) {
  url_names <- c("scheme", "hostname", "port", "path", "query")
  if (all(length(url) > 1, is.list(url),
     url_names %xy% url)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_response_obj <- function(dat) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  if ("statuses" %xy% dat) {
    dat <- dat[["statuses"]]
  }

  if (!"id_str" %xy% dat) {
    if ("id" %xy% dat) {
      dat$id_str <- dat$id
    }
  }
  dat
}

check_user_obj <- function(x) {

  if ("user" %in% names(x)) {
    x <- x[["user"]]
  }
  if (!"id_str" %in% names(x)) {
    if ("id" %in% names(x)) {
      x$id_str <- x$id
    } else {
      stop("object does not contain ID variable.", call. = FALSE)
    }
  }
  x
}
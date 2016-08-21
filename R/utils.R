return_last <- function(x, n = 1) {
  x <- rev(x)
  x[seq_along(n)]
}

#' @importFrom dplyr bind_rows
bply <- function(x, f) {
  x <- bind_rows(lapply(x, f))
  x <- x[!duplicated(x), ]
}

exclude_list_null <- function(x) {
  if (is.list(x)) x <- x[!sapply(x, is.null)]
  x
}

#' @importFrom dplyr data_frame
return_n_rows <- function(x, n = NULL) {
  if (!any(c("tbl_df", "tibble", "tbl") %in% class(x))) {
    x <- data_frame(x)
  }
  x[seq_n_rows(n), ]
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

#' @import httr
#' @importFrom jsonlite fromJSON
from_js <- function(rsp) {
  if (http_type(rsp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  fromJSON(content(rsp, as = "text"))
}


.ids_type <- function(x) {
  if (is.list(x)) x <- unlist(x)
  for (i in seq_along(x)) {
    x[i] <- .id_type(x[i])
  }
  if (length(unique(x)) > 1) {
    stop("users must be user_ids OR screen_names, not both.")
  }
  unique(x)
}


.id_type <- function(x) {
  if (suppressWarnings(is.na(as.numeric(x)))) {
    return("screen_name")
  } else {
    return("user_id")
  }
}


format_date <- function(x, date = TRUE) {
  x <- as.POSIXct(x,
    format = "%a %b %d %H:%M:%S %z %Y",
    tz = Sys.timezone())
  if (date) {
    x <- as.Date(x)
  }
  x
}

check_user_id <- function(dat, n = NULL) {

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  user_id <- rep(NA_character_, n)

  if ("user" %in% names(dat)) {
    user <- dat[["user"]]

    if ("id_str" %in% names(user)) {
      user_id <- user[["id_str"]]
    }
  }

  user_id
}


return_with_NA <- function(x, n) {
  if (is.character(x)) {
    myNA <- NA_character_
  } else if (is.logical(x)) {
    myNA <- NA
  } else if (is.integer(x)) {
    myNA <- NA_integer_
  } else {
    myNA <- NA_character_
  }
  if (any(is.null(x), identical(x, ""))) {
    x <- rep(NA, n)
  }
  for (i in seq_along(x)) {
    if (any(is.null(x[[i]]), identical(x[[i]], ""))) {
      x[[i]] <- NA
    }
  }
  x
}

is_empty_list <- function(x) {
  if (is.list(x)) {
    is.null(unlist(x))
  }
}

is_na <- function(x) {
  if (is.list(x)) {
    x <- unlist(lapply(x, function(x)
      any(is.null(x), suppressWarnings(is.na(x)), is_empty_list(x))),
      recursive = FALSE)
  } else {
    x <- is.na(x)
  }
  x
}

filter_na_rows <- function(x) {
  foo <- function(x) all(is_na(x))
  stopifnot(is.data.frame(x))
  x[!apply(x, 1, foo), ]
}

is_n <- function(n) {
  if (is.character(n)) {
    n <- suppressWarnings(as.numeric(n))
  }
  if (all(
    length(n) == 1,
    is.numeric(n),
    identical(n %% 1, 0),
    n > 0)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_url <- function(url) {
  url_names <- c("scheme", "hostname", "port", "path", "query")
  if (all(length(url) > 1, is.list(url),
     url_names %in% names(url))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_response_obj <- function(dat) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  if ("statuses" %in% names(dat)) {
    dat <- dat[["statuses"]]
  }

  if (!"id_str" %in% names(dat)) {
    if ("id" %in% names(dat)) {
      dat$id_str <- dat$id
    }
  }

  dat
}

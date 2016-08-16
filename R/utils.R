return_last <- function(x, n = 1) {
  x <- rev(x)
  x[seq_along(n)]
}

#' @importFrom dplyr bind_rows
bply <- function(x, f) {
  x <- bind_rows(lapply(x, f))
}

exclude_list_null <- function(x) {
  if (is.list(x)) x <- x[!sapply(x, is.null)]
  x
}

#' @importFrom dplyr tbl_df
return_n_rows <- function(x, n = NULL) {
  stopifnot(is.data.frame(x))
  x <- x[seq_n_rows(n), ]
  if (!any(c("tbl_df", "tibble", "tbl") %in% class(x))) {
    x <- tbl_df(x)
  }
  x
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

#' @keywords internal
#' @import httr
#' @importFrom jsonlite fromJSON
from_js <- function(rsp) {

  if (http_type(rsp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  fromJSON(content(rsp, as = "text"))
}

#' @keywords internal
.ids_type <- function(x) {
  if (is.list(x)) x <- unlist(x)
  for (i in seq_along(x)) {
    x[i] <- .id_type(x[i])
  }
  if (length(unique(x)) > 1) {
    stop("user object must contain user_ids OR only scree_names, but not both.")
  }
  unique(x)
}

#' @keywords internal
.id_type <- function(x) {
  if (suppressWarnings(is.na(as.numeric(x)))) {
    return("screen_name")
  } else {
    return("user_id")
  }
}

#' @keywords internal
format_date <- function(x, date = TRUE) {
  x <- as.POSIXct(x,
    format = "%a %b %d %H:%M:%S %z %Y",
    tz = Sys.timezone())
  if (date) {
    x <- as.Date(x)
  }
  x
}

#' @keywords internal
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

#' @keywords internal
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

is_na <- function(x) {
  if (is.list(x)) {
    sapply(x, is.null)
  } else {
    is.na(x)
  }
}

filter_na_rows <- function(x) {
  foo <- function(x) all(is_na(x))
  stopifnot(is.data.frame(x))
  x[!apply(x, 1, foo), ]
}

#' @keywords internal
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

#' @keywords internal
is_url <- function(url) {
  url_names <- c("scheme", "hostname", "port", "path", "query")
  if (all(length(url) > 1, is.list(url),
     url_names %in% names(url))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

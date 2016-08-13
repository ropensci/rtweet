#' @importFrom httr warn_for_status
scroller <- function(url, n, n.times, ..., catch_error = FALSE) {

  stopifnot(is_n(n), is_url(url))

  if (missing(n.times)) n.times <- 1

  x <- vector("list", n.times)

  for (i in seq_along(x)) {

    r <- tryCatch(
      TWIT(get = TRUE, url, ...),
      error = function(e) NULL)

    if (is.null(r)) break

    if (catch_error) {
      warn_for_status(r)
    }

    x[[i]] <- from_js(r)

    count <- n - unique_id_count(x)

    if (break_check(x[[i]], url, count)) break

    if ("cursor" %in% names(url$query)) {
      url$query$cursor <- get_max_id(x[[i]])
    } else {
      url$query$max_id <- get_max_id(x[[i]])
    }
  }

  x
}


unique_id <- function(x) {
  if ("statuses" %in% tolower(names(x))) {
    x <- x[["statuses"]]
  }
  if ("id_str" %in% names(x)) {
    return(x[["id_str"]])
  }
  if ("ids" %in% names(x)) {
    return(x[["ids"]])
  }
  if (is.null(names(x))) {
    if ("ids" %in% names(x[[1]])) {
      return(x[[1]][["ids"]])
    }
  }
  if ("status_id" %in% names(x)) {
    return(x[["status_id"]])
  }
  if ("user_id" %in% names(x)) {
    return(x[["user_id"]])
  }
}


unique_id_count <- function(x) {
  if (is.data.frame(x)) {
    x <- unique_id(x)
  } else {
    x <- unlist(lapply(x, unique_id))
  }
  length(unique(x))
}



get_max_id <- function(x) {

  if ("statuses" %in% tolower(names(x))) {
    x <- x[["statuses"]]
  }
  if ("id_str" %in% names(x)) {
    x <- x[["id_str"]]
  } else if ("ids" %in% names(x)) {
    return(x[["next_cursor_str"]])
  } else if (is.null(names(x))) {
    if ("ids" %in% names(x[[1]])) {
      return(x[[1]][["next_cursor_str"]])
    }
  } else if ("status_id" %in% names(x)) {
    x <- x[["status_id"]]
  } else if ("user_id" %in% names(x)) {
    x <- x[["user_id"]]
  }
  return_last(x)
}


#' @importFrom dplyr bind_rows
break_check <- function(r, url, count = NULL) {
  if (!is.null(count)) {
    if (count <= 0) return(TRUE)
  }

  if (is.null(r)) return(TRUE)

  x <- get_max_id(r)

  if (is.null(x)) return(TRUE)
  if (any(x == 0, x == "0")) return(TRUE)

  if ("max_id" %in% names(url$query)) {
    if (is.null(url$query$max_id)) return(FALSE)
    if (x == url$query$max_id) return(TRUE)
  }

  FALSE
}

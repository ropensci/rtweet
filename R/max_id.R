get_max_id <- function(x, adj = -1L) {
  if (!is.atomic(x)) {
    if (has_name_(x, "statuses")) {
      x <- x[["statuses"]]
    }
    if (has_name_(x, "id")) {
      x <- x[["id"]]
    } else if (has_name_(x, "ids")) {
      return(x[["next_cursor_str"]])
    } else if (is.null(names(x))) {
      if (has_name_(x, "ids")) {
        return(x[[1]][["next_cursor_str"]])
      }
    } else if (has_name_(x, "status_id")) {
      x <- x[["status_id"]]
    } else if (has_name_(x, "user_id")) {
      x <- x[["user_id"]]
    }
  }
  ##return_last(x) + adj
  bit64::as.integer64(return_last(x)) + adj
}


#' @rdname next_cursor
#' @export
max_id <- function(x) UseMethod("max_id")

#' @export
max_id.default <- function(x) return_last(x)

#' @export
max_id.character <- function(x) {
  x <- sort(bit64::as.integer64(x))
  as.character(x[1])
}

#' @export
max_id.data.frame <- function(x) {
  if (has_name_(attributes(x), "max_id_str")) return(attr(x, "max_id_str"))
  if (has_name_(attributes(x), "max_id")) return(attr(x, "max_id"))
  x <- x[[grep("id$", names(x))[1]]]
  NextMethod()
}

#' @export
max_id.list <- function(x) {
  if (has_name_(x, "max_id_str")) return(x[["max_id_str"]])
  if (has_name_(x, "max_id")) return(x[["max_id"]])
  if (!is.null(names(x))) {
    x <- list(x)
  }
  x <- lapply(x, function(x) x[[grep("id$", names(x))[1]]])
  x <- unlist(lapply(x, max_id))
  return_last(x)
}

#' @export
max_id.response <- function(x) {
  x <- from_js(x)
  NextMethod()
}

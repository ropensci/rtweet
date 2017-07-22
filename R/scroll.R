scroller <- function(url, n, n.times, type = NULL, ...) {
  ## check args
  stopifnot(is_n(n), is_url(url))

  ## if missing set to 1
  if (missing(n.times)) n.times <- 1

  ## initialize vector and counter
  x <- vector("list", n.times)
  counter <- 0

  for (i in seq_along(x)) {
    ## send GET request
    x[[i]] <- httr::GET(url, ...)

    ## if NULL (error) break
    if (is.null(x[[i]])) break

    ## convert from json to R list
    x[[i]] <- from_js(x[[i]])

    ## if length of x or len of statuses == 0, break
    if (any(length(x[[i]]) == 0L,
            all("statuses" %in% names(x[[i]]),
                length(x[[i]][['statuses']]) == 0L))) {
      break
    }
    if (has_name(x[[i]], "errors")) {
      warning(x[[i]]$errors[["message"]], call. = FALSE)
      x[[i]] <- list(data.frame())
      break
    }
    ## if reach counter, break
    counter <- counter +
      as.numeric(unique_id_count(x[[i]], type = type))
    if (counter >= n) break
    ## check other possible fails
    if (break_check(x[[i]], url)) break
    ## if cursor in URL then update otherwise use max id
    if ("cursor" %in% names(url$query)) {
      url$query$cursor <- get_max_id(x[[i]])
    } else {
      url$query$max_id <- get_max_id(x[[i]])
    }
  }
  ## drop NULLs
  if (is.null(names(x))) {
    x <- x[lengths(x) > 0]
  }
  x
}


scroller_ <- function(url, n, n.times, type = NULL, ...) {
  ## check args
  stopifnot(is_n(n), is_url(url))
  ## if missing set to 1
  ##if (missing(n.times)) n.times <- 1
  ## initialize vector and counter
  x <- vector("list", n.times)
  counter <- 0
  x <- httr::GET(url, ...)
  for (i in seq_along(x)) {
    ## send GET request
    x[[i]] <- httr::GET(url, ...)
    url[["max_id"]] <- x[[i]]$search_metadata$max_id_str
  }
  x
}




unique_id_count <- function(x, type = NULL) {
  if (!is.null(type)) {
    if (type == "search") return(100)
    if (type == "timeline") return(200)
    if (type == "followers") return(5000)
  }
  if (isTRUE(length(x) > 1L)) {
    if (!is.null(names(x[[2]]))) {
      x <- unlist(lapply(x, unique_id),
                  use.names = FALSE)
    } else {
      x <- unique_id(x)
    }
  } else {
    x <- unique_id(x)
  }
  if (any(is.null(x), identical(length(x), 0L)))
    return(0)
  length(unique(x))
}



unique_id <- function(x) {
  if ("statuses" %in% tolower(names(x))) {
    x <- x[["statuses"]]
  }
  if ("id_str" %in% names(x)) {
    x[["id_str"]]
  } else if ("ids" %in% names(x)) {
    x[["ids"]]
  } else if ("ids" %in% names(x[[1]])) {
    x[[1]][["ids"]]
  } else if ("status_id" %in% names(x)) {
    x[["status_id"]]
  } else if ("user_id" %in% names(x)) {
    x[["user_id"]]
  }
}


#' get max id
#'
#' Returns max ID for search iteration.
#'
#' @param x Data object returned by Twitter API.
#' @param adj Deired adjustment to the max ID number. Defaults to
#'   one ID older than the previous max.
#' @return Max id string.
#' @importFrom bit64 as.integer64
#' @noRd
get_max_id <- function(x, adj = -1L) {
  if (!is.atomic(x)) {

    if ("statuses" %in% tolower(names(x))) {
      x <- x[["statuses"]]
    }
    if ("id" %in% names(x)) {
      x <- x[["id"]]
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
  }
  ##return_last(x) + adj
  bit64::as.integer64(return_last(x)) + adj
}

last_dig <- function(x, adj = -1L) {
  stopifnot(is.atomic(x), length(x) == 1L)
  n <- nchar(x)
  x2 <- as.integer(substr(x, n, n)) + adj
  paste0(x, x2)
}


break_check <- function(r, url, count = NULL) {
  if (!is.null(count)) {
    if (as.numeric(count) <= 0) return(TRUE)
  }

  if (is.null(r)) return(TRUE)

  x <- get_max_id(r)

  if (is.null(x)) return(TRUE)
  if (any(identical(x, 0), identical(x, "0"))) return(TRUE)

  if ("max_id" %in% names(url$query)) {
    if (is.null(url$query$max_id)) return(FALSE)
    if (identical(as.character(x),
                  as.character(url$query$max_id))) return(TRUE)
  }
  FALSE
}

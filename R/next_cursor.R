#' next_cursor/previous_cursor/max_id
#'
#' Method for returning next value (used to request next page or results)
#' object returned from Twitter APIs.
#'
#' @param x Data object returned by Twitter API.
#'
#' @examples
#' \dontrun{
#'
#' ## Retrieve user ids of accounts following POTUS
#' f1 <- get_followers("potus", n = 75000)
#'
#' ## store next_cursor in page
#' page <- next_cursor(f1)
#'
#' ## max. number of ids returned by one token is 75,000 every 15
#' ##   minutes, so you'll need to wait a bit before collecting the
#' ##   next batch of ids
#' sys.Sleep(15 * 60) ## Suspend execution of R expressions for 15 mins
#'
#' ## Use the page value returned from \code{next_cursor} to continue
#' ##   where you left off.
#' f2 <- get_followers("potus", n = 75000, page = page)
#'
#' ## combine
#' f <- do.call("rbind", list(f1, f2))
#'
#' ## count rows
#' nrow(f)
#'
#' }
#'
#' @return Character string of next cursor value used to retrieved
#'   the next page of results. This should be used to resume data
#'   collection efforts that were interrupted by API rate limits.
#'   Modify previous data request function by entering the returned
#'   value from \code{next_cursor} for the \code{page} argument.
#' @family ids
#' @family extractors
#' @rdname next_cursor
#' @export
next_cursor <- function(x) UseMethod("next_cursor")

#' @export
next_cursor.default <- function(x) return_last(x)

#' @export
next_cursor.numeric <- function(x) {
  sp <- getOption("scipen")
  on.exit(options(sp), add = TRUE)
  options(scipen = 14)
  x <- as.character(x)
  NextMethod()
}

#' @export
next_cursor.character <- function(x) {
  return_last(x)
}

#' @export
next_cursor.data.frame <- function(x) {
  if (has_name_(x, "next_cursor_str")) return(x[["next_cursor_str"]])
  if (has_name_(x, "next_cursor")) return(x[["next_cursor"]])
  if (has_name_(attributes(x), "next_cursor")) return(attr(x, "next_cursor"))
  x <- x[[grep("id$", names(x))[1]]]
  NextMethod()
}

#' @export
next_cursor.list <- function(x) {
  if (has_name_(x, "next_cursor_str")) return(x[["next_cursor_str"]])
  if (has_name_(x, "next_cursor")) return(x[["next_cursor"]])
  if (has_name_(attributes(x), "next_cursor")) return(attr(x, "next_cursor"))
  if (!is.null(names(x))) {
    x <- list(x)
  }
  x <- lapply(x, function(x) x[[grep("id$", names(x))[1]]])
  x <- unlist(lapply(x, next_cursor))
  return_last(x)
}

#' @export
next_cursor.response <- function(x) {
  x <- from_js(x)
  NextMethod()
}






##----------------------------------------------------------------------------##
##                                   MAX_ID                                   ##
##----------------------------------------------------------------------------##



get_max_id <- function(x) {
  if (!is.atomic(x)) {
    if (has_name_(x, "statuses")) {
      x <- x[["statuses"]]
    }
    if (has_name_(x, "next_cursor_str")) {
      return(x[["next_cursor_str"]])
    }
    if (has_name_(x, "id_str")) {
      x <- x[["id_str"]]
    } else if (has_name_(x, "id")) {
      x <- x[["id"]]
    } else if (has_name_(x, "ids")) {
      x <- x[["ids"]]
    } else if (is.null(names(x))) {
      if (has_name_(x[[1]], "next_cursor_str")) {
        return(x[[1]][["next_cursor_str"]])
      }
      if (has_name_(x[[1]], "next_cursor")) {
        return(x[[1]][["next_cursor"]])
      }
      if (has_name_(x[[1]], "id_str")) {
        x <- x[[1]][["id_str"]]
      } else if (has_name_(x[[1]], "id")) {
        x <- x[[1]][["id"]]
      } else if (has_name_(x[[1]], "ids")) {
        x <- x[[1]][["ids"]]
      }
    } else if (has_name_(x, "status_id")) {
      x <- x[["status_id"]]
    } else if (has_name_(x, "user_id")) {
      x <- x[["user_id"]]
    }
  }
  ##return_last(x) + adj
  if (!is.recursive(x)) {
    id_minus_one(return_last(x))
  } else {
    NULL
  }
}


#' @rdname next_cursor
#' @param .x id
#' @export
max_id <- function(.x) UseMethod("max_id")

#' @export
max_id.default <- function(.x) {
  if (inherits(.x, "response")) {
    .x <- from_js(.x)
  } else {
    return(NULL)
  }
  max_id(.x)
}

#' @export
max_id.character <- function(.x) {
  .x[length(.x)]
}

#' @export
max_id.NULL <- function(.x) return(NULL)

#' @export
max_id.numeric <- function(.x) {
  sp <- getOption("scipen")
  on.exit(options(scipen = sp), add = TRUE)
  options(scipen = 15)
  .x <- as.character(.x)
  max_id(.x)
}

#' @export
max_id.integer <- function(.x) {
  sp <- getOption("scipen")
  on.exit(options(scipen = sp), add = TRUE)
  options(scipen = 15)
  .x <- as.character(.x)
  max_id(.x)
}


#' @export
max_id.factor <- function(.x) {
  .x <- as.character(.x)
  max_id(.x)
}


#' @export
max_id.data.frame <- function(.x) {
  if (has_name_(attributes(.x), "max_id_str")) return(attr(.x, "max_id_str"))
  if (has_name_(attributes(.x), "max_id")) return(attr(.x, "max_id"))
  idvar <- c("status_id", "id_str", "id")
  if (nrow(.x) > 0L && any(idvar %in% names(.x))) {
    idvar <- idvar[idvar %in% names(.x)][1]
    .x <- .x[[idvar]]
  } else if (nrow(.x) > 0L && "user_id" %in% names(.x) &&
      any(c("description", "profile_image_url", "friends_count") %in% names(.x))) {
    stop("Failed to find status ID variable. You may have specified users data by mistake.")
  } else {
    .x <- NULL
  }
  max_id(.x)
}

#' @export
max_id.list <- function(.x) {
  if (has_name_(.x, "max_id_str")) return(.x[["max_id_str"]])
  if (has_name_(.x, "max_id")) return(.x[["max_id"]])
  while (is_emptylist(.x)) {
    .x <- .x[[1]]
  }
  if (is.null(names(.x)) &&
        any(vapply(.x, function(x) isTRUE("statuses" %in% names(x)),
                   logical(1)))) {
    .x <- lapply(.x, "[[", "statuses")
  }
  if (is.null(names(.x)) && any(vapply(.x, is.recursive, logical(1)))) {
    .x <- .x[lengths(.x) > 0L & vapply(.x, is.recursive, logical(1))]
    .x <- .x[[length(.x)]]
  }
  if (isTRUE("statuses" %in% names(.x))) {
    .x <- .x[["statuses"]]
  }
  if (is.null(.x) || length(.x) == 0L) return(NULL)
  .x <- tryCatch(
    as.data.frame(.x[!vapply(.x, is.recursive, logical(1))],
                  row.names = NULL, stringsAsFactors = FALSE),
    error = function(e) return(NULL)
  )
  max_id(.x)
}

#' @export
max_id.response <- function(.x) {
  .x <- from_js(.x)
  max_id(.x)
}



id_minus_one <- function(x) {
  if (gregexpr("[0]{1,}$", x)[[1]] != -1) {
    m <- gregexpr("[0]{1,}$", x)
    m <- regmatches(x, m)[[1]]
    nines <- paste(rep("9", nchar(m)), collapse = "")
    x <- gsub("[0]{1,}$", "", x)
    if (nchar(x) == 0) {
      x <- paste0("1", nines)
    } else {
      ln <- substr(x, nchar(x), nchar(x))
      ln <- as.character(as.integer(ln) - 1L)
      x <- gsub("[0-9]{1}$", "", x)
      x <- paste0(x, ln, nines)
    }
    return(x)
  }
  ln <- substr(x, nchar(x), nchar(x))
  ln <- as.character(as.integer(ln) - 1L)
  x <- gsub("[0-9]{1}$", "", x)
  paste0(x, ln)
}




is_emptylist <- function(x) {
  inherits(x, "list") && length(x) == 1L && is.null(names(x))
}




##----------------------------------------------------------------------------##
##                               PREVIOUS CURSOR                              ##
##----------------------------------------------------------------------------##

#' Previous cursor
#'
#' Paginate in reverse (limited integration)
#'
#' @family ids
#' @family extractors
#' @rdname next_cursor
#' @export
#' @export
previous_cursor <- function(x) UseMethod("previous_cursor")

#' @export
previous_cursor.default <- function(x) return_last(x)

#' @export
previous_cursor.numeric <- function(x) {
  sp <- getOption("scipen")
  on.exit(options(scipen = sp), add = TRUE)
  options(scipen = 14)
  x <- as.character(x)
  NextMethod()
}

#' @export
previous_cursor.character <- function(x) {
  return_last(x)
}

#' @export
previous_cursor.data.frame <- function(x) {
  if (has_name_(x, "previous_cursor_str")) return(x[["previous_cursor_str"]])
  if (has_name_(x, "previous_cursor")) return(x[["previous_cursor"]])
  if (has_name_(attributes(x), "previous_cursor")) return(attr(x, "previous_cursor"))
  x <- x[[grep("id$", names(x))[1]]]
  NextMethod()
}

#' @export
previous_cursor.list <- function(x) {
  if (has_name_(x, "previous_cursor_str")) return(x[["previous_cursor_str"]])
  if (has_name_(x, "previous_cursor")) return(x[["previous_cursor"]])
  if (has_name_(attributes(x), "previous_cursor")) return(attr(x, "previous_cursor"))
  if (!is.null(names(x))) {
    x <- list(x)
  }
  x <- lapply(x, function(x) x[[grep("id$", names(x))[1]]])
  x <- unlist(lapply(x, previous_cursor))
  return_last(x)
}

#' @export
previous_cursor.response <- function(x) {
  x <- from_js(x)
  NextMethod()
}





##----------------------------------------------------------------------------##
##                                  SINCE_ID                                  ##
##----------------------------------------------------------------------------##



#' since_id
#'
#' Get the newest ID collected to date.
#'
#' @rdname next_cursor
#' @export
since_id <- function(.x) UseMethod("since_id")

#' @export
since_id.default <- function(.x) {
  if (inherits(.x, "response")) {
    .x <- from_js(.x)
  } else {
    return(NULL)
  }
  since_id(.x)
}

#' @export
since_id.character <- function(.x) {
  .x[1]
}

#' @export
since_id.NULL <- function(.x) return(NULL)

#' @export
since_id.numeric <- function(.x) {
  sp <- getOption("scipen")
  on.exit(options(scipen = sp), add = TRUE)
  options(scipen = 15)
  .x <- as.character(.x)
  since_id(.x)
}

#' @export
since_id.integer <- function(.x) {
  sp <- getOption("scipen")
  on.exit(options(scipen = sp), add = TRUE)
  options(scipen = 15)
  .x <- as.character(.x)
  since_id(.x)
}


#' @export
since_id.factor <- function(.x) {
  .x <- as.character(.x)
  since_id(.x)
}


#' @export
since_id.data.frame <- function(.x) {
  if (has_name_(attributes(.x), "since_id_str")) return(attr(.x, "since_id_str"))
  if (has_name_(attributes(.x), "since_id")) return(attr(.x, "since_id"))
  idvar <- c("status_id", "id_str", "id")
  if (nrow(.x) > 0L && any(idvar %in% names(.x))) {
    idvar <- idvar[idvar %in% names(.x)][1]
    .x <- .x[[idvar]]
  } else if (nrow(.x) > 0L && "user_id" %in% names(.x) &&
      any(c("description", "profile_image_url", "friends_count") %in% names(.x))) {
    stop("Failed to find status ID variable. You may have specified users data by mistake.")
  } else {
    .x <- NULL
  }
  since_id(.x)
}

#' @export
since_id.list <- function(.x) {
  if (has_name_(.x, "since_id_str")) return(.x[["since_id_str"]])
  if (has_name_(.x, "since_id")) return(.x[["since_id"]])
  while (is_emptylist(.x)) {
    .x <- .x[[1]]
  }
  if (is.null(names(.x)) &&
      any(vapply(.x, function(x) isTRUE("statuses" %in% names(x)),
        logical(1)))) {
    .x <- lapply(.x, "[[", "statuses")
  }
  if (is.null(names(.x)) && any(vapply(.x, is.recursive, logical(1)))) {
    .x <- .x[lengths(.x) > 0L & vapply(.x, is.recursive, logical(1))]
    .x <- .x[[length(.x)]]
  }
  if (isTRUE("statuses" %in% names(.x))) {
    .x <- .x[["statuses"]]
  }
  if (is.null(.x) || length(.x) == 0L) return(NULL)
  .x <- tryCatch(
    as.data.frame(.x[!vapply(.x, is.recursive, logical(1))],
      row.names = NULL, stringsAsFactors = FALSE),
    error = function(e) return(NULL)
  )
  since_id(.x)
}

#' @export
since_id.response <- function(.x) {
  .x <- from_js(.x)
  since_id(.x)
}

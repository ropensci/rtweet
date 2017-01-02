scroller <- function(url, n, n.times, type = NULL, ...) {

    stopifnot(is_n(n), is_url(url))
    if (missing(n.times)) n.times <- 1
    x <- vector("list", n.times)
    counter <- 0

    for (i in seq_along(x)) {
  	x[[i]] <- tryCatch(
            TWIT(get = TRUE, url, ...),
            error = function(e) return(NULL))
        if (is.null(x[[i]])) break

        x[[i]] <- from_js(x[[i]])

        if ("statuses" %in% names(x[[i]])) {
            if (identical(length(x[[i]][["statuses"]]), 0L)) break
        }
        counter <- counter +
            as.numeric(unique_id_count(x[[i]], type = type))

        if (counter >= n) break
        if (break_check(x[[i]], url)) break
        if ("cursor" %in% names(url$query)) {
            url$query$cursor <- get_max_id(x[[i]])
        } else {
            url$query$max_id <- get_max_id(x[[i]])
        }
    }
    if (is.null(names(x))) {
        x <- x[vapply(x, length, double(1)) > 0]
    }
    x
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


unique_id_count <- function(x, type = NULL) {
    if (!is.null(type)) {
        if (type == "search") return(100)
        if (type == "timeline") return(200)
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

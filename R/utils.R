
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
  	all(c("content", "headers") %in% names(x)))
}

is_json <- function (x) {
    stopifnot(is_response(x))
    grepl("application/json", x$headers[["content-type"]])
}

#' @importFrom jsonlite fromJSON
from_js <- function(rsp, check_rate_limit = TRUE) {
    if (!is_json(rsp)) {
        stop("API did not return json", call. = FALSE)
    }
    rsp <- rsp[["content"]] %>%
        rawToChar()
    rsp <- fromJSON(rsp)
    if (check_rate_limit) {
        if (all(
            identical(names(rsp), "errors"),
            identical(rsp$errors[["message"]],
                      "Rate limit exceeded"))) {
            warning("rate limit exceeded.", call. = FALSE)
            rsp <- NULL
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

format_date <- function(x, date = FALSE) {
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

check_screen_name <- function(dat, n = NULL) {

    screen_name <- NULL

    dat <- check_response_obj(dat)

    if (is.null(n)) n <- length(dat[["id_str"]])

    screen_name <- rep(NA_character_, n)

    if ("user" %in% names(dat)) {
        user <- dat[["user"]]

        if ("screen_name" %in% names(user)) {
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
    if (any(is.null(x), identical(x, ""),
            identical(length(x), 0L))) {
        x <- rep(NA, n)
    } else {
  	for (i in seq_along(x)) {
            if (any(is.null(x[[i]]),
                    identical(x[[i]], ""),
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
        return(identical(length(unlist(
            x, use.names = FALSE)), 0))
    } else if (identical(length(x), 0)) {
        return(TRUE)
    }
    FALSE
}

is_na <- function(x) {
    if (is.list(x)) {
        x <- unlist(lapply(x, function(x)
            any(is.null(x), suppressWarnings(is.na(x)),
                is_empty_list(x))),
            use.names = FALSE)
    } else {
        x <- is.na(x)
    }
    x
}

filter_na_rows <- function(x) {
    foo <- function(x) all(is_na(x))
    if (is.data.frame(x)) {
        x[!unlist(apply(x, 1, foo),
                  use.names = FALSE), ]
    }

}

is_n <- function(n) {
    if (is.character(n)) {
        n <- suppressWarnings(as.numeric(n))
    }
    if (all(
        length(n) == 1,
        is.numeric(n),
        n > 0)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

is_url <- function(url) {
    url_names <- c("scheme", "hostname",
                   "port", "path", "query")
    if (all(length(url) > 1, is.list(url),
            url_names %in% names(url))) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

check_response_obj <- function(dat) {

    if (missing(dat)) {
        stop("Must specify tweets object, dat.",
             call. = TRUE)
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

check_user_obj <- function(x) {

    if ("user" %in% names(x)) {
        x <- x[["user"]]
    }
    if (!"id_str" %in% names(x)) {
        if ("id" %in% names(x)) {
            x$id_str <- x$id
        } else {
            stop("object does not contain ID variable.",
                 call. = FALSE)
        }
    }
    x
}


#' trim_ts
#'
#' @param x Data from ts_filter.
#' @return Trimmed data frame
#' @export
trim_ts <- function(x) {
    n <- sum(x$filter == x$filter[1])
    nope <- c(seq(1, nrow(x), n), seq(n, nrow(x), n))
    x <- x[-nope, ]
    row.names(x) <- NULL
    x
}
return_last <- function(x, n = 1) {
    x[length(x) - seq_len(n) + 1]
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
    rsp <- rawToChar(rsp[["content"]])
    rsp <- fromJSON(rsp)
    if ("statuses" %in% names(rsp) && "full_text" %in% names(rsp$statuses)) {
        names(rsp[["statuses"]])[names(rsp[["statuses"]]) == "text"] <- "texttrunc"
        names(rsp[["statuses"]])[names(rsp[["statuses"]]) == "full_text"] <- "text"
    } else if ("status" %in% names(rsp) && "full_text" %in% names(rsp$status)) {
        names(rsp[["status"]])[names(rsp[["status"]]) == "text"] <- "texttrunc"
        names(rsp[["status"]])[names(rsp[["status"]]) == "full_text"] <- "text"
    } else if ("full_text" %in% names(rsp)) {
        names(rsp)[names(rsp) == "text"] <- "texttrunc"
        names(rsp)[names(rsp) == "full_text"] <- "text"
    }
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
        return("screen_name")
        ##stop("users must be user_ids OR screen_names, not both.")
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

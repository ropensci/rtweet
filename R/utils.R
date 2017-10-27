
##----------------------------------------------------------------------------##
##                            fetch/return features                           ##
##----------------------------------------------------------------------------##

go_get_var <- function(x, ..., expect_n = NULL) {
  vars <- c(...)
  success <- FALSE
  for (i in vars) {
    if (!is.recursive(x)) break
    if (has_name_(x, i)) {
      x <- x[[i]]
      if (i == vars[length(vars)]) {
        success <- TRUE
      }
    } else if (any_recursive(x) && any(sapply(x, has_name_, i))) {
      kp <- sapply(x, has_name_, i)
      x <- x[kp]
      x <- lapply(x, "[[", i)
      if (i == vars[length(vars)]) {
        success <- TRUE
      }
    }
  }
  if (!success && is.null(expect_n)) {
    return(NULL)
  }
  if (any_recursive(x) && is.null(expect_n)) {
    return(x)
  }
  x <- unlist(x)
  if (!is.null(expect_n) && length(x) < expect_n) {
      x <- c(x, rep(NA, expect_n - length(x)))
  }
  x
}

all_uq_names <- function(x) {
  unique(unlist(lapply(x, names)))
}

return_last <- function(x, n = 1) {
  x[length(x) - seq_len(n) + 1]
}

##----------------------------------------------------------------------------##
##                                 check data                                 ##
##----------------------------------------------------------------------------##

has_name_ <- function(x, ...) {
  vars <- c(...)
  stopifnot(is.character(vars))
  if (!is.recursive(x)) {
    return(FALSE)
  }
  all(vars %in% names(x))
}

any_recursive <- function(x) {
  if (!is.recursive(x)) {
    return(FALSE)
  }
  any(vapply(x, is.recursive, logical(1)))
}

is_response <- function(x) {
  inherits(x, "response")
}

is_json <- function(x) {
  grepl("application/json", x$headers[["content-type"]])
}

is.na.quiet <- function(x) {
  suppressWarnings(is.na(x))
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
  length(n) == 1L && is.numeric(n) && !is.na(n) && n > 0L
}

maybe_n <- function(x) {
  if (is.character(x)) {
    x <- suppressWarnings(as.numeric(x))
  }
  length(x) == 1L && is.numeric(x) && !is.na(x)
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


##----------------------------------------------------------------------------##
##                                  wranglers                                 ##
##----------------------------------------------------------------------------##

nanull <- function(x) {
  if (is.null(x)) return(NA)
  if (identical(x, "")) return(NA)
  if (length(x) == 0) return(NA)
  x[x == ""] <- NA
  x[is.null(x)] <- NA
  x
}

#' @importFrom jsonlite fromJSON
from_js <- function(rsp) {
  stopifnot(is_response(rsp))
  if (!is_json(rsp)) {
    stop("API did not return json", call. = FALSE)
  }
  rsp <- httr::content(rsp, "text", encoding = "UTF-8")
  rsp <- jsonlite::fromJSON(rsp)
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
  rsp
}

na_omit <- function(x) {
  if (is.atomic(x)) {
    x[!is.na(x)]
  } else {
    x[!vapply(x, function(x) isTRUE(is.na(x)), FUN.VALUE = logical(1))]
  }
}


##----------------------------------------------------------------------------##
##                            user type classifers                            ##
##----------------------------------------------------------------------------##

.ids_type <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- .id_type(x)
  if (length(unique(x)) > 1) {
    stop("users must be user_ids OR screen_names, not both.")
  }
  unique(x)
}

.id_type <- function(x) {
  if (is_screen_name(x)) {
    return("screen_name")
  }
  if (is_user_id(x)) {
    return("user_id")
  }
  x <- suppressWarnings(is.na(as.numeric(x)))
  if (length(unique(x)) > 1) {
    return("screen_name")
  }
  x <- unique(x)
  if (x) {
    return("screen_name")
  } else {
    return("user_id")
  }
}


##----------------------------------------------------------------------------##
##                                flatten data                                ##
##----------------------------------------------------------------------------##

flatten_rtweet <- function(x) {
  lst <- vapply(x, is.list, logical(1))
  x[lst] <- lapply(x[lst], vobs2string)
  x
}

vobs2string <- function(x, sep = " ") {
  x[lengths(x) == 0L] <- NA_character_
  x[lengths(x) > 1L] <- vapply(
    x[lengths(x) > 1L],
    obs2string, sep = sep,
    FUN.VALUE = character(1)
  )
  as.character(x)
}

obs2string <- function(x, sep) {
  stopifnot(is.atomic(x))
  if (all(is.na(x))) {
    return(NA_character_)
  }
  x[is.na(x)] <- ""
  paste(x, collapse = sep)
}

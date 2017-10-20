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


#' trim_ts
#'
#' @param x Data from ts_filter.
#' @param group Name of grouping variable. Default (NULL) will use
#'   variable of grouped data frames or look for variables labelled
#'   "group" or "filter". Set this to FALSE to override these defaults.
#' @return Trimmed data frame
#' @noRd
trim_ts <- function(x, group = NULL) {
  if (all(is.null(group), inherits(x, "grouped_df"))) {
    group <- attr(x, "vars")
  } else if (all(is.null(group), "group" %in% names(x))) {
    group <- "group"
  } else if (all(is.null(group), "filter" %in% names(x))) {
    group <- "filter"
  }
  if (any(is.null(group), identical(group, FALSE))) {
    n <- nrow(x)
  } else {
    n <- sum(x[[group]] == x[[group]][1])
  }
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
  inherits(x, "response")
}

is_json <- function(x) {
  grepl("application/json", x$headers[["content-type"]])
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

is.na.quiet <- function(x) {
  suppressWarnings(is.na(x))
}


unlister <- function(x) {
  unlist(x, use.names = FALSE, recursive = TRUE)
}

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


extract_att_data <- function(df) {
  att <- names(attributes(df))
  att <- att[att %in% c("users", "tweets")]
  if (att == "users") {
    users_data(df)
  } else {
    tweets_data(df)
  }
}

do_call_rbind <- function(df) {
  att_df <- lapply(df, extract_att_data)
  att_df <- do.call("rbind", att_df)
  if (all(c("text", "hashtags") %in% names(att_df))) {
    att <- "tweets"
  } else {
    att <- "users"
  }
  df <- do.call("rbind", df)
  attr(df, att) <- att_df
  df
}


rm_fancy_apostrophes <- function(x) gsub(intToUtf8(8217), "'", x)

rm_fancy_spaces <- function(x) gsub(intToUtf8(65039), " ", x)

rm_links <- function(x) {
  x <- gsub("\\s{0,1}http\\S{1,}\\s{0,1}", "", x)
  gsub("\\s{0,1}\\S{1,}\\.com\\b\\s{0,1}", "", x)
}

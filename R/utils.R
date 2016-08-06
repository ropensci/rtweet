#' from_js
#'
#' @description Convert json object to nested list.
#'
#' @param rsp json response object
#'
#' @keywords internal
#' @import httr
#' @importFrom jsonlite fromJSON
#' @export
from_js <- function(rsp) {

  if (http_type(rsp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  fromJSON(content(rsp, as = "text"))
}

#' .id_type
#'
#' @keywords internal
#' @return Character vector of either screen_name or user_id
.id_type <- function(x) {
  if (suppressWarnings(is.na(as.numeric(x)))) {
    return("screen_name")
  } else {
    return("user_id")
  }
}



#' stream_params
#'
#' @keywords internal
#' @description Returns stream param.
#' @param stream character stream query
#' @return param character vector
stream_params <- function(stream) {
  stream <- unlist(trimws(unlist(strsplit(stream, ","))))

  if (!all(suppressWarnings(is.na(as.numeric(stream))))) {
    if (all(is.integer(as.integer(stream)))) {
      params <- list(follow = stream)
    } else {
      params <- list(locations = stream)
    }
  } else {
    params <- list(track = stream)
  }
  params
}



format_date <- function(x, date = TRUE) {
  x <- as.POSIXct(x,
    format = "%a %b %d %H:%M:%S %z %Y",
    tz = Sys.timezone())
  if (date) {
    x <- as.Date(x)
  }
  x
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


## utility functions
wrangle_into_clean_data <- function(x, type = NULL) {
  x <- keep_atomic(x)
  x <- only_keep_str_vars(x, type = type)
  format_created_at(x)
}

keep_atomic <- function(x) {
  x[!vapply(x, is.recursive, logical(1))]
}

only_keep_str_vars <- function(x, type = NULL) {
  strs <- grep("\\_str$", names(x), value = TRUE)
  if (length(strs) > 0L) {
    ## remove _str
    strs_names <- gsub("_str$", "", strs)
    ## drop non_str
    x <- x[!names(x) %in% strs_names]
    ## update names
    if (!is.null(type)) {
      strs_names <- paste0(type, "_", strs_names)
    }
    names(x)[grep("\\_str$", names(x))] <- strs_names
  }
  x
}

format_created_at <- function(x) {
  if (has_name_(x, "created_at")) {
    x$created_at <- format_date(x$created_at)
  }
  x
}

##----------------------------------------------------------------------------##
##                                 lists_users                                ##
##----------------------------------------------------------------------------##

as_lists_users <- function(x) {
  structure(x, class = "lists_users")
}

#' @export
as.data.frame.lists_users <- function(x, row.names, optional, ...) {
  if (!missing(row.names)) {
    warning("`row.names` argument is ignored.")
  }
  if (!missing(optional)) {
    warning("`optional` argument is ignored.")
  }
  if (has_name_(x, "lists")) {
    x <- x[["lists"]]
  }
  out <- tibble::as_tibble(
    wrangle_into_clean_data(x, "list")
  )
  if (has_name_(x, "user")) {
    users <- tibble::as_tibble(
      wrangle_into_clean_data(x$user, "user")
    )
    attr(out, "users") <- users
  }
  if (has_name_(x, "status")) {
    tweets <- tibble::as_tibble(
      wrangle_into_clean_data(x$status, "status")
    )
    attr(out, "tweets") <- tweets
  }
  out
}

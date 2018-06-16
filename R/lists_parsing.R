
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

as.data.frame.lists_users <- function(x) {
  if (has_name_(x, "lists")) {
    x <- x[["lists"]]
  }
  out <- as_tbl(
    wrangle_into_clean_data(x, "list")
  )
  if (has_name_(x, "user")) {
    users <- as_tbl(
      wrangle_into_clean_data(x$user, "user")
    )
    attr(out, "users") <- users
  }
  if (has_name_(x, "status")) {
    tweets <- as_tbl(
      wrangle_into_clean_data(x$status, "status")
    )
    attr(out, "tweets") <- tweets
  }
  out
}


##----------------------------------------------------------------------------##
##                              lists_memberships                             ##
##----------------------------------------------------------------------------##

as_lists_memberships <- function(x) {
  structure(x, class = "lists_memberships")
}

as.data.frame.lists_memberships <- function(x) {
  if (has_name_(x, "lists")) {
    x <- x[["lists"]]
  }
  out <- as_tbl(
    wrangle_into_clean_data(x, "list")
  )
  if (has_name_(x, "user")) {
    users <- as_tbl(
      wrangle_into_clean_data(x$user, "user")
    )
    attr(out, "users") <- users
  }
  if (has_name_(x, "status")) {
    tweets <- as_tbl(
      wrangle_into_clean_data(x$status, "status")
    )
    attr(out, "tweets") <- tweets
  }
  out
}

##----------------------------------------------------------------------------##
##                                lists_members                               ##
##----------------------------------------------------------------------------##
as_lists_members <- function(x) {
  structure(x, class = "lists_members")
}


as.data.frame.lists_members <- function(x) {
  if (has_name_(x, "users")) {
    x <- x[["users"]]
  }
  out <- as_tbl(
    wrangle_into_clean_data(x, "user")
  )
  if (has_name_(x, "status")) {
    tweets <- as_tbl(
      wrangle_into_clean_data(x$status, "status")
    )
    attr(out, "tweets") <- tweets
  }
  out
}


##----------------------------------------------------------------------------##
##                               lists_statuses                               ##
##----------------------------------------------------------------------------##

as_lists_statuses <- function(x) {
  structure(x, class = "lists_statuses")
}

as.data.frame.lists_statuses <- function(x) {
  out <- as_tbl(
    wrangle_into_clean_data(x, "status")
  )
  if (has_name_(x, "user")) {
    users <- as_tbl(
      wrangle_into_clean_data(x$user, "user")
    )
    attr(out, "users") <- users
  }
  out
}




##----------------------------------------------------------------------------##
##                              lists_subscribers                             ##
##----------------------------------------------------------------------------##
as_lists_subscribers <- function(x) {
  structure(x, class = "lists_subscribers")
}


as.data.frame.lists_subscribers <- function(x) {
  if (has_name_(x, "users")) {
    x <- x$users
  }
  out <- as_tbl(
    wrangle_into_clean_data(x, "user")
  )
  if (has_name_(x, "status")) {
    tweets <- as_tbl(
      wrangle_into_clean_data(x$status, "status")
    )
    attr(out, "tweets") <- tweets
  }
  out
}


##----------------------------------------------------------------------------##
##                             lists_subscriptions                            ##
##----------------------------------------------------------------------------##

as_lists_subscriptions <- function(x) {
  structure(x, class = "lists_subscriptions")
}

as.data.frame.lists_subscriptions <- function(x) {
  out <- as_tbl(
    wrangle_into_clean_data(x, "list")
  )
  if (has_name_(x, "user")) {
    users <- as_tbl(
      wrangle_into_clean_data(x$user, "user")
    )
    attr(out, "users") <- users
  }
  out
}

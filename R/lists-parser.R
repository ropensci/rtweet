
## utility function
keep_atomic <- function(x) {
  x[!vapply(x, is.recursive, logical(1))]
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
  out <- tibble::as_tibble(keep_atomic(x))
  if (has_name_(x, "user")) {
    users <- tibble::as_tibble(keep_atomic(x$user))
    attr(out, "users") <- users
  }
  if (has_name_(x, "status")) {
    tweets <- tibble::as_tibble(keep_atomic(x$status))
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
  out <- tibble::as_tibble(keep_atomic(x))
  if (has_name_(x, "user")) {
    users <- tibble::as_tibble(keep_atomic(x$user))
    attr(out, "users") <- users
  }
  if (has_name_(x, "status")) {
    tweets <- tibble::as_tibble(keep_atomic(x$status))
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
  out <- tibble::as_tibble(keep_atomic(x))
  if (has_name_(x, "status")) {
    tweets <- tibble::as_tibble(keep_atomic(x$status))
    attr(out, "tweets") <- tweets
  }
  out
}




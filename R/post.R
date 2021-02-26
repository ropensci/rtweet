check_status_code <- function(x) {
  if (has_name_(x, "status_code") && is.integer(x$status_code)) {
    status <- x$status_code
  } else if (has_name_(x, "status") && is.integer(x$status)) {
    status <- x$status
  } else if (any(grepl("status", names(x)))) {
    int <- sapply(
      x[grep("status", names(x))],
      is.integer
    )
    if (sum(int) > 0L) {
      status <- x[grep("status", names(x))][int][1]
    } else {
      return(FALSE)
    }
  } else if (any(grepl("code", names(x)))) {
    int <- sapply(
      x[grep("code", names(x))],
      is.integer
    )
    if (sum(int) > 0L) {
      status <- x[grep("code", names(x))][int][1]
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
  if (!is.integer(status)) {
    return(FALSE)
  }
  if (status == 200) {
    return(TRUE)
  }
  FALSE
}

parse <- function(x, expansions, fields) {

  if (is.null(fields) && is.null(expansions)) {
    pages <- lapply(x, parse_page, expansions = expansions, fields = fields)
    if (length(pages) == 1) {
      return(pages[[1]])
    }
    return(do.call(rbind, pages))
  }

  abort(c("Not yet implemented!",
          i = "Stay tuned for further updates or use `parse = FALSE`"))
}

enlist <- function(x) {
  # Going through lapply and converting it to a list
  # This works for matrices or data.frames
  if (!is.data.frame(x)) {
    x[lengths(x) > 1] <- lapply(x[lengths(x) > 1], function(x){list(x)})
  }
  x
}

parse_page <- function(page, expansions, fields) {
  if (is.null(fields) && is.null(expansions)) {
    if (is.null(page$data)) {
      dcr <- list2DF(page)
    } else if (!is.null(names(page$data))) {
      dcr <- list2DF(enlist(page$data))
    } else {
      ldf <- lapply(page$data, function(x){list2DF(enlist(x))})
      dcr <- do.call(rbind, ldf)
    }
    # User search do not return meta
    if (!is.null(page$meta)) {
      attr(dcr, "meta") <- page$meta
    }
    class(dcr) <- c("page", class(dcr))
    return(dcr)
  }

  abort("Not yet implemented")
}

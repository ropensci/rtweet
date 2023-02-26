parse <- function(x, expansions, fields) {

  if (is.null(fields) && is.null(expansions)) {
    if (length(x) == 1) {
      return(parse_page(x, expansions, fields))
    }    
    pages <- lapply(x, parse_page, expansions = expansions, fields = fields)
    return(do.call(rbind, pages))
  }

  abort("Not yet implemented")
}

parse_page <- function(page, expansions, fields) {
  if (is.null(fields) && is.null(expansions)) {
    ldf <- lapply(page$data, list2DF)
    dcr <- do.call(rbind, ldf)
    # User search do not return meta
    if (!is.null(page$meta)) {
      attr(dcr, "meta") <- page$meta
    }
    class(dcr) <- c("page", class(dcr))
    return(dcr)
  }

  abort("Not yet implemented")
}

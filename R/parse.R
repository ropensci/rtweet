parse <- function(x, expansions, fields) {

  if (is.null(fields) && is.null(expansions)) {
    pages <- lapply(x, parse_page, fields = fields, expansions = expansions)
    return(do.call(rbind, pages))
  }

  abort("Not yet implemented")
}

parse_page <- function(page, expansions, fields) {
  if (is.null(fields) && is.null(expansions)) {
    ldf <- lapply(page$data, list2DF)
    dcr <- do.call(rbind, ldf)

    attr(dcr, "meta") <- page$meta
    class(dcr) <- c("page", class(dcr))
    return(dcr)
  }

  abort("Not yet implemented")
}

parse <- function(x, fields, expansions) {
  if (is.null(fields) && is.null(expansions)) {
    ldf <- lapply(x, list2DF)
    dcr <- do.call(rbind, ldf)
    return(dcr)
  }
}

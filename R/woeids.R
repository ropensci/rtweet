
find_woeid <- function(x) {
  out <- NA_real_
  if (length(match_woeid(x)) > 0) {
    out <- match_woeid(x)
  } else {
    warning(paste0(
      "unable to find matching location.",
      "Using WOEID for Worldwide trends instead."))
    out <- 1
  }
  if (length(out) > 1) {
    out <- out[1]
  }
  out
}

check_woeid <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (is.na(out)) {
    out <- find_woeid(x)
  }
  if (is_zero(out)) {
    out <- 1
  }
  ##stopifnot(is.numeric(out))
  out
}

is_zero <- function(x) isTRUE(identical(length(x), 0L))

match_woeid <- function(x) {
  if (tolower(x) %in% c("world", "worldwide",
                        "world wide", "all")) {
    return(1)
  } else if (tolower(x) %in% c("us", "u.s.", "u s", "usa", "unitedstates")) {
    return("23424977")
  } else {
    places <- sysdat$woeid[["name"]]
    woeids <- as.character(sysdat$woeid[["woeid"]])
    woeids[tolower(places) == tolower(x)]
  }
}

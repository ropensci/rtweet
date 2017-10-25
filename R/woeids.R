
find_woeid <- function(x) {
  if (length(x) == 0L) {
    warning(paste0(
      "unable to find matching location.",
      "Using WOEID for Worldwide trends instead."))
    x <- "1"
  } else if (length(match_woeid(x)) > 0L) {
    x <- match_woeid(x)
  } else {
    warning(paste0(
      "unable to find matching location.",
      "Using WOEID for Worldwide trends instead."))
    x <- "1"
  }
  if (length(x) == 0L) {
    warning(paste0(
      "unable to find matching location.",
      "Using WOEID for Worldwide trends instead."))
    x <- "1"
  }
  if (length(x) > 1L) {
    x <- x[1L]
  }
  x
}

check_woeid <- function(x) {
  if (is_n(x)) return(as.character(x))
  x <- find_woeid(x)
  as.character(x)
}

is_zero <- function(x) isTRUE(identical(length(x), 0L))

match_woeid <- function(x) {
  if (tolower(x) %in% c("world", "worldwide",
                        "world wide", "all")) {
    return("1")
  } else if (tolower(x) %in% c("us", "u.s.", "u s", "usa", "unitedstates")) {
    return("23424977")
  } else {
    places <- sysdat$woeid[["name"]]
    woeids <- as.character(sysdat$woeid[["woeid"]])
    woeids[tolower(places) == tolower(x)]
  }
}

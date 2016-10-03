#' find_woeid
#'
#' @description Returns WOEID number for desired town or country.
#'
#' @param x Character string, place
#' @keywords internal
#' @details WOEID matching funcitons are offered for convenience.
#'   They are not nearly exhaustive and may sometimes imprecise
#'   (especially when multiple locations are matched to the provided
#'   string). To ensure validity, I recommend looking up the desired
#'   WOEID using one of the many different tools easily found via
#'   Google search.
#' @family trends
#' @export
find_woeid <- function(x) {
  out <- NA_real_
  if (length(match_woeid(x)) > 0) {
  	out <- match_woeid(x)
  } else {
  	warning("unable to find matching location. Using WOEID for
    Worldwide trends instead.")
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
  stopifnot(is.numeric(out))
  out
}

is_zero <- function(x) isTRUE(identical(length(x), 0L))

match_woeid <- function(x) {
	if (tolower(x) %in% c("world", "worldwide", "world wide", "all")) {
		return(1)
	} else if (tolower(x) %in% c("us", "u.s.", "u s")) {
		x <- "united states"
	} else {
		places <- sysdat$woeid[["name"]]
		woeids <- as.numeric(sysdat$woeid[["woeid"]])
		woeids[tolower(places) == tolower(x)]
	}
}

.Renviron <- function() {
  if (file.exists(".Renviron")) {
    ".Renviron"
  } else {
    file.path(home(), ".Renviron")
  }
}

home <- function() {
  if (!identical(Sys.getenv("HOME"), "")) {
    file.path(Sys.getenv("HOME"))
  } else {
    file.path(normalizePath("~"))
  }
}


is_named <- function(x) UseMethod("is_named")
is_named.default <- function(x) !is.null(names(x))

are_named <- function(x) UseMethod("are_named")
are_named.default <- function(x) is_named(x) && !"" %in% names(x)

readlines <- function(x, ...) {
  con <- file(x)
  x <- readLines(con, warn = FALSE, ...)
  close(con)
  x
}

define_args <- function(args, ...) {
  dots <- list(...)
  nms <- names(dots)
  for (i in nms) {
    if (!has_name(args, i)) {
      args[[i]] <- dots[[i]]
    }
  }
  args
}

append_lines <- function(x, ...) {
  args <- define_args(
    c(x, list(...)),
    append = TRUE,
    fill = TRUE
  )
  do.call("cat", args)
}

is_incomplete <- function(x) {
  if (!file.exists(x)) return(FALSE)
  con <- file(x)
  x <- tryCatch(readLines(con), warning = function(w) return(TRUE))
  close(con)
  ifelse(isTRUE(x), TRUE, FALSE)
}


check_renv <- function() {
  if (is_incomplete(.Renviron())) {
    append_lines("", file = .Renviron())
  }
}

set_renv <- function(...) {
  dots <- list(...)
  stopifnot(are_named(dots))
  x <- paste0(names(dots), "=", dots)
  x <- paste(x, collapse = "\n")
  check_renv()
  append_lines(x, file = .Renviron())
  readRenviron(.Renviron())
}

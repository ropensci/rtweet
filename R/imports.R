#' tbl
#'
#' Imports tibble as_data_frame
#'
#' @param \dots Args passed to tibble as_data_frame
#' @return Tibble data frame.
#' @importFrom tibble as_data_frame
#' @export
tbl <- function(...) tibble::as_data_frame(...)

#' Remove tokens
#'
#' If there is a file with saved tokens it will delete it.
#'
#' This functions helps to comply with CRAN policy to remove files created by
#' the package.
#' @param old A logical value if you want to remove old tokens.
#' @param new A logical value if you want to remove new tokens.
#'
#' @return An invisible logical value showing the success of the operation.
#' If no tokens need to be deleted it will return FALSE too.
#' `NULL` if there is nothing to do.
#' @export
#' @examples
#' auth_clean(TRUE, TRUE)
auth_clean <- function(old = TRUE, new = FALSE) {
  stopifnot(is_logical(old))
  stopifnot(is_logical(new))

  if (isFALSE(old) && isFALSE(new)) {
    inform(c("Nothing to do",
             i = "Did you meant to set `old = TRUE`?"))
    return(invisible(NULL))
  }
  old_tokens <- find_old_tokens()
  tools_tokens <- find_tools_tokens()
  all_tokens_files <- c(old_tokens, tools_tokens)
  if (is.null(all_tokens_files)) {
    inform("No tokens were found! Nothing to do.")
    return(invisible(NULL))
  }
  ot <- FALSE
  nt <- FALSE
  if (length(old_tokens) != 0 && old) {
    ot <- unlink(old_tokens)
  }
  if (length(tools_tokens) != 0 && new) {
    nt <- unlink(tools_tokens)
  }
  out <- old && ot || new && nt
  invisible(out)
}

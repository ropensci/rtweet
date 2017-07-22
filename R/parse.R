
parse.piper.fs <- function(f, n = NULL) {
  if (!is.list(f)) {
    f <- list(f)
  }
  if (length(f) == 0L) {
    return(data.frame())
  }
  df <- unlist(lapply(f, "[[[", "ids"), use.names = FALSE)
  if (length(df) == 0L) {
    return(data.frame())
  }
  nextcursor <- unlist(lapply(f, "[[[", "next_cursor_str"), use.names = FALSE)
  nextcursor <- na_omit(nextcursor)
  nextcursor <- nextcursor[length(nextcursor)]
  df <- tibble::as_tibble(list(user_id = df), validate = FALSE)
  attr(df, "next_cursor") <- nextcursor
  if (!is.null(n)) {
    if (n < nrow(df)) {
      df <- df[seq_len(n), ]
    }
  }
  df
}


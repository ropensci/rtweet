# next_cursor generates informative errors

    Code
      next_cursor(letters)
    Error <rlang_error>
      `cursor` must be a string or data frame
    Code
      next_cursor(mtcars)
    Error <rlang_error>
      `cursor` must have a `rtweet_cursor` attribute

# max_id and since_id generate informative erorrs

    Code
      max_id(10)
    Error <rlang_error>
      `max_id` must be a character vector or data frame
    Code
      max_id(mtcars)
    Error <rlang_error>
      `max_id` must contain a `id` column
    Code
      since_id(10)
    Error <rlang_error>
      `since_id` must be a character vector or data frame
    Code
      since_id(mtcars)
    Error <rlang_error>
      `since_id` must contain a `id` column


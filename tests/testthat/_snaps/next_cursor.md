# max_id and since_id generate informative erorrs

    Code
      max_id(10)
    Error <rlang_error>
      `max_id` must be a character vector or data frame
    Code
      max_id(mtcars)
    Error <rlang_error>
      `max_id` must contain a `status_id` column
    Code
      since_id(10)
    Error <rlang_error>
      `since_id` must be a character vector or data frame
    Code
      since_id(mtcars)
    Error <rlang_error>
      `since_id` must contain a `status_id` column


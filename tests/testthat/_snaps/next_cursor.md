# next_cursor generates informative errors

    Code
      next_cursor(letters)
    Condition
      Error in `next_cursor()`:
      ! `cursor` must be a string or data frame
    Code
      next_cursor(mtcars)
    Condition
      Error in `next_cursor()`:
      ! `cursor` must have a `rtweet_cursor` attribute

# max_id and since_id generate informative erorrs

    Code
      max_id(10)
    Condition
      Error in `find_id()`:
      ! `max_id` must be a character vector or data frame
    Code
      max_id(mtcars)
    Condition
      Error in `find_id()`:
      ! `max_id` must contain a `id` column
    Code
      since_id(10)
    Condition
      Error in `find_id()`:
      ! `since_id` must be a character vector or data frame
    Code
      since_id(mtcars)
    Condition
      Error in `find_id()`:
      ! `since_id` must contain a `id` column


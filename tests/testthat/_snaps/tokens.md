# get_token() and get_tokens() are deprecated

    Code
      . <- get_token()
    Condition
      Warning:
      `get_token()` was deprecated in rtweet 1.0.0.
      Please use `auth_get()` instead.
    Code
      . <- get_tokens()
    Condition
      Warning:
      `get_tokens()` was deprecated in rtweet 1.0.0.
      Please use `auth_get()` instead.

# create_token is deprecated

    Code
      token <- suppressMessages(create_token("my-app", "x", "x", "y", "y"))
    Condition
      Warning:
      `create_token()` was deprecated in rtweet 1.0.0.
      See vignette('auth') for details


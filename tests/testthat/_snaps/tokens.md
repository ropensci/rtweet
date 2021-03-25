# get_token() and get_tokens() are deprecated

    Code
      . <- get_token()
    Warning <lifecycle_warning_deprecated>
      `get_token()` was deprecated in rtweet 1.0.0.
      Please use `auth_get()` instead.
    Code
      . <- get_tokens()
    Warning <lifecycle_warning_deprecated>
      `get_tokens()` was deprecated in rtweet 1.0.0.
      Please use `auth_get()` instead.

# create_token is deprecated

    Code
      token <- suppressMessages(create_token("my-app", "x", "x", "y", "y"))
    Warning <lifecycle_warning_deprecated>
      `create_token()` was deprecated in rtweet 1.0.0.
      See vignette('auth') for details


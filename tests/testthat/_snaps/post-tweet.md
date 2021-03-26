# Check geo-related inputs for post_tweet

    Code
      msg <- paste("test geolocated error", Sys.time())
      post_tweet(msg, lat = "x", long = 0)
    Error <simpleError>
      `lat` must be numeric.
    Code
      post_tweet(msg, lat = 0, long = "x")
    Error <simpleError>
      `long` must be numeric.
    Code
      post_tweet(msg, lat = 91, long = 0)
    Error <simpleError>
      `lat` must be between -90 and 90 degrees.
    Code
      post_tweet(msg, lat = 0, long = 181)
    Error <simpleError>
      `long` must be between -180 and 180 degrees.
    Code
      post_tweet(msg, lat = 0, long = 0, display_coordinates = "error")
    Error <simpleError>
      `display_coordinates` must be TRUE/FALSE.


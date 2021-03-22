# post_tweet geolocated validity checks

    Code
      post_tweet(msg, lat = "x", long = 0)
    Error <simpleError>
      lat should be numeric

---

    Code
      post_tweet(msg, lat = 0, long = "x")
    Error <simpleError>
      long should be numeric

---

    Code
      post_tweet(msg, lat = 91, long = 0)
    Error <simpleError>
      lat should be between -90 and 90 degrees

---

    Code
      post_tweet(msg, lat = 0, long = 181)
    Error <simpleError>
      long should be between -180 and 180  degrees

---

    Code
      post_tweet(msg, lat = 0, long = 0, display_coordinates = "error")
    Error <simpleError>
      display_coordinates should be TRUE/FALSE


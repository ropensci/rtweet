# gives useful errors

    Code
      search_tweets(c(1:10), verbose = FALSE)
    Condition
      Error in `search_params()`:
      ! length(q) == 1L is not TRUE

---

    Code
      search_tweets("stats", type = "all")
    Condition
      Error in `search_params()`:
      ! `type` must be one of "mixed", "recent", or "popular", not "all".


# gives useful errors

    Code
      search_tweets(c(1:10), verbose = FALSE)
    Error <simpleError>
      length(q) == 1L is not TRUE

---

    Code
      search_tweets("stats", type = "all")
    Error <rlang_error>
      `type` must be one of "mixed", "recent", or "popular".


# search_fullarchive works

    Code
      df <- search_fullarchive(q = "#covid place:UK OR place:GB OR place:\"United Kindom\"",
        n = 20, env_name = "fullArchive", fromDate = "201810010000")

# search_fullarchive queries bigger than page size work

    Code
      df <- search_fullarchive(q = "#covid place:UK OR place:GB OR place:\"United Kindom\"",
        n = 20, env_name = "fullArchive", fromDate = "201810010000")

# search_fullarchive does not return duplicate tweets

    Code
      df <- search_fullarchive(q = "#halalan22", n = 450, env_name = "fullArchive",
        fromDate = "202202080000", toDate = "202205100000", parse = TRUE)


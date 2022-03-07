library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  filter_request_headers = list(Authorization = "My oauth token is safe",
                                GOOGLE_KEY = "My google key is safe.",
                                GOOGLE_MAPS_KEY = "Your google key is safe.")
))
vcr::check_cassette_names()

if (requireNamespace("vcr", quietly = TRUE)) {
  library("vcr") # *Required* as vcr is set up on loading
  invisible(vcr::vcr_configure(
    dir = vcr::vcr_test_path("fixtures"),
    filter_request_headers = list(Authorization = "My oauth token is safe",
                                  GOOGLE_KEY = "My google key is safe.",
                                  GOOGLE_MAPS_KEY = "My google key is safe."),
    filter_query_parameters = c(status = "status message"),
    serialize_with = "json",
    warn_on_empty_cassette = TRUE

  ))
  vcr::check_cassette_names(allowed_duplicates = "lookup_users1")
}

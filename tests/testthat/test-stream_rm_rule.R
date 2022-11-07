test_that("stream_rm_rule removes multiple rules", {
  auth_as("RTAA")
  b <- c("1564729327710879748", "1564729327710879749", "1564729327710879750")
  vcr::use_cassette("stream_rm_rule", {
    out <- stream_rm_rule(b)
  })
  out$meta$summary$deleted == length(b)
})

# Use conditionally testthat
if (requireNamespace("rtweet", quietly = TRUE)) {
  library(testthat)
  library(rtweet)
  
  test_check("rtweet")
  
}

# Use conditionally testthat
tests_are_doable <- requireNamespace("rtweet", quietly = TRUE) && 
  requireNamespace("testthat", quietly = TRUE) && 
  requireNamespace("vcr", quietly = TRUE)
if (tests_are_doable) {
  library(testthat)
  library(rtweet)
  
  test_check("rtweet")
  
}

# Helper function for testing

testing_with_authentication <- function(auth = "RTAA") {
  skip_if_offline()
  if (!auth %in% auth_list()) {
    skip("Requires a different authentication")
  }
  suppressMessages(auth_as(auth))
}


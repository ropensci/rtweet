# Helper function for testing

testing_with_authentication <- function(auth) {
  skip_if_offline()
  if (!auth %in% auth_list()) {
    skip("Requires a different authentication")
  }
  withr::defer_parent(auth_as(auth_get()))
  auth_as(auth)
}


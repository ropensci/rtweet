test_that("twit_params abstracts over user type", {
  expect_equal(
    twit_params(user = "hadleywickham"), 
    list(screen_name = "hadleywickham")
  )
  
  expect_equal(
    twit_params(user = 2973406683), 
    list(user_id = "2973406683")
  )
})

test_that("twit_params collapses multiple users", {
  expect_equal(
    twit_params(user = c("hadleywickham", "jennybryan")), 
    list(screen_name = "hadleywickham,jennybryan")
  )
})

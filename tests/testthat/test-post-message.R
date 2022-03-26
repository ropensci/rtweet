test_that("post_message works", {
  skip("requires manual testing")
  vcr::use_cassette("post_message", {
    rt <- expect_message(
      post_message(paste("Testing", Sys.time()), user = "Lluis_Revilla"),
      "Your DM has been posted!"
    )
  })
  expect_equal(httr::status_code(rt), 200L)
  
})

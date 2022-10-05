test_that("clean_tweets works", {

  vcr::use_cassette("clean_tweets", {
    tweet1 <- lookup_tweets("1576990222075985920")
  })

  out1 <- clean_tweets(tweet1)
  expect_equal(out1,
               "Código para baixar os dados da votação presidencial, por municípios, em R ()\n\n(Produzido junto com o )\n\n \n\n")
  expect_true(is.character(out1))
})

context("get_friends")

test_that("get_friends returns data frame with ids", {
    skip_on_cran()

    token <- readRDS("twitter_tokens")
    rl <- rate_limit(token, "get_followers")
    if (rl$remaining > 1) {
        f <- get_friends("kearneymw", token = token)
    
        expect_true(is.data.frame(f))
        ##expect_true(is.character(f[["ids"]]))
        expect_gt(nrow(f), 200)
    } else {
        expect_true(rl$limit == 15)
        expect_true(rl$remaining == 0)
        expect_true(rl$limit == 15)
        expect_true(rl$limit == 15)
    }
})

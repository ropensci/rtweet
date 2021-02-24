context("get_friends")

test_that("get_friends returns data frame with ids", {
    skip_on_cran()
    skip_if_offline()

    rl <- rate_limit(NULL, "get_followers")
    if (rl$remaining > 1) {
        f <- get_friends("kearneymw")
    
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

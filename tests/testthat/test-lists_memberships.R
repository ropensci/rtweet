test_that("lists_memberships returns data frame with nrow > 1", {
    skip_on_cran()
    skip_if_offline()

    sns <- "kearneymw"
    x <- lists_memberships(sns)

    expect_true(is.data.frame(x))
    expect_gt(nrow(x), 0)
    expect_true(is.character(previous_cursor(x)))
    expect_true(is.character(next_cursor(x)))

    x <- lists_memberships(sns, parse = FALSE)
    expect_true(is.data.frame(as.data.frame(x)))
    expect_true(is.character(previous_cursor(x)))
    expect_true(is.character(next_cursor(x)))
})

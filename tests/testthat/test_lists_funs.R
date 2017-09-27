## TEST LISTS FUNCTIONS

context("lists")

test_that("lists_users returns data frame with nrow > 1", {
    skip_on_cran()

    sns <- "kearneymw"
    token <- readRDS("twitter_tokens")
    x <- lists_users(sns, token = token)

    expect_true(is.data.frame(x))
    ##expect_true(is.character(f[["ids"]]))
    expect_gt(nrow(x), 0)
})

test_that("lists_memberships returns data frame with nrow > 1", {
    skip_on_cran()

    sns <- "kearneymw"
    token <- readRDS("twitter_tokens")
    x <- lists_memberships(sns, token = token)

    expect_true(is.data.frame(x))
    ##expect_true(is.character(f[["ids"]]))
    expect_gt(nrow(x), 0)
})


test_that("lists_members returns data frame with nrow > 1", {
    skip_on_cran()

    lst_id <- "849721680402333696"
    token <- readRDS("twitter_tokens")
    x <- lists_members(lst_id, token = token)

    expect_true(is.data.frame(x))
    ##expect_true(is.character(f[["ids"]]))
    expect_gt(nrow(x), 0)
})

test_that("lists_statuses returns data frame with nrow > 1", {
    skip_on_cran()

    lst_id <- "849721680402333696"
    token <- readRDS("twitter_tokens")
    x <- lists_statuses(lst_id, token = token)

    expect_true(is.data.frame(x))
    ##expect_true(is.character(f[["ids"]]))
    expect_gt(nrow(x), 0)
})

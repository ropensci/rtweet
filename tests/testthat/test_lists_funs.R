## TEST LISTS FUNCTIONS

context("lists")

test_that("lists_users returns data frame with nrow > 1", {
    skip_on_cran()

    sns <- "kearneymw"
    token <- readRDS("twitter_tokens")
    x <- lists_users(sns, token = token)
    expect_true(is.data.frame(x))
    expect_gt(nrow(x), 0)
    x <- lists_members(slug = "senators", owner_user = "cspan", token = token)
    expect_true(is.data.frame(x))
    expect_gt(nrow(x), 0)

    x <- lists_members(slug = "rstats", owner_user = "scultrera")
    expect_true(is.data.frame(x))
    expect_gt(nrow(x), 0)

    ## get up to 200 list memberships of Nate Silver
    x <- lists_memberships("NateSilver538", n = 200)
    expect_true(is.data.frame(x))
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

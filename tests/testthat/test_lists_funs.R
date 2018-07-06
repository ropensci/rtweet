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
    ## get up to 200 list memberships of Nate Silver
    x <- lists_memberships("NateSilver538", n = 200, token = token)
    expect_true(is.data.frame(x))
    expect_gt(nrow(x), 0)
})

test_that("lists_memberships returns data frame with nrow > 1", {
    skip_on_cran()

    sns <- "kearneymw"
    token <- readRDS("twitter_tokens")
    x <- lists_memberships(sns, token = token)

    expect_true(is.data.frame(x))
    expect_gt(nrow(x), 0)
    expect_true(is.character(previous_cursor(x)))
    expect_true(is.character(next_cursor(x)))
    expect_true(is.null(max_id(x)))
    expect_true(is.null(since_id(x)))

    x <- lists_memberships(sns, token = token, parse = FALSE)
    expect_true(is.data.frame(as.data.frame(x)))
    expect_true(is.character(previous_cursor(x)))
    expect_true(is.character(next_cursor(x)))
    expect_true(is.null(max_id(x)))
    expect_true(is.null(since_id(x)))
})


test_that("lists_members returns data frame with nrow > 1", {
    skip_on_cran()

    lst_id <- "849721680402333696"
    token <- readRDS("twitter_tokens")
    x <- lists_members(lst_id, token = token)

    expect_true(is.data.frame(x))
    expect_gt(nrow(x), 0)
})

test_that("lists_statuses returns data frame with nrow > 1", {
    skip_on_cran()

    lst_id <- "849721680402333696"
    token <- readRDS("twitter_tokens")
    x <- lists_statuses(lst_id, token = token)

    expect_true(is.data.frame(x))
    expect_gt(nrow(x), 0)
})


test_that("lists_subscribers returns users data frame", {
  skip_on_cran()
  token <- readRDS("twitter_tokens")

  x <- lists_subscribers(
    slug = "new-york-times-politics",
    owner_user = "nytpolitics",
    n = 200,
    token = token
  )

  expect_true(is.data.frame(x))
  expect_true("description" %in% names(x))
  expect_gt(nrow(x), 50)
})


test_that("lists_subscriptions returns lists data frame", {
  skip_on_cran()
  token <- readRDS("twitter_tokens")

  x <- lists_subscriptions(
    user = "kearneymw",
    token = token
  )

  expect_true(is.data.frame(x))
  expect_true("list_id" %in% names(x))
  expect_gt(nrow(x), 1)
})




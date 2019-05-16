testthat::context("yml")

this <-
  "palette:\n- g: 175.0\n- a: 0.7\ntemplate: panda-syntax\nfont_family: Fira Cod\npadding_vertical: 11\npadding_horizontal: 14"

this_noname <-
  "palette:\n- 175.0\n- 0.7\ntemplate: panda-syntax\nfont_family: Fira Cod\npadding_vertical: 11\npadding_horizontal: 14"

reset <- function(this) {
  tf <- tempfile(fileext = ".yml")
  cat(this, file = tf, sep = "\n")
  carbonate::carbon$new(code = "abc", yml = tf, silent_yml = TRUE)
}

reset_verbose <- function(this) {
  tf <- tempfile(fileext = ".yml")
  cat(this, file = tf, sep = "\n")
  carbonate::carbon$new(code = "abc", yml = tf, silent_yml = FALSE)
}

testthat::describe("yaml fields", {
  it("rgba", {
    x <- reset(this)
    expect_equal(c(r = 171, g = 175, b = 195, a = 0.7), x$palette)
  })

  it("template", {
    x <- reset(this)
    expect_equal("panda-syntax", x$template)
  })

  it("bad font family", {
    x <- reset(this)
    expect_equal("Hack", x$font_family)
  })

  it("pv", {
    x <- reset(this)
    expect_equal(11, x$padding_vertical)
  })

  it("ph", {
    x <- reset(this)
    expect_equal(14, x$padding_horizontal)
  })
})

testthat::describe("yaml verbose", {
  out <- "palette:\n- 171.0\n- 175.0\n- 195.0\n- 0.7\ntemplate: panda-syntax\npadding_vertical: 11\npadding_horizontal: 14"

  x <- testthat::expect_message(object = reset_verbose(this), regexp = "invalid value")
  x <- testthat::expect_output(object = reset_verbose(this), regexp = out)
})

testthat::describe("namesless palette", {
  x <- reset(this_noname)

  it("fill in palette", {
    testthat::expect_equal(x$palette, c(r = 175, g = 0.7, b = 195, a = 1))
  })
})

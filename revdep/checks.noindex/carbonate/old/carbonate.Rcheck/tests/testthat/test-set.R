testthat::context("set_")

x <- carbonate::carbon$new(code = "abc", yml = NULL)

testthat::describe("set functions", {
  it("set_template", {
    x$set_template(x$get_templates()[1])
    testthat::expect_equal(x$template, x$get_templates()[1])
  })

  it("set_font_family", {
    x$set_font_family(x$get_font_families()[1])
    testthat::expect_equal(x$font_family, x$get_font_families()[1])
  })

  it("set_windows_control_theme", {
    x$set_window_control_theme(x$get_windows_control_themes()[1])
    testthat::expect_equal(x$window_control_theme, x$get_windows_control_themes()[1])
  })
})

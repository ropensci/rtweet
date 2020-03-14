testthat::context("uri")

x <- carbonate::carbon$new(code = "abc", yml = NULL)

testthat::describe("options", {
  bench <- "bg=rgba(171%2C184%2C195%2C1)&t=seti&wt=none&l=r&ds=true&dsyoff=20px&dsblur=68px&wc=true&wa=true&pv=48px&ph=32px&ln=false&fm=Hack&fs=14px&lh=133%25&si=false&es=1x&wm=false&ts=false&code=abc"

  it("benchmark", {
    testthat::expect_equal(bench, x$options())
  })
})

testthat::describe("uri", {
  bench <- "https://carbon.now.sh/?bg=rgba(171%2C184%2C195%2C1)&t=seti&wt=none&l=r&ds=true&dsyoff=20px&dsblur=68px&wc=true&wa=true&pv=48px&ph=32px&ln=false&fm=Hack&fs=14px&lh=133%25&si=false&es=1x&wm=false&ts=false&code=abc"

  it("benchmark", {
    testthat::expect_equal(bench, x$uri())
  })

  it("200", {
    testthat::expect_false(httr::http_error(x$uri()))
  })
})

testthat::describe("encode", {
  it("encode character", {
    testthat::expect_equal("%2523%2540%2524%2525%27", x$encode(URL = "#@$%'"))
  })

  it("no encode character", {
    testthat::expect_equal("!();?._~-", x$encode(URL = "!();?._~-"))
  })
})

testthat::describe("tiny", {
  it("valid tiny", {
    testthat::skip_on_cran()
    testthat::expect_false(httr::http_error(x$tiny()))
  })

  it("clipboard", {
    testthat::skip_on_travis()
    skip_if_no_clipboard()
    
    cl <- x$tiny(clip = TRUE)
    testthat::expect_equal(cl, clipr::read_clip())
  })
})

testthat::describe("bad template", {
  x$template <- "xxx"
  it("error uri", testthat::expect_error(x$uri(), "template not valid: xxx"))
})

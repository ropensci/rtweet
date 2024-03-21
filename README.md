
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtweet <img src="man/figures/logo.png" width="160px" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci/rtweet/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/rtweet/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/rtweet)](https://cran.r-project.org/package=rtweet)
[![Coverage
Status](https://codecov.io/gh/ropensci/rtweet/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ropensci/rtweet?branch=master)
![Downloads](https://cranlogs.r-pkg.org/badges/rtweet)
[![ZENODO](https://zenodo.org/badge/64161359.svg)](https://zenodo.org/badge/latestdoi/64161359)
[![rOpenSci](https://badges.ropensci.org/302_status.svg)](https://github.com/ropensci/software-review/issues/302)
[![JOSS](https://joss.theoj.org/papers/10.21105/joss.01829/status.svg)](https://doi.org/10.21105/joss.01829)
[![“Buy Me A
Coffee”](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/llrs)

<!-- badges: end -->

Use twitter from R.

**This package is no longer updated and no fixes can be expected.**  
A request to Orphan the package on CRAN was sent!

The maintainer cannot longer check most of the output of the
functionality provided. If you want to volunteer reach out to the
maintainer.

## Installation

To get the current released version from CRAN:

``` r
install.packages("rtweet")
```

You can install the development version of rtweet from GitHub with:

``` r
install.packages("rtweet", repos = 'https://ropensci.r-universe.dev')
```

## Usage

All users must be authenticated to interact with Twitter’s APIs. See
`vignette("auth", package = "rtweet")` for details.

``` r
library(rtweet)
```

rtweet should be used in strict accordance with Twitter’s [developer
terms](https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases).

## Usage

Depending on if you are a paid user or not you can do more or less
things.

# Free

You can post (`tweet_post()`) and retrieve information about yourself
(`user_self()`).

# Paid

You can do all the other things: search tweets
(`tweet_search_recent()`), retrieve your own bookmarks
(`user_bookmarks()`), check who follows who, (`user_following()`, or
`user_followers()`), ….

## Contact

Communicating with Twitter’s APIs relies on an internet connection,
which can sometimes be inconsistent.

If you have questions, or needs an example or want to share a [use
case](https://ropensci.org/usecases/), you can post them on [rOpenSci’s
discuss](https://discuss.ropensci.org/). Were you can [browse uses of
rtweet](https://discuss.ropensci.org/tags/c/usecases/10/rtweet) too.

With that said, if you encounter an obvious bug for which there is not
already an active [issue](https://github.com/ropensci/rtweet/issues),
please [create a new
issue](https://github.com/ropensci/rtweet/issues/new) with all code used
(preferably a reproducible example) on Github.

# Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

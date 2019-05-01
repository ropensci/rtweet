#' Capture an image of a tweet/thread
#'
#' Provide a status id or a full Twitter link to a tweet and this function
#' will capture an image of the tweet --- or tweet + thread (if there are
#' Twitter-linked replies) --- from the mobile version of said tweet/thread.
#'
#' For this to work, you will need to ensure the packages in `Suggests:` are
#' installed as they will be loaded upon the first invocation of this function.
#'
#' Use the `zoom` factor to get more pixels which may improve the text rendering
#' of the tweet/thread.
#'
#' @md
#' @param statusid_or_url a valid Twitter status id (e.g. "`947082036019388416`") or
#'     a valid Twitter status URL (e.g. "`https://twitter.com/jhollist/status/947082036019388416`").
#' @param zoom a positive number >= 1. See the help for `[webshot::webshot()]` for more information.
#' @param scale auto-scale the image back to 1:1? Default it `TRUE`, which means `magick`
#'     will be used to return a "normal" sized tweet. Set it to `FALSE` to perform your
#'     own image manipulation.
#' @return `magick` object
#' @export
#' @examples \dontrun{
#' tweet_shot("947082036019388416")
#' tweet_shot("https://twitter.com/jhollist/status/947082036019388416")
#' }
tweet_shot <- function(statusid_or_url, zoom = 3, scale = TRUE) {
  ## check for required packages
  try_require("magick", "tweet_shot")
  try_require("webshot", "tweet_shot")

  statusid_or_url <- statusid_or_url[1]
  zoom <- zoom[1]
  scale <- scale[1]

  if (zoom <= 1) {
    stop("zoom must be a positive number, >= 1", call. = FALSE)
  }
  if (!is.logical(scale)) {
    stop("scale must be TRUE/FALSE", call. = FALSE)
  }

  x <- statusid_or_url

  ## first test if we have a Twitter URL
  is_url <- grepl("^http[s]://", x)

  if (is_url) {

    ## shld have "twitter" in it
    is_twitter <- grepl("twitter", x)
    if (!is_twitter) {
      stop("statusid_or_url must be a valid Twitter status id or URL",
        call. = FALSE)
    }

    ## shld also have "status" in it
    is_status <- grepl("status", x)
    if (!is_status) {
      stop("statusid_or_url must be a valid Twitter status id or URL",
        call. = FALSE)
    }

    ## if it's not a mobile status, make it one
    already_mobile <- grepl("://mobile\\.", x)
    if (!already_mobile) {
      x <- sub("://twi", "://mobile.twi", x)
    }

  } else {

    ## let's see if it's a status id
    x <- rtweet::lookup_tweets(x)
    if (!(nrow(x) > 0)) {
      stop("Twitter status not found", call. = FALSE)
    }

    ## make a mobile URL
    x <- sprintf("https://mobile.twitter.com/%s/status/%s",
      x$screen_name, x$status_id)

  }

  ## keep the filesystem clean
  tf <- tempfile(fileext = ".png")
  ## it'll clean up for us
  on.exit(unlink(tf), add = TRUE)

  ## capture the tweet
  webshot::webshot(url = x, file = tf, zoom = zoom)

  ## read the image in
  img <- magick::image_read(tf)
  ## remove the extraneous border
  img <- magick::image_trim(img)

  ## scale if we want to
  if ((zoom > 1) && (scale)) {
    img <- magick::image_scale(img, as_percent(1/zoom))
  }

  ## return img
  img
}

as_percent <- function(x) {
  paste0(round(x * 100), "%")
}


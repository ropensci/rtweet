#' Posts status update to user's Twitter account
#'
#' @inheritParams lookup_users
#' @param status Character, tweet status. Must be 280 characters or less.
#' @param media Length 1 character vector with a file path to video media **OR**
#'     up-to length 4 character vector with file paths to static images to be included in tweet.
#'     **The caller is responsible for managing this.**
#' @param in_reply_to_status_id Status ID of tweet to which you'd like to reply.
#'   Note: in line with the Twitter API, this parameter is ignored unless the
#'   author of the tweet this parameter references is mentioned within the
#'   status text.
#' @param destroy_id To delete a status, supply the single status ID here. If a
#'   character string is supplied, overriding the default (NULL), then a destroy
#'   request is made (and the status text and media attachments) are irrelevant.
#' @param retweet_id To retweet a status, supply the single status ID here. If a
#'   character string is supplied, overriding the default (NULL), then a retweet
#'   request is made (and the status text and media attachments) are irrelevant.
#' @param auto_populate_reply_metadata If set to TRUE and used with
#'   in_reply_to_status_id, leading @mentions will be looked up from the
#'   original Tweet, and added to the new Tweet from there. Defaults to FALSE.
#' @param media_alt_text attach additional [alt text](https://en.wikipedia.org/wiki/Alt_attribute)
#'        metadata to the `media` you are uploading. Should be same length as
#'        `media` (i.e. as many alt text entries as there are `media` entries). See
#'        [the official API documentation](https://developer.twitter.com/en/docs/twitter-api/v1/media/upload-media/api-reference/post-media-metadata-create)
#'        for more information.
#' @param lat A numeric value representing the latitude of the location the
#'   tweet refers to. Range should be between -90 and 90 (north). Note that you
#'   should enable the "Precise location" option in your account via *Settings
#'   and privacy > Privacy and Safety > Location*. See
#'   [the official Help Center section](https://help.twitter.com/en/safety-and-security/twitter-location-services-for-mobile).
#' @param long A numeric value representing the longitude of the location the
#'   tweet refers to. Range should be between -180 and 180 (west). See
#'   `lat` parameter.
#' @param display_coordinates Put a pin on the exact coordinates a tweet has
#'   been sent from. Value should be TRUE or FALSE. This parameter would apply
#'   only if you have provided a valid `lat/long` pair of valid values.
#' @examples
#' if (auth_has_default()) {
#' ## generate data to make/save plot (as a .png file)
#' x <- rnorm(300)
#' y <- x + rnorm(300, 0, .75)
#' col <- c(rep("#002244aa", 50), rep("#440000aa", 50))
#' bg <- c(rep("#6699ffaa", 50), rep("#dd6666aa", 50))
#'
#' ## create temporary file name
#' tmp <- tempfile(fileext = ".png")
#'
#' ## save as png
#' png(tmp, 6, 6, "in", res = 127.5)
#' par(tcl = -.15, family = "Inconsolata",
#'     font.main = 2, bty = "n", xaxt = "l", yaxt = "l",
#'     bg = "#f0f0f0", mar = c(3, 3, 2, 1.5))
#' plot(x, y, xlab = NULL, ylab = NULL, pch = 21, cex = 1,
#'      bg = bg, col = col,
#'      main = "This image was uploaded by rtweet")
#' grid(8, lwd = .15, lty = 2, col = "#00000088")
#' dev.off()
#'
#' ## post tweet with media attachment
#' post_tweet("a tweet with media attachment", media = tmp,
#'            media_alt_text = "Random  points example of rtweet::post_tweet.
#'            rtweet requires alt text with all media")
#'
#' # example of replying within a thread
#' ## first post
#' pt <- post_tweet(status="first in a thread")
#'
#' reply_id <- ids(pt)
#'
#' ## post reply
#' post_tweet("second in the thread",
#'   in_reply_to_status_id = reply_id)
#' }
#' @family post
#' @aliases post_status
#' @export
#' @references
#' Tweet: <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/post-statuses-update>
#' Retweet: <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/post-statuses-retweet-id>
#' Media: <https://developer.twitter.com/en/docs/twitter-api/v1/media/upload-media/api-reference/post-media-metadata-create>
#' Alt-text: <https://developer.twitter.com/en/docs/twitter-api/v1/media/upload-media/api-reference/post-media-metadata-create>
post_tweet <- function(status = "my first rtweet #rstats",
                       media = NULL,
                       token = NULL,
                       in_reply_to_status_id = NULL,
                       destroy_id = NULL,
                       retweet_id = NULL,
                       auto_populate_reply_metadata = FALSE,
                       media_alt_text = NULL,
                       lat = NULL,
                       long = NULL,
                       display_coordinates = FALSE) {

  ## if delete
  if (!is.null(destroy_id)) {
    lifecycle::deprecate_warn("1.0.0", "post_tweet(destroy_id)", "post_destroy()")
    return(post_destroy(destroy_id, token=token))
  }

  ## if retweet
  if (!is.null(retweet_id)) {
    stopifnot(is.character(retweet_id) && length(retweet_id) == 1)

    query <- sprintf("/1.1/statuses/retweet/%s", retweet_id)
    r <- TWIT_post(token, query)
    message("the tweet has been retweeted!")
    return(invisible(r))
  }

  stopifnot(is.character(status), length(status) == 1)
  ## media if provided
  if (!is.null(media)) {
    check_media(media, media_alt_text)
    media_id_string <- character(length(media))
    for (i in seq_along(media)) {
      media_id_string[[i]] <- upload_media_to_twitter(media[[i]], token, media_alt_text[[i]])
    }
    media_id_string <- paste(media_id_string, collapse = ",")
    params <- list(
      status = status,
      media_ids = media_id_string
    )
  } else {
    params <- list(
      status = status
    )
  }

  ## geotag if provided
  if (!is.null(lat) && !is.null(long)) {
    # Validate inputs
    if (!is.numeric(lat)) stop("`lat` must be numeric.")
    if (!is.numeric(long)) stop("`long` must be numeric.")

    if (!is.logical(display_coordinates)) {
      stop("`display_coordinates` must be TRUE/FALSE.")
    }

    if (abs(lat) > 90) stop("`lat` must be between -90 and 90 degrees.")
    if (abs(long) > 180) stop("`long` must be between -180 and 180 degrees.")

    params[["lat"]] <- as.double(lat)
    params[["long"]] <- as.double(long)

    if (display_coordinates) {
      params[["display_coordinates"]] <- "true"
    } else {
      params[["display_coordinates"]] <- "false"
    }
  }

  if (!is.null(in_reply_to_status_id)) {
    params[["in_reply_to_status_id"]] <- in_reply_to_status_id
  }
  if (auto_populate_reply_metadata) {
    params[["auto_populate_reply_metadata"]] <- "true"
  }

  r <- TWIT_post(token, "/1.1/statuses/update", params)
  message("Your tweet has been posted!")
  class(r) <- c("post_tweet", class(r))
  invisible(r)
}

#' Uploads media using chunked media endpoint
#'
#' @param media Path to media file (image or movie) to upload.
#' @inheritParams lookup_users
#' @noRd
upload_media_to_twitter <- function(media,
                                    token = NULL,
                                    alt_text = NULL,
                                    chunk_size = 5 * 1024 * 1024) {
  media_type <- switch(tools::file_ext(media),
    jpg = ,
    jpeg = "image/jpeg",
    png = "image/png",
    gif = "image/gif",
    mp4 = "video/mp4",
    stop("Unsupported file extension", call. = FALSE)
  )

  file_size <- file.size(media)

  if (file_size <= chunk_size && media_type != "video/mp4") {
    resp <- TWIT_upload(token, "/1.1/media/upload", list(
      media = httr::upload_file(media)
    ))
    media_id <- from_js(resp)$media_id_string
  } else {
    # https://developer.twitter.com/en/docs/twitter-api/v1/media/upload-media/uploading-media/chunked-media-upload

    # Initialize upload
    resp <- TWIT_upload(token, "/1.1/media/upload", list(
      command = "INIT",
      media_type = media_type,
      total_bytes = file_size
    ))
    media_id <- from_js(resp)$media_id_string

    # Send chunks
    bytes_sent <- 0
    videofile <- file(media, open = "rb")
    withr::defer(close(videofile))

    segment_id <- 0
    while (bytes_sent < file_size) {
      chunk <- readBin(videofile, chunk_size, what = "raw")
      resp <- TWIT_upload(token, "/1.1/media/upload", list(
        command = "APPEND",
        media_id = media_id,
        segment_index = segment_id,
        media = chunk
      ))

      segment_id <- segment_id + 1
      bytes_sent <- bytes_sent + chunk_size
    }

    # Finalize
    resp <- TWIT_upload(token, "/1.1/media/upload", list(
      command = "FINALIZE",
      media_id = media_id
    ))
    wait_for_chunked_media(resp, media_id, token)
  }

  if (!is.null(alt_text)) {
    # https://developer.twitter.com/en/docs/twitter-api/v1/media/upload-media/api-reference/post-media-metadata-create
    TWIT_upload(token, "/1.1/media/metadata/create",
      list(
        media_id = media_id,
        alt_text = list(text = substr(as.character(alt_text), 1, 1000))
      ),
      encode = "json"
    )
  }

  media_id
}

TWIT_upload <- function(token, api, body, ...) {
  TWIT_post(token, api, body = body, ..., host = "upload.twitter.com")
}

wait_for_chunked_media <- function(resp, media_id, token = NULL) {
  json <- from_js(resp)
  if (is.null(json$process_info)) {
    return()
  }

  params <- list(
    command = "STATUS",
    media_id = media_id
  )

  while (!json$processing_info$state %in% c("pending", "in_progress")) {
    Sys.sleep(json$processing_info$check_after_secs)

    json <- TWIT_get(token, "/1.1/media/upload",
      params = params,
      host = "upload.twitter.com"
    )
  }

  invisible()
}

check_media <- function(media, alt_text) {
  if (!is.character(media) | !is.character(alt_text)) {
    stop("Media and alt_text must be character vectors.", call. = FALSE)
  }
  media_type <- tools::file_ext(media)
  if (length(media) > 4) {
    stop("At most 4 images per plot can be uploaded.", call. = FALSE)
  }

  if (all(media_type %in% c("gif", "mp4")) && length(media) > 1) {
    stop("Cannot upload more than one gif or video per tweet.", call. = TRUE)
  }

  if (!is.null(alt_text) && length(alt_text) != length(media)) {
    stop("Alt text for media isn't provided for each image.", call. = TRUE)
  }
  if (!any(media_type %in% c("jpg", "jpeg", "png", "gif", "mp4"))) {
    stop("Media type format not recognized.", call. = TRUE)
  }

  if (any(nchar(alt_text) > 1000)) {
    stop("Alt text cannot be longer than 1000 characters.", call. = TRUE)
  }
}

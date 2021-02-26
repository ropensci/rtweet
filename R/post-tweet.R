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
#'        [the official API documentation](https://developer.twitter.com/en/docs/media/upload-media/api-reference/post-media-metadata-create)
#'        for more information.
#' @examples
#' \dontrun{
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
#' post_tweet("a tweet with media attachment", media = tmp)
#'
#' # example of replying within a thread
#' ## first post
#' post_tweet(status="first in a thread")
#'
#' ## lookup status_id
#' my_timeline <- get_timeline(rtweet:::home_user())
#'
#' ## ID for reply
#' reply_id <- my_timeline$status_id[1]
#'
#' ## post reply
#' post_tweet("second in the thread",
#'   in_reply_to_status_id = reply_id)
#' }
#' @family post
#' @aliases post_status
#' @export
post_tweet <- function(status = "my first rtweet #rstats",
                       media = NULL,
                       token = NULL,
                       in_reply_to_status_id = NULL,
                       destroy_id = NULL,
                       retweet_id = NULL,
                       auto_populate_reply_metadata = FALSE,
                       media_alt_text = NULL) {

  ## check token
  token <- check_token(token)

  ## if delete
  if (!is.null(destroy_id)) {
    ## validate destroy_id
    stopifnot(is.character(destroy_id) && length(destroy_id) == 1)
    ## build query
    query <- sprintf("statuses/destroy/%s", destroy_id)
    ## make URL
    url <- make_url(query = query)

    ## send request
    r <- TWIT(get = FALSE, url, token)

    ## if it didn't work return message
    if (r$status_code != 200) {
      return(httr::content(r))
    }
    ## if it did, print message and silently return response object
    message("your tweet has been deleted!")
    return(invisible(r))
  }

  ## if retweet
  if (!is.null(retweet_id)) {
    ## validate destroy_id
    stopifnot(is.character(retweet_id) && length(retweet_id) == 1)
    ## build query
    query <- sprintf("statuses/retweet/%s", retweet_id)
    ## make URL
    url <- make_url(query = query)

    ## send request
    r <- TWIT(get = FALSE, url, token)

    ## wait for status
    warn_for_twitter_status(r)

    ## if it didn't work return message
    if (r$status_code != 200) {
      return(r)
    }

    ## if it did, print message and silently return response object
    message("the tweet has been retweeted!")
    return(invisible(r))
  }

  ## validate
  stopifnot(is.character(status))
  stopifnot(length(status) == 1)

  ## update statuses query
  query <- "statuses/update"

  ## make sure encoding is utf-8
  enc <- getOption("encoding")
  on.exit(options(encoding = enc), add = TRUE)
  options(encoding = "UTF-8")

  ## validate status text – IF
  ##   (Part 1) status text is > 280 characters
  ##            ***AND***
  ##   (Part 2) status text does not include hyperlink
  ## ––––––––––logic:
  ##   Twitter will shorten long URLs (so the characters in any supplied URL may
  ##   not count 1:1 toward the 280 character limit); i'm not sure when and how
  ##   this works (we'd need to know exactly *when* and *to what extent* URLs
  ##   get shorted), so this is an inexact solution
  if (all(!is_tweet_length(status), !grepl("https?://\\S+", status))) {
    stop("cannot exceed 280 characters.", call. = FALSE)
  }
  if (length(status) > 1) {
    stop("can only post one status at a time",
         call. = FALSE)
  }

  ## media if provided
  if (!is.null(media)) {
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
  query <- "statuses/update"
  if (!is.null(in_reply_to_status_id)) {
    params[["in_reply_to_status_id"]] <- in_reply_to_status_id
  }

  if (auto_populate_reply_metadata) {
    params[["auto_populate_reply_metadata"]] <- "true"
  }

  url <- make_url(query = query, param = params)

  r <- TWIT(get = FALSE, url, token)

  if (r$status_code != 200) {
    return(httr::content(r))
  }
  message("your tweet has been posted!")
  invisible(r)
}

is_tweet_length <- function(.x, n = 280) {
  .x <- gsub("https?://[[:graph:]]+\\s?", "", .x)
  while (grepl("^@\\S+\\s+", .x)) {
    .x <- sub("^@\\S+\\s+", "", .x)
  }
  nchar(.x) <= n
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
  
  if (file_size <= chunk_size) {
    resp <- TWIT_upload(token, "media/upload", list(
      media = httr::upload_file(media)
    ))
    media_id <- from_js(resp)$media_id_string
  } else {
    # https://developer.twitter.com/en/docs/twitter-api/v1/media/upload-media/uploading-media/chunked-media-upload

    # Initialize upload
    resp <- TWIT_upload(token, "media/upload", list(
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
      resp <- TWIT_upload(token, "media/upload", list(
        command = "APPEND",
        media_id = media_id, 
        segment_index = segment_id, 
        media = chunk
      ))

      segment_id <- segment_id + 1
      bytes_sent <- bytes_sent + chunk_size
    }
    
    # Finalize
    resp <- TWIT_upload(token, "media/upload", list(
      command = "FINALIZE", 
      media_id = media_id
    ))
    wait_for_chunked_media(resp, media_id, token)
  }
  
  if (!is.null(alt_text)) {
    # https://developer.twitter.com/en/docs/twitter-api/v1/media/upload-media/api-reference/post-media-metadata-create
    TWIT_upload(token, "media/metadata/create", 
      list(
        media_id = media_id,
        alt_text = list(text = substr(as.character(alt_text), 1, 1000))
      ), 
      encode = "json"
    )
  }
  
  media_id
}

TWIT_upload <- function(token, api, params, ...) {
  TWIT_post(token, api, params, ..., host = "upload.twitter.com")
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
    
    json <- TWIT_get(token, "media/upload", 
      params = params, 
      host = "upload.twitter.com"
    )
  }

  invisible()
}




#' Uploads media using chunked media endpoint
#'
#' @param media Path to media file (image or movie) to upload. 
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @noRd 
upload_media_to_twitter <- function(media, token = NULL, alt_text = NULL) {
  media2upload <- httr::upload_file(media)
  token <- check_token(token)
  
  mediatype <- tools::file_ext(media)
  
  rurl <- "https://upload.twitter.com/1.1/media/upload.json"
  filesize <- file.size(media)
  gif_no_chunked <- mediatype == "gif" && filesize <= 5*1024*1024
  if (mediatype %in% c("jpg","jpeg", "png") || gif_no_chunked) {
    rpost <- httr::POST(rurl, body = list(media = media2upload), token)
    httr::stop_for_status(rpost)
    status_final <- httr::content(rpost)
  }
  if (mediatype %in% c("gif", "mp4")) {
    if (mediatype == "gif") {
      category <- "tweet_gif"
    } else {
      category <- "tweet_video"
    }
    init_r <- httr::POST(rurl, body = list(command = "INIT", 
                                           media_type = mediatype, 
                                           total_bytes = filesize,
                                           media_category = category), token)
    httr::stop_for_status(init_r)
    init_r_parsed <- httr::content(init_r)
    media_id <- init_r_parsed$media_id_string
    bytes_sent <- 0
    videofile <- file(media, open = "rb") 
    segment_id <- 0
    while (bytes_sent < filesize) {
      chunk <- readBin(videofile, 4*1024*1024, what = "raw")
      bytes_sent <- bytes_sent + 4*1024*1024
      request_data <- list(command = "APPEND", media_id = media_id, 
                           segment_index = segment_id, media = chunk)
      r <- httr::POST(rurl, body = request_data, token)
      httr::stop_for_status(r)
      segment_id <- segment_id + 1
    }
    close(videofile)
    finalize_data <- httr::POST(rurl, body = list(command = "FINALIZE", 
                                                  media_id = media_id),  token)
    httr::stop_for_status(finalize_data)
    status_final <- check_chunked_media_status(httr::content(finalize_data), token, rurl)
  }
  if (!is.null(alt_text) && nzchar(alt_text)) {
    if ("media_id_string" %in% names(status_final)) {
      rurl <- "https://upload.twitter.com/1.1/media/metadata/create.json"
      r <- httr::POST(
        rurl, 
        body = list(
          media_id = status_final[["media_id_string"]][1],
          alt_text = list(
            text = substr(as.character(alt_text[1]), 1, 1000)
          )
        ), 
        token,
        encode = "json"
      )
      httr::stop_for_status(r)
    }
  }
  
  return(status_final)
}

#' Checks status of chunked media upload`
#'
#' @param finalize_data Output of FINALIZE or STATUS command.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @param rurl Upload address for media.
#' @importFrom httr GET content
#' @noRd 
check_chunked_media_status = function(finalize_data, token, rurl) {
  if (finalize_data$processing_info$state == "succeeded") {
    return(finalize_data)
  } 
  if (finalize_data$processing_info$state %in% c("pending", "in_progress")) {
    Sys.sleep(finalize_data$processing_info$check_after_secs)
    status <- httr::GET(rurl, 
                        query = list(command = "STATUS",
                                     media_id = finalize_data$media_id_string),  
                        token)
    httr::stop_for_status(status)
    status <- check_chunked_media_status(httr::content(status), token, rurl)
    return(status)
  }
}


#' @importFrom tools file_ext
check_media <- function(media, alt_text) {
  media_type <- file_ext(media)
  if (length(media) > 4) {
    stop("At most 4 images per plot can be uploaded.", call. = FALSE)
  }
  
  if (media_type %in% c("gif", "mp4") && length(media) > 1) {
    stop("Cannot upload more than one gif or video per tweet.", call. = TRUE)
  }
  
  if (!is.null(alt_text) && length(alt_text) != length(media)) {
    stop("Alt text for media isn't provided for each image.", call. = TRUE)
  }
  if (!any(media_type %in% c("jpg", "jpeg", "png", "gif", "mp4"))) {
    stop("Media type format not recognized.", call. = TRUE)
  }
  
  if (any(nchar(alt_text) > 1000)) {
    stop("Alt text cannot be bigger than 1000 characters.", call. = TRUE)
  }
}

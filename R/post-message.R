#' Posts direct message from user's Twitter account
#'
#' @inheritParams lookup_users
#' @param text Character, text of message.
#' @param user Screen name or user ID of message target.
#' @param media File path to image or video media to be
#'   included in tweet.
#' @importFrom httr POST upload_file content
#' @export
post_message <- function(text, user, media = NULL, token = NULL) {
  ## get user id
  user_id <- lookup_users(user)
  user_id <- user_id$user_id[1]
  stopifnot(is.character(text))
  stopifnot(length(text) == 1)
  query <- "direct_messages/events/new"
  body <- list(
    event = list(type = "message_create",
      message_create = list(target = list(recipient_id = user_id),
    message_data = list(text = text))))
  body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  if (length(text) > 1) {
    stop("can only post one message at a time",
         call. = FALSE)
  }
  token <- check_token(token)
  ## media if provided
  if (!is.null(media)) {
    media2upload <- httr::upload_file(media)
    rurl <- paste0(
      "https://upload.twitter.com/1.1/media/upload.json"
    )
    r <- httr::POST(rurl, body = list(media = media2upload), token)
    httr::stop_for_status(r)
    r <- httr::content(r, "parsed")
    params <- list(
      media_ids = r$media_id_string
    )
  } else {
    params <- NULL
  }
  #names(params)[2] <- .id_type(user)
  query <- "direct_messages/events/new"
  url <- make_url(query = query, param = params)

  r <- TWIT(get = FALSE, url, token, body = body)
  if (r$status_code != 200) {
    return(httr::content(r))
  }
  message("your tweet has been posted!")
  invisible(r)
}

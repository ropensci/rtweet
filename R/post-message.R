#' Posts direct message from user's Twitter account
#'
#' @inheritParams lookup_users
#' @param text Character, text of message.
#' @param user Screen name or user ID of message target.
#' @param media File path to image or video media to be
#'   included in tweet.
#' @export
post_message <- function(text, user, media = NULL, token = NULL) {
  ## get user id
  user_id <- lookup_users(user)
  user_id <- user_id$user_id[1]
  stopifnot(is.character(text))
  stopifnot(length(text) == 1)

  body <- list(
    event = list(
      type = "message_create",
      message_create = list(
        target = list(recipient_id = user_id),
        message_data = list(text = text)
      )
    )
  )
  
  if (!is.null(media)) {
    media_id <- upload_media_to_twitter(media, token = token)
    body$event$message_create$message_data$attachment <- list(
      type = "media",
      media = list(id = media_id)
    )
  }

  r <- TWIT_post(token, "direct_messages/events/new", 
    body = body, 
    encode = "json"
  )
  message("your DM has been posted!")
  invisible(r)
}

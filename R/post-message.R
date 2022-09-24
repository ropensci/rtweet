#' Posts direct message from user's Twitter account
#'
#' @inheritParams get_timeline
#' @param text Character, text of message.
#' @param media File path to image or video media to be
#'   included in tweet.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/direct-messages/sending-and-receiving/api-reference/new-event>
post_message <- function(text, user, media = NULL, token = NULL) {
  ## get user id
  if (user_type(user) != "user_id") {
    user_id <- lookup_users(user, token = token)
    user_id <- user_id$id_str[1]
  } else {
    user_id <- user
  }

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

  r <- TWIT_post(token, "/1.1/direct_messages/events/new",
    body = body,
    encode = "json"
  )
  message("Your DM has been posted!")
  invisible(r)
}

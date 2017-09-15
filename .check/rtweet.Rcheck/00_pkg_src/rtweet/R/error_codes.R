
check_if_error_codes <- function(r) {
  stopifnot(inherits(r, "response"))
  if (!grepl("twitter\\.com", r[["url"]])) {
    warnmessage <- "This HTTP request was not made to a twitter.com API."
    return(warnmessage)
  }
  if (r[["status_code"]] == 200) {
    return(FALSE)
  }
  e <- httr::content(r)
  if (!"errors" %in% names(e) || !"code" %in% names(e[["errors"]][[1]])) {
    warnmessage <- paste0(
      "HTTP status code was ",
      r[["status_code"]],
      ", but no error message was found."
    )
    return(warnmessage)
  }
  e <- e[["errors"]][[1]][["code"]]
  if (!is.integer(e)) {
    e <- suppressWarnings(as.integer("a"))
    if (length(e) > 1L || is.na(e)) {
      warnmessage <- paste0(
        "HTTP status code was ",
        r[["status_code"]],
        ", but no error code was provided."
      )
      return(warnmessage)
    }
  }
  if (e == 32) {
    warnmessage <- "Could not authenticate you. Your call could not be completed as dialed."
  } else if (e == 34) {
    warnmessage <- "Sorry, that page does not exist. The specified resource was not found."
  } else if (e == 36) {
    warnmessage <- "You cannot report yourself for spam. You cannot use your own user ID in a report spam call."
  } else if (e == 44) {
    warnmessage <- "attachment_url parameter is invalid. The URL value provided is not a URL that can be attached to this Tweet."
  } else if (e == 50) {
    warnmessage <- "User not found. The user is not found."
  } else if (e == 63) {
    warnmessage <- "User has been suspended. The user account has been suspended and information cannot be retrieved."
  } else if (e == 64) {
    warnmessage <- "Your account is suspended and is not permitted to access this feature. The access token being used belongs to a suspended user and they can't complete the action you're trying to take."
  } else if (e == 68) {
    warnmessage <- "The Twitter REST API v1 is no longer active. Please migrate to API v1.1."
  } else if (e == 87) {
    warnmessage <- "Client is not permitted to perform this action. The endpoint called is not a permitted URL."
  } else if (e == 88) {
    warnmessage <- "Rate limit exceeded. The request limit for this resource has been reached for the current rate limit window."
  } else if (e == 89) {
    warnmessage <- "Invalid or expired token. The access token used in the request is incorrect or has expired."
  } else if (e == 92) {
    warnmessage <- "SSL is required. Only SSL connections are allowed in the API, you should update your request to a secure connection. See how to connect using TLS."
  } else if (e == 93) {
    warnmessage <- "This application is not allowed to access or delete your direct messages. The OAuth token does not provide access to Direct Messages."
  } else if (e == 99) {
    warnmessage <- "Unable to verify your credentials. The OAuth credentials cannot be validated. Check that the token is still valid."
  } else if (e == 130) {
    warnmessage <- "Over capacity. Twitter is temporarily over capacity."
  } else if (e == 131) {
    warnmessage <- "Internal error. An unknown internal error occurred."
  } else if (e == 135) {
    warnmessage <- "Could not authenticate you. Timestamp out of bounds (check your system clock)."
  } else if (e == 144) {
    warnmessage <- "No status found with that ID. The requested Tweet ID is not found (if it existed, it was probably deleted)."
  } else if (e == 150) {
    warnmessage <- "You cannot send messages to users who are not following you. Sending a Direct Message failed."
  } else if (e == 151) {
    warnmessage <- "There was an error sending your message: reason. Sending a Direct Message failed. The reason value will provide more information."
  } else if (e == 161) {
    warnmessage <- "You are unable to follow more people at this time. Thrown when a user cannot follow another user due to some kind of limit."
  } else if (e == 179) {
    warnmessage <- "Sorry, you are not authorized to see this status. Thrown when a Tweet cannot be viewed by the authenticating user, usually due to the Tweet's author having protected their Tweets."
  } else if (e == 185) {
    warnmessage <- "User is over daily status update limit. Thrown when a Tweet cannot be posted due to the user having no allowance remaining to post. Despite the text in the error message indicating that this error is only thrown when a daily limit is reached, this error will be thrown whenever a posting limitation has been reached. Posting allowances have roaming windows of time of unspecified duration."
  } else if (e == 187) {
    warnmessage <- "Status is a duplicate. The status text has already been Tweeted by the authenticated account."
  } else if (e == 205) {
    warnmessage <- "You are over the limit for spam reports. The account limit for reporting spam has been reached. Try again later."
  } else if (e == 215) {
    warnmessage <- "Bad authentication data. The method requires authentication but it was not presented or was wholly invalid."
  } else if (e == 220) {
    warnmessage <- "Your credentials do not allow access to this resource. The authentication token in use is restricted and cannot access the requested resource."
  } else if (e == 226) {
    warnmessage <- "This request looks like it might be automated. To protect our users from spam and other malicious activity, we can't complete this action right now. We constantly monitor and adjust our filters to block spam and malicious activity on the Twitter platform. These systems are tuned in real-time. If you get this response our systems have flagged the Tweet or DM as possibly fitting this profile. If you feel that the Tweet or DM you attempted to create was flagged in error, please report the details around that to us by filing a ticket at https://support.twitter.com/forms/platform."
  } else if (e == 231) {
    warnmessage <- "User must verify login. Returned as a challenge in xAuth when the user has login verification enabled on their account and needs to be directed to twitter.com to generate a temporary password ."
  } else if (e == 251) {
    warnmessage <- "This endpoint has been retired and should not be used."
  } else if (e == 261) {
    warnmessage <- "Application cannot perform write actions. Thrown when the application is restricted from POST, PUT, or DELETE actions. See How to appeal application suspension and other disciplinary actions ."
  } else if (e == 271) {
    warnmessage <- "You can't mute yourself. The authenticated user account cannot mute itself."
  } else if (e == 272) {
    warnmessage <- "You are not muting the specified user. The authenticated user account is not muting the account a call is attempting to unmute."
  } else if (e == 323) {
    warnmessage <- "Animated GIFs are not allowed when uploading multiple images. Only one animated GIF is allowed to be attached to a single Tweet."
  } else if (e == 324) {
    warnmessage <- "The validation of media ids failed. There was a problem with the media ID submitted with the Tweet."
  } else if (e == 325) {
    warnmessage <- "A media id was not found. The media ID attached to the Tweet was not found."
  } else if (e == 326) {
    warnmessage <- "To protect our users from spam and other malicious activity, this account is temporarily locked. The user should log in to https://twitter.com to unlock their account."
  } else if (e == 354) {
    warnmessage <- "The text of your direct message is over the max character limit. The message size exceeds the number of characters permitted in a direct message."
  } else if (e == 385) {
    warnmessage <- "You attempted to reply to a tweet that is deleted or not visible to you. A reply can only be sent with reference to an existing public Tweet."
  } else if (e == 386) {
    warnmessage <- "The Tweet exceeds the number of allowed attachment types. A Tweet is limited to a single attachment resource (media, Quote Tweet, etc.)."
  }
  warnmessage
}

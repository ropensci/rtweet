#' Posts status update to user's Twitter account
#'
#' @param status Character, tweet status. Must be 280 characters or less.
#' @param media File path to image or video media to be included in tweet.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable tokens.
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
#' @examples
#' \dontrun{
#' ## generate data to make/save plot (as a .png file)
#' x <- rnorm(300)
#' y <- x + rnorm(300, 0, .75)
#' col <- c(rep("#002244aa", 50), rep("#440000aa", 50))
#' bg <- c(rep("#6699ffaa", 50), rep("#dd6666aa", 50))
#'
#' ## crate temporary file name
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
                       auto_populate_reply_metadata = FALSE) {

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
    r <- vector("list", length(media))
    media_id_string <- vector("list", length(media))
    for (i in seq_along(media)) {
      r[[i]] <- upload_media_to_twitter(media[[i]], token)
      if (has_name_(r[[i]], "media_id_string")) {
        media_id_string[[i]] <- r[[i]]$media_id_string
      } else {
        stop(paste("media file number", i, "failed to upload"), call. = FALSE)
      }
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

upload_media_to_twitter <- function(media, token) {
  media2upload <- httr::upload_file(media)
  query <- "media/upload"
  rurl <- paste0(
    "https://upload.twitter.com/1.1/media/upload.json"
  )
  r <- httr::POST(rurl, body = list(media = media2upload), token)
  httr::content(r)
}


#' Posts direct message from user's Twitter account
#'
#' @param text Character, text of message.
#' @param user Screen name or user ID of message target.
#' @param media File path to image or video media to be
#'   included in tweet.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
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

#' Follows target twitter user.
#'
#' @param user Screen name or user id of target user.
#' @param destroy Logical indicating whether to post (add) or
#'   remove (delete) target tweet as favorite.
#' @param mute Logical indicating whether to mute the intended
#'   friend (you must already be following this account prior
#'   to muting them)
#' @param notify Logical indicating whether to enable notifications
#'   for target user. Defaults to false.
#' @param retweets Logical indicating whether to enable retweets
#'   for target user. Defaults to true.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @aliases follow_user
#' @examples
#' \dontrun{
#' post_follow("BarackObama")
#' }
#' @family post
#' @export
post_follow <- function(user,
                        destroy = FALSE,
                        mute = FALSE,
                        notify = FALSE,
                        retweets = TRUE,
                        token = NULL) {

  stopifnot(is.atomic(user), is.logical(notify))

  token <- check_token(token)

  if (all(!destroy, !retweets)) {
    query <- "friendships/update"
    params <- list(
      user_type = user,
      notify = notify,
      retweets = retweets)
  } else if (mute) {
    query <- "mutes/users/create"
    params <- list(
      user_type = user)
  } else if (destroy) {
    query <- "friendships/destroy"
    params <- list(
      user_type = user,
      notify = notify)
  } else {
    query <- "friendships/create"
    params <- list(
      user_type = user,
      notify = notify
    )
  }

  names(params)[1] <- .id_type(user)
  url <- make_url(query = query, param = params)
  r <- TWIT(get = FALSE, url, token)
  if (!check_status_code(r)) {
    return(httr::content(r))
  }
  r
}

check_status_code <- function(x) {
  if (has_name_(x, "status_code") && is.integer(x$status_code)) {
    status <- x$status_code
  } else if (has_name_(x, "status") && is.integer(x$status)) {
    status <- x$status
  } else if (any(grepl("status", names(x)))) {
    int <- sapply(
      x[grep("status", names(x))],
      is.integer
    )
    if (sum(int) > 0L) {
      status <- x[grep("status", names(x))][int][1]
    } else {
      return(FALSE)
    }
  } else if (any(grepl("code", names(x)))) {
    int <- sapply(
      x[grep("code", names(x))],
      is.integer
    )
    if (sum(int) > 0L) {
      status <- x[grep("code", names(x))][int][1]
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
  if (!is.integer(status)) {
    return(FALSE)
  }
  if (status == 200) {
    return(TRUE)
  }
  FALSE
}


#' @aliases unfollow_user
#' @rdname post_follow
#' @export
post_unfollow_user <- function(user, token = NULL) {
  post_follow(user, destroy = TRUE, token = token)
}

#' @aliases unfollow_user
#' @rdname post_follow
#' @aliases mute_user
#' @export
post_mute <- function(user, token = NULL) {
  post_follow(user, mute = TRUE, token = token)
}


#' Favorites target status id.
#'
#' @param status_id Status id of target tweet.
#' @param destroy Logical indicating whether to post (add) or
#'   remove (delete) target tweet as favorite.
#' @param include_entities Logical indicating whether to
#'   include entities object in return.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @aliases post_favourite favorite_tweet
#' @examples
#' \dontrun{
#' rt <- search_tweets("rstats")
#' r <- lapply(rt$user_id, post_favorite)
#' }
#' @family post
#' @export
post_favorite <- function(status_id,
                          destroy = FALSE,
                          include_entities = FALSE,
                          token = NULL) {

  stopifnot(is.atomic(status_id))

  token <- check_token(token)

  if (destroy) {
    query <- "favorites/destroy"
  } else {
    query <- "favorites/create"
  }

  params <- list(
    id = status_id)

  url <- make_url(query = query, param = params)

  r <- TWIT(get = FALSE, url, token)

  if (!check_status_code(r)) {
    return(httr::content(r))
  }
  invisible(r)
}


#' Updates friendship notifications and retweet abilities.
#'
#' @param user Screen name or user id of target user.
#' @param device Logical indicating whether to enable or disable
#'    device notifications from target user behaviors. Defaults
#'    to false.
#' @param retweets Logical indicating whether to enable or disable
#'    retweets from target user behaviors. Defaults to false.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @aliases friendship_update
#' @family post
#' @export
post_friendship <- function(user,
                            device = FALSE,
                            retweets = FALSE,
                            token = NULL) {

  stopifnot(is.atomic(user), is.logical(device),
            is.logical(retweets))

  token <- check_token(token)

  query <- "friendships/update"

  params <- list(
    user_type = user,
    device = device,
    retweets = retweets)

  names(params)[1] <- .id_type(user)

  url <- make_url(query = query, param = params)

  r <- TWIT(get = FALSE, url, token)

  if (!check_status_code(r)) {
    return(httr::content(r))
  }
  invisible(r)
}



post_list_create <- function(name,
                             description = NULL,
                             private = FALSE,
                             token = NULL) {

  stopifnot(is.atomic(name), length(name) == 1, is.logical(private))

  token <- check_token(token)

  query <- "lists/create"

  if (private) {
    mode <- "private"
  } else {
    mode <- "public"
  }

  params <- list(
    name = name,
    mode = mode,
    description = description)

  url <- make_url(query = query, param = params)

  r <- TWIT(get = FALSE, url, token)

  warn_for_twitter_status(r)

  r
}



post_list_add_one <- function(user,
                              list_id = NULL,
                              slug = NULL,
                              token = NULL) {
  ## must id list via numeric ID or slug
  if (is.null(list_id) && is.null(slug)) {
    stop("must supply either list_id or slug to identify pre-existing list")
  }

  ## check and reformat users
  stopifnot(is.character(user))
  if (length(user) > 1) {
    warning("Can only add 1 users at a time via this method. Adding user[]...",
      call. = FALSE)
    user <- user[1]
  }
  users_param_name <- .ids_type(user)

  ## check token
  token <- check_token(token)

  ## specify API path
  query <- "lists/members/create"

  ## if list id
  if (!is.null(list_id)) {
    stopifnot(is.atomic(list_id), length(list_id) == 1)
    params <- list(
      list_id = list_id,
      user = user
    )

  ## if slug
  } else {
    stopifnot(is.atomic(slug), length(slug) == 1)
    params <- list(
      slug = slug,
      owner_screen_name = home_user(),
      user = user
    )
  }
  ## rename last param
  names(params)[length(params)] <- users_param_name

  ## build URL
  url <- make_url(query = query, param = params)

  ## send request
  r <- TWIT(get = FALSE, url, token)

  ## check status
  warn_for_twitter_status(r)

  ## return response object
  r
}


post_list_destroy <- function(list_id = NULL,
                              slug = NULL,
                              token = NULL) {
  if (!is.null(list_id)) {

    stopifnot(is.atomic(list_id), length(list_id) == 1)
    params <- list(list_id = list_id)

  } else if (!is.null(slug)) {

    stopifnot(is.atomic(slug), length(slug) == 1)
    params <- list(slug = slug, owner_screen_name = home_user())

  } else {
    stop("must supply list_id or slug")
  }

  token <- check_token(token)

  query <- "lists/destroy"

  url <- make_url(query = query, param = params)

  r <- TWIT(get = FALSE, url, token)

  warn_for_twitter_status(r)

  r
}

post_list_create_all <- function(users,
                                 list_id = NULL,
                                 slug = NULL,
                                 token = NULL) {
  ## must id list via numeric ID or slug
  if (is.null(list_id) && is.null(slug)) {
    stop("must supply either list_id or slug to identify pre-existing list")
  }

  ## check and reformat users
  stopifnot(is.character(users))
  if (length(users) > 100) {
    warning("Can only add 100 users at a time. Adding users[1:100]...",
      call. = FALSE)
    users <- users[1:100]
  }
  users_param_name <- .ids_type(users)
  users <- paste0(users, collapse = ",")

  ## check token
  token <- check_token(token)

  ## specify API path
  query <- "lists/members/create_all"

  ## if list id
  if (!is.null(list_id)) {
    stopifnot(is.atomic(list_id), length(list_id) == 1)
    params <- list(
      list_id = list_id,
      users = users
    )

  ## if slug
  } else {
    stopifnot(is.atomic(slug), length(slug) == 1)
    params <- list(
      slug = slug,
      owner_screen_name = home_user(),
      users = users
    )
  }
  ## rename last param
  names(params)[length(params)] <- users_param_name

  ## build URL
  url <- make_url(query = query, param = params)

  ## send request
  r <- TWIT(get = FALSE, url, token)

  ## check status
  warn_for_twitter_status(r)

  ## return response object
  r
}

post_list_destroy_all <- function(users,
                                  list_id = NULL,
                                  slug = NULL,
                                  token = NULL) {
  ## must id list via numeric ID or slug
  if (is.null(list_id) && is.null(slug)) {
    stop("must supply either list_id or slug to identify pre-existing list")
  }

  ## check and reformat users
  stopifnot(is.character(users))
  if (length(users) > 100) {
    warning("Can only drop 100 users at a time. Dropping users[1:100]...",
      call. = FALSE)
    users <- users[1:100]
  }
  users_param_name <- .ids_type(users)
  users <- paste0(users, collapse = ",")

  ## check token
  token <- check_token(token)

  ## specify API path
  query <- "lists/members/destroy_all"

  ## if list id
  if (!is.null(list_id)) {
    stopifnot(is.atomic(list_id), length(list_id) == 1)
    params <- list(
      list_id = list_id,
      users = users
    )

  ## if slug
  } else {
    stopifnot(is.atomic(slug), length(slug) == 1)
    params <- list(
      slug = slug,
      owner_screen_name = home_user(),
      users = users
    )
  }
  ## rename last param
  names(params)[length(params)] <- users_param_name

  ## build URL
  url <- make_url(query = query, param = params)

  ## send request
  r <- TWIT(get = FALSE, url, token)

  ## check status
  warn_for_twitter_status(r)

  ## return response object
  r
}


#' Manage Twitter lists
#'
#' Create, add users, and destroy Twitter lists
#'
#' @param users Character vectors of users to be added to list.
#' @param name Name of new list to create.
#' @param description Optional, description of list (single character string).
#' @param private Logical indicating whether created list should be private.
#'   Defaults to false, meaning the list would be public. Not applicable if list
#'   already exists.
#' @param destroy Logical indicating whether to delete a list. Either `list_id` or
#'   `slug` must be provided if `destroy = TRUE`.
#' @param list_id Optional, numeric ID of list.
#' @param slug Optional, list slug.
#' @param token OAuth token associated with user who owns [or will own] the list
#'   of interest. Token must have write permissions!
#' @return Response object from HTTP request.
#' @examples
#' \dontrun{
#'
#' ## CNN twitter accounts
#' users <- c("cnn", "cnnbrk", "cnni", "cnnpolitics", "cnnmoney",
#'   "cnnnewsroom", "cnnspecreport", "CNNNewsource",
#'   "CNNNSdigital", "CNNTonight")
#'
#' ## create CNN-accounts list with 9 total users
#' (cnn_lst <- post_list(users,
#'   "cnn-accounts", description = "Official CNN accounts"))
#'
#' ## view list in browser
#' browseURL(sprintf("https://twitter.com/%s/lists/cnn-accounts",
#'   rtweet:::home_user()))
#'
#' ## search for more CNN users
#' cnn_users <- search_users("CNN", n = 200)
#'
#' ## filter and select more users to add to list
#' more_users <- cnn_users %>%
#'   subset(verified & !tolower(screen_name) %in% tolower(users)) %>%
#'   .$screen_name %>%
#'   grep("cnn", ., ignore.case = TRUE, value = TRUE)
#'
#' ## add more users to list- note: can only add up to 100 at a time
#' post_list(users = more_users, slug = "cnn-accounts")
#'
#' ## view updated list in browser (should be around 100 users)
#' browseURL(sprintf("https://twitter.com/%s/lists/cnn-accounts",
#'   rtweet:::home_user()))
#'
#' ## select users on list without "cnn" in their name field
#' drop_users <- cnn_users %>%
#'   subset(screen_name %in% more_users & !grepl("cnn", name, ignore.case = TRUE)) %>%
#'   .$screen_name
#'
#' ## drop these users from the cnn list
#' post_list(users = drop_users, slug = "cnn-accounts",
#'   destroy = TRUE)
#'
#' ## view updated list in browser (should be around 100 users)
#' browseURL(sprintf("https://twitter.com/%s/lists/cnn-accounts",
#'   rtweet:::home_user()))
#'
#' ## delete list entirely
#' post_list(slug = "cnn-accounts", destroy = TRUE)
#'
#' }
#' @export
post_list <- function(users = NULL,
                      name = NULL,
                      description = NULL,
                      private = FALSE,
                      destroy = FALSE,
                      list_id = NULL,
                      slug = NULL,
                      token = NULL) {
  ## this took me forever to figure out but gotta have ut8-encoding
  ## for the comma separated IDs
  op <- getOption("encoding")
  on.exit(options(encoding = op), add = TRUE)
  options(encoding = "UTF-8")

  ## destroy list
  if (destroy && is.null(users)) {
    return(post_list_destroy(list_id, slug, token))
  }

  ## drop users from list
  if (destroy) {
    return(post_list_destroy_all(users, list_id, slug, token))
  }

  ## create list
  if (is.null(list_id) && is.null(slug)) {
    r <- post_list_create(name, description, private, token)
    if (r$status_code != 200) {
      stop("failed to create list")
    }
    ## use returned list ID to add users
    tl <- from_js(r)
    list_id <- tl$id_str
  }

  ## add users to list one at a time
  #post_list_create_all(users, list_id, slug, token)
  r <- list()
  for (i in seq_along(users)) {
    r[[length(r) + 1]] <- post_list_add_one(users[i], list_id = list_id, slug = slug, token = token)
  }
  if (r[[length(r)]]$status_code == 200) {
    message("Successfully added users to list!")
    invisible(r)
  } else {
    message(httr::content(r))
    r
  }
}

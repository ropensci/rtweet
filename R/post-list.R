#' Manage Twitter lists
#'
#' Create, add users, and destroy Twitter lists
#'
#' @inheritParams lookup_users
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

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
#'   rtweet:::api_screen_name()))
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
#'   rtweet:::api_screen_name()))
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
#'   rtweet:::api_screen_name()))
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
    tl <- from_js(r)
    list_id <- tl$id_str
  }

  ## add users to list one at a time
  #post_list_create_all(users, list_id, slug, token)
  r <- list()
  for (i in seq_along(users)) {
    r[[length(r) + 1]] <- post_list_add_one(users[i], list_id = list_id, slug = slug, token = token)
  }

  invisible(r)
}

post_list_create <- function(name,
                             description = NULL,
                             private = FALSE,
                             token = NULL) {

  stopifnot(is.atomic(name), length(name) == 1, is.logical(private))

  if (private) {
    mode <- "private"
  } else {
    mode <- "public"
  }

  params <- list(
    name = name,
    mode = mode,
    description = description
  )
  TWIT_post(token, "/1.1/lists/create", params)
}



post_list_add_one <- function(user,
                              list_id = NULL,
                              slug = NULL,
                              token = NULL) {

  ## check and reformat users
  stopifnot(is.character(user))
  if (length(user) > 1) {
    warning("Can only add 1 users at a time via this method. Adding user[]...",
      call. = FALSE)
    user <- user[1]
  }
  
  params <- my_list_params(token,
    users = user,
    list_id = list_id,
    slug = slug
  )
  TWIT_post(token, "/1.1/lists/members/create", params)
}


post_list_destroy <- function(list_id = NULL,
                              slug = NULL,
                              token = NULL) {
  
  params <- my_list_params(token,
    list_id = list_id,
    slug = slug
  )
  TWIT_post(token, "/1.1/lists/destroy", params)
}

post_list_create_all <- function(users,
                                 list_id = NULL,
                                 slug = NULL,
                                 token = NULL) {
  stopifnot(is.character(users))
  if (length(users) > 100) {
    warning("Can only add 100 users at a time. Adding users[1:100]...",
      call. = FALSE)
    users <- users[1:100]
  }
  
  params <- my_list_params(token,
    users = users,
    list_id = list_id,
    slug = slug
  )
  TWIT_post(token, "/1.1/lists/members/create_all", params)
}

post_list_destroy_all <- function(users,
                                  list_id = NULL,
                                  slug = NULL,
                                  token = NULL) {
  
  stopifnot(is.character(users))
  if (length(users) > 100) {
    warning("Can only drop 100 users at a time. Dropping users[1:100]...",
      call. = FALSE)
    users <- users[1:100]
  }

  params <- my_list_params(token,
    users = users,
    list_id = list_id,
    slug = slug
  )
  TWIT_post(token, "lists/members/destroy_all", params = params)
}

my_list_params <- function(token, slug = NULL, list_id = NULL, ..., users = NULL) {
  params <- list(...)
  
  if (!is.null(list_id) && is.null(slug)) {
    stopifnot(is.atomic(list_id), length(list_id) == 1)
    
    params$list_id <- list_id
  } else if (is.null(slug) && !is.null(list_id)) {
    stopifnot(is.atomic(slug), length(slug) == 1)
    
    params$slug <- slug
    params$owner_screen_name = api_screen_name(token)
  } else {
    abort("Must supply exactly one of `list_id` or `slug` to identify a list")
  } 
  
  if (!is.null(users)) {
    params[[.ids_type(users)]] <- paste0(users, collapse = ",")
  }
  
}

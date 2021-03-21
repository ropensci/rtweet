lists_params <- function(list_id = NULL, slug = NULL, owner_user = NULL, ...) {
  params <- list(...)

  if (is.null(list_id) && !is.null(slug) & !is.null(owner_user)) {
    params$slug <- slug
    params[[paste0("owner_", user_type(owner_user, "owner_user"))]] <- owner_user
  } else {
    params$list_id <- list_id
  }
  params
}

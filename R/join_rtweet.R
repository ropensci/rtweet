
join_rtweet <- function(x) {
  if (!is.data.frame(x) || (nrow(x) == 0 &&
      (NROW(attr(x, "users")) == 0) &&
      (NROW(attr(x, "tweets")) == 0))) {
    return(data.frame())
  }
  if ("users" %in% names(attributes(x))) {
    tw <- as_tbl(x)
    ## get users data
    us <- as_tbl(attr(x, "users"))
    ## they should be same length; if so fill in user rows w/o tweets
    if (NROW(tw) == NROW(us)) {
      tw$user_id[is.na(tw$user_id)] <- us$user_id[is.na(tw$user_id)]
      tw$screen_name[is.na(tw$screen_name)] <- us$screen_name[is.na(tw$screen_name)]
    }
    ## if users data is empty, create NA-filled users data for tw$user_id
    if (NROW(us) == 0) {
      us <- as_tbl(data.frame(
        list(tw$user_id, as.list(rep(NA, length(usercols_())))),
        stringsAsFactors = FALSE, check.rows = FALSE, check.names = FALSE,
        row.names = NULL))
      names(us) <- names(usercols_())
      ## if tweets data is empty, create NA-filled tweets data for us$user_id
    } else if (NROW(tw) == 0) {
      tw <- as_tbl(data.frame(
        list(us$user_id, as.list(rep(NA, length(statuscols_())))),
        stringsAsFactors = FALSE, check.rows = FALSE, check.names = FALSE,
        row.names = NULL))
      names(tw) <- names(statuscols_())
      if (NROW(tw) == NROW(us)) {
        tw$user_id[is.na(tw$user_id)] <- us$user_id[is.na(tw$user_id)]
        tw$screen_name[is.na(tw$screen_name)] <- us$screen_name[is.na(tw$screen_name)]
      }
      ## if any us$users are not in tw$users
    } else if (any(!us$user_id %in% tw$user_id)) {
      tw2 <- as_tbl(data.frame(
        list(as.list(rep(NA, 2)),
          unique(us$user_id[!us$user_id %in% tw$user_id]),
          as.list(rep(NA, length(statuscols_()) - 3))),
        stringsAsFactors = FALSE, check.rows = FALSE, check.names = FALSE,
        row.names = NULL))
      names(tw2) <- names(statuscols_())
      tw <- rbind(tw, tw2)
    }
    ## remove screen name
    us <- us[, names(us) != "screen_name"]
    ## remove duplicate user rows
    us <- us[!duplicated(us$user_id), ]
    ## merge tweets and users data
    x <- merge(tw, us, by = "user_id", sort = FALSE)
    ## remove duplicate tweet rows
    x <- as_tbl(x[!(duplicated(x$status_id) & !is.na(x$status_id)), ])
  } else if ("tweets" %in% names(attributes(x))) {
    us <- as_tbl(x)
    ## get tweets data
    tw <- as_tbl(attr(x, "tweets"))
    ## they should be same length; if so fill in user rows w/o tweets
    if (NROW(tw) == NROW(us)) {
      tw$user_id[is.na(tw$user_id)] <- us$user_id[is.na(tw$user_id)]
      tw$screen_name[is.na(tw$screen_name)] <- us$screen_name[is.na(tw$screen_name)]
    }
    ## if tweets data is empty, create NA-filled tweets data for us$user_id
    if (NROW(tw) == 0) {
      tw <- as_tbl(data.frame(
        list(us$user_id, as.list(rep(NA, length(usercols_())))),
        stringsAsFactors = FALSE, check.rows = FALSE, check.names = FALSE,
        row.names = NULL))
      names(tw) <- names(usercols_())
      ## if users data is empty, create NA-filled users data for tw$user_id
    } else if (NROW(us) == 0) {
      us <- as_tbl(data.frame(
        list(tw$user_id, as.list(rep(NA, length(statuscols_())))),
        stringsAsFactors = FALSE, check.rows = FALSE, check.names = FALSE,
        row.names = NULL))
      names(us) <- names(statuscols_())
      ## if any us$users are not in tw$users
    } else if (any(!us$user_id %in% tw$user_id)) {
      tw2 <- as_tbl(data.frame(list(
        unique(us$user_id[!us$user_id %in% tw$user_id]),
        as.list(rep(NA, length(statuscols_()) - 1))),
        stringsAsFactors = FALSE, check.rows = FALSE, check.names = FALSE,
        row.names = NULL))
      names(tw2) <- names(statuscols_())
      tw <- rbind(tw, tw2)
    }
    ## remove screen name
    us <- us[, names(us) != "screen_name"]
    ## remove duplicate user rows
    us <- us[!duplicated(us$user_id), ]
    ## merge tweets and users data
    x <- merge(tw, us, by = "user_id", sort = FALSE)
    ## remove duplicate tweet rows
    x <- as_tbl(x[!(duplicated(x$status_id) & !is.na(x$status_id)), ])
  }
  x
}


as_tbl <- function(x, ...) {
  tibble::as_tibble(x, ...)
}

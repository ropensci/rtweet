#' Twitter Tokens sitrep
#'
#' Get a situation report of your current tokens; useful for upgrading from
#' rtweet 0.7.0 to 1.0.0 and diagnosing problems with tokens.
#'
#' Prints rtweet tokens on the old folder (rtweet < 0.7.0) and on the
#' new (rtweet > 1.0.0) default location.
#' For each folder it reports apps and then users and bots authentications.
#' For users authentications it reports the user_id, so that you can check who is that user.
#'
#' Users should follow its advise, if there is no advise but there are still
#' some problems authenticating regenerate the authentications.
#' @note It is safe to use in public, as instead of the tokens or keys it reports a letter.
#' @return Invisibly, TRUE if some problems were found and FALSE otherwise
#' @export
#' @seealso [auth_as()]
#' @examples
#' auth_sitrep()
auth_sitrep <- function() {
  old_tokens <- find_old_tokens()
  tools_tokens <- find_tools_tokens()
  all_tokens_files <- c(old_tokens, tools_tokens)
  if (is.null(all_tokens_files)) {
    inform("No tokens were found! See ?auth_as for more details.")
    return(NULL)
  }
  change <- FALSE
  change_old <- FALSE
  change_rappdirs <- FALSE
  change_tools <- FALSE

  if (length(old_tokens) != 0) {
    inform(paste0("Tokens from rtweet version < 1.0.0 found on ",
           unique(dirname(old_tokens)), ":"))

    change_old <- auth_check(read_tokens(old_tokens))
  }

  if (length(tools_tokens) != 0) {
    inform(paste0("Tokens found on ", unique(dirname(tools_tokens)), ":"))
    change_tools <- auth_check(read_tokens(tools_tokens))
  }

  if (change_old || change_rappdirs || change_tools) {
    change <- TRUE
  }
  if (change_old) {
    inform(paste0("All tokens should be moved to ", auth_path()))
  }

  invisible(change)
}

find_old_tokens <- function() {
  twitter_pat <- Sys.getenv("TWITTER_PAT")

  home_path <- normalizePath(file.path("~"), mustWork = TRUE)

  many_paths <- c(twitter_pat, home_path, dirname(twitter_pat))
  old_tokens <- lapply(many_paths, list.files, pattern = ".rtweet_token.*rds",
                       full.names = TRUE, all.files = TRUE)
  unique(unlist(old_tokens, TRUE, FALSE))
}

find_tools_tokens <- function() {
  list.files(auth_path(), pattern = "*.rds",
             all.files = TRUE, full.names = TRUE)
}

bearer_auth <- function(bearer) {
  oauth2 <- vapply(bearer, has_name_, name = "access_token", logical(1L))

  tok <- c(vapply(bearer[!oauth2], function(x){x$token}, character(1L)),
           vapply(bearer[oauth2], function(x){x$access_token}, character(1L)))
  tok <- as.factor(tok)
  levels(tok) <- LETTERS[seq_along(unique(tok))]
  df <- data.frame(token = tok)
  rownames(df) <- names(bearer)
  df
}

token_auth <- function(tokens) {
  tokens <- lapply(tokens, raw_auth)
  n <- length(tokens)
  df <- data.frame(app = character(n),
                   user_id = character(n),
                   key = character(n))
  df <- as.data.frame(t(list2DF(tokens)))
  rownames(df) <- names(tokens)
  if (ncol(df) < 3L) {
    x <- seq_len(ncol(df))
  } else {
    x <- 1L:3L
  }
  colnames(df) <- c("app", "user_id", "key")[x]
  uk <- unique(df$key)
  length_levels <- length(uk) - sum(any(uk == ""))
  df$key <- factor(df$key, labels = LETTERS[seq_len(length_levels)], exclude = "")
  df
}

#' @importFrom methods is
type_auth <- function(tokens) {

  class_tokens <- vapply(tokens, function(x){is(x)[1]}, character(1L))
  class_tokens2 <- character(length(class_tokens))
  class_tokens2[class_tokens %in% c("Token1.0", "TwitterToken1.0")] <- "token"
  class_tokens2[class_tokens == "rtweet_bearer"] <- "bearer"
  class_tokens2[class_tokens == "httr2_token"] <- "httr2_token"
  if (any(!nzchar(class_tokens2))) {
    warn("Detected some file which doesn't seems created by rtweet.", parent = current_call())
  }
  class_tokens2
}

move_tokens <- function(tokens, folder) {
  file_names <- basename(tokens)
  # Replace old tokens names to an easy to read name and findable by auth_as
  file_names <- gsub("\\.([0-9]*)rds", "\\1.rds",
                     file_names, ignore.case = TRUE)
  file_names <- gsub("^\\.", "", file_names)
  new_names <- file.path(folder, file_names)
  moving_files <- tokens != new_names
  file_existed <- file.exists(new_names[moving_files])
  file.rename(from = tokens[moving_files][!file_existed],
              to = new_names[moving_files][!file_existed])
  if (any(moving_files)) {
    inform(c("Moving and renaming:",
             paste0("from ", tokens[moving_files][!file_existed], " to ",
                    new_names[moving_files][!file_existed])))
  }
  if (any(file_existed)) {
    inform(paste0("Auth ", tokens[moving_files][file_existed], " not moved",
                  " but should be on ", folder))
  }
  for (p in unique(dirname(tokens[moving_files]))) {
    if (length(list.files(p, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(p, recursive = TRUE)
    }
  }
  tokens[moving_files][!file_existed] <- new_names[moving_files][!file_existed]
  tokens
}

raw_auth <- function(auth) {
  c("app" = auth$app$appname, "user_id" = auth$credentials$user_id, key = auth$app$key)
}


handle_bearer <- function(bearers) {
  action_bearer <- FALSE
  bearers_f <- bearer_auth(bearers)
  rownames(bearers_f) <- basename(names(bearers))
  if (anyDuplicated(bearers$token) != 0) {
    inform("Multiple authentications with the same token found")
    action_bearer <- TRUE
  }

  if (action_bearer) {
    inform("Choose which is the best path of action for the bearer tokens:")
  }
  rownames(bearers_f) <- tools::file_path_sans_ext(rownames(bearers_f))
  print(bearers_f)
  action_bearer
}


handle_token <- function(tokens) {
  action_tokens <- FALSE
  token <- token_auth(tokens)
  rownames(token) <- basename(rownames(token))

  if (any(is.na(token$key))) {
    action_tokens <- TRUE
    inform("Empty tokens were found.")
  }
  if (anyDuplicated(token$key)) {
    action_tokens <- TRUE
    inform("Multiple authentications with the same key found!")
  }

  if (anyDuplicated(token$app) != 0) {
    action_tokens <- TRUE
    inform("Multiple authentications with the same app found!")
  }
  if (action_tokens) {
    inform("Choose which is the best path of action for the tokens:")
  }
  rownames(token) <- tools::file_path_sans_ext(rownames(token))
  print(token)
  action_tokens
}

auth_check <- function(tokens) {
  type_auth <- type_auth(tokens)

  action <- FALSE
  bearer_action <- FALSE
  token_action <- FALSE

  if (any(type_auth == "bearer")){
    bearer_action <- handle_bearer(tokens[type_auth == "bearer"])
  }
  if (any(type_auth == "token")){
    token_action <- handle_token(tokens[type_auth == "token"])
  }
  if (bearer_action || token_action){
    action <- TRUE
  }
  action
}

#' Help managing rtweet tokens
#'
#' Moves old tokens to new path, if keys are on two tokens only one of them is kept.
#' At the end it runs auth_sitrep to show if any other action is needed.
#' @return TRUE if manual actions are needed from the user, FALSE otherwise.
#' @seealso [auth_sitrep()]
#' @examples
#' auth_helper
#' @noRd
auth_helper <- function() {

  old_tokens <- find_old_tokens()
  tools_tokens <- find_tools_tokens()
  all_tokens_files <- c(old_tokens, tools_tokens)

  final_path <- auth_path()
  all_tokens_files <- move_tokens(all_tokens_files, final_path)

  tokens <- read_tokens(all_tokens_files)
  type_auth <- type_auth(tokens)

  if (any(type_auth == "bearer")) {
    bearer_summary <- bearer_auth(tokens[type_auth == "bearer"])
    # Delete any duplicated bearer
    unlink(rownames(bearer_summary)[duplicated(bearer_summary)])
  }
  if (any(type_auth == "token")) {
    token_summary <- token_auth(tokens[type_auth == "token"])

    # Delete any "token" without key
    unlink(rownames(token_summary)[is.na(token_summary$key)])
    # Delete any "token" without user_id
    unlink(rownames(token_summary)[is.na(token_summary$user_id)])

    if (anyDuplicated(token_summary$key) != 0) {
      for (key in token_summary$key) {
        unlink(rownames(token_summary)[token_summary$key == key][-1])
      }
    }
  }
  auth_sitrep()
}

read_tokens <- function(tokens_files) {
  all_tokens <- lapply(tokens_files, readRDS)
  names(all_tokens) <- tokens_files
  all_tokens
}

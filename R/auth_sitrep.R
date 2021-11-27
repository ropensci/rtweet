#' Twitter Tokens sitrep
#' 
#' Get a situation report of your current tokens; useful for upgrading from 
#' rtweet 0.7.0 to 1.0.0 and diagnosing problems with tokens. Called for its side effects
#' 
#' Searches for old tokens on the user folder, if duplicate tokens are found 
#' they are deleted. Tokens are then moved to the new location. 
#' @return Invisibly, the path of the tokens
#' @export
#' @examples
#' auth_sitrep()
auth_sitrep <- function() {
  all_tokens_files <- c(find_old_tokens(), 
                        find_rappdirs_tokens(), 
                        find_tools_tokens())
  path <- tools::R_user_dir("rtweet", "config")
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
  
  if (is.null(all_tokens_files)) {
    inform("No tokens were found! See ?auth_as for more details.")
    return(NULL)
  }
  
  type_auth <- type_auth(all_tokens_files)
  all_tokens <- lapply(all_tokens_files, readRDS)
  names(all_tokens) <- all_tokens_files
  
  if (any(type_auth == "bearer")){
    handle_bearer(all_tokens[type_auth == "bearer"], path)
  }
  if (any(type_auth == "token")){
    handle_token(all_tokens[type_auth == "token"], path)
  }
  invisible(list.files(path, pattern = "*.rds", all.files = TRUE, 
                       full.names = TRUE))
}

find_old_tokens <- function() {
  twitter_pat <- Sys.getenv("TWITTER_PAT")
  home_path <- normalizePath(file.path("~"), mustWork = TRUE)
  
  many_paths <- c(twitter_pat, home_path)
  old_tokens <- lapply(many_paths, list.files, pattern = ".rtweet_token.*rds", 
                       full.names = TRUE, all.files = TRUE)
  unlist(old_tokens, TRUE, FALSE)
}

find_rappdirs_tokens <- function() {
  if (requireNamespace("rappdirs", quietly = TRUE)) {
    rappdirs_path <- getOption("rtweet:::config_dir", 
                               rappdirs::user_config_dir("rtweet"))
    rappdirs_tokens <- list.files(rappdirs_path, pattern = "*.rds", 
                                  all.files = TRUE, full.names = TRUE)
  } else {
    rappdirs_tokens <- NULL
  }
  rappdirs_tokens
}

find_tools_tokens <- function() {
  path <- tools::R_user_dir("rtweet", "config")
  final_path <- getOption("rtweet:::config_dir", path)
  if (dir.exists(final_path)) {
    new_tokens <- list.files(final_path, pattern = "*.rds", 
                             all.files = TRUE, full.names = TRUE)
  } else {
    new_tokens <- NULL
  }
}

bearer_auth <- function(bearer) {
  tok <- vapply(bearer, function(x){x$token}, character(1L))
  tok <- as.factor(tok)
  levels(tok) <- LETTERS[length(unique(tok))]
  df <- data.frame(token = tok)
  rownames(df) <- bearer
}

token_auth <- function(tokens) {
  tokens <- lapply(tokens, raw_auth)
  n <- length(tokens)
  df <- data.frame(app = character(n), 
                   user_id = character(n), 
                   key = character(n))
  for (i in seq_along(tokens)) {
    token <- tokens[[i]]
    df[i, names(token)] <- token
  }
  rownames(df) <- names(tokens)
  uk <- unique(df$key)
  length_levels <- length(uk) - sum(any(uk == ""))
  df$key <- factor(df$key, labels = LETTERS[seq_len(length_levels)], exclude = "")
  df
}

#' @importFrom methods is
type_auth <- function(old_tokens_files) {
  old_tokens <- lapply(old_tokens_files, readRDS)
  names(old_tokens) <- old_tokens_files
  class_tokens <- sapply(old_tokens, is)
  class_tokens <- ifelse(endsWith(class_tokens, "Token1.0"), "token", "bearer")
}

move_tokens <- function(tokens, folder) {
  file_names <- basename(tokens)
  file_names <- gsub("\\.([0-9]*)rds", "\\1.rds", file_names, ignore.case = TRUE)
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


handle_bearer <- function(bearers, path) {
  action_bearer <- FALSE
  bearers <- bearer_auth(bearers)
  mv <- move_tokens(rownames(bearers), path)
  rownames(bearers) <- basename(mv)
  if (anyDuplicated(bearers$token)) {
    inform("Multiple authentications with the same token found")
    action_bearer <- TRUE
  }
  
  if (action_bearer) {
    inform("Choose which is the best path of action for the bearer tokens:")
    print(bearers)
    inform(paste0("You can find them at ", path))
  }
  NULL
}


handle_token <- function(tokens, path) {
  action_tokens <- FALSE
  token <- token_auth(tokens)
  mv <- move_tokens(rownames(token), path)
  rownames(token) <- basename(mv)
  
  if (any(is.na(token$key))) {
    remove <- which(is.na(token$key))
    for (r in remove) {
      inform(paste0("Removing empty auth for app ", token$app[r], " from ", 
                    dirname(mv[r])))
      unlink(mv[r])
    }
    token <- token[-remove, ]
  }
  if (anyDuplicated(token$key)) {
    action_tokens <- TRUE
    inform("Multiple authentications with the same key found!")
  }
  
  if (anyDuplicated(token$app)) {
    action_tokens <- TRUE
    inform("Multiple authentications with the same app found!")
  }
  if (action_tokens) {
    inform("Choose which is the best path of action for the tokens:")
    print(token)
    inform(paste0("You can find them at ", path))
  }
  NULL
}

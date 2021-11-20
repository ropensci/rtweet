find_old_tokens <- function() {
  twitter_pat <- Sys.getenv("TWITTER_PAT")
  home_path <- normalizePath(file.path("~"), mustWork = TRUE)
  
  many_paths <- c(twitter_pat, home_path)
  old_tokens <- lapply(many_paths, list.files, pattern = ".rtweet_token.*rds", 
                       full.names = TRUE, all.files = TRUE)
  unlist(old_tokens, TRUE, FALSE)
}

clean_tokens <- function(tokens) {
  tokens <- lapply(tokens, raw_auth)
  df <- data.frame(app = character(), user_id = character(), key = character())
  for (i in seq_along(tokens)) {
    token <- tokens[[i]]
    df[i, names(token)] <- token
  }
  rownames(df) <- names(tokens)
  tokens <- t(df)
  
  
  dup_app <- duplicated(tokens["app", ])
  dup_user <- duplicated(tokens["user_id", ])
  dup_key <- duplicated(tokens["key", ])
  
  duplicate_tokens <- dup_app & dup_key & dup_user
  names(duplicate_tokens) <- colnames(tokens)
  if (any(duplicate_tokens)) {
    inform(paste0("Removing duplicate token: ", 
                  names(duplicate_tokens)[duplicate_tokens]))
    unlink(names(duplicate_tokens)[duplicate_tokens])
  }
  same_user_app <- dup_app & dup_user & !duplicate_tokens
  names(same_user_app) <- colnames(tokens)
  if (any(same_user_app)) {
    message <- paste0("Found authentications for the same app: ", 
                      paste(names(same_user_app)[same_user_app], 
                            collapse = " and\n\t"))
    inform(message)
    inform(c("Check these tokens and keep just one.",
           "Recommended via auth_as() and auth_get() to inspect them."))
  }
  colnames(tokens)[!duplicate_tokens]
}

clean_bearer <- function(bearer) {
  tokens <- vapply(bearer, function(x){x$token}, character(1L))
  dup_tokens <- duplicated(tokens)
  if (any(dup_tokens)) {
    inform("Found bearer authentications duplicated. Keeping just one.")
    unlink(names(tokens)[dup_tokens])
  }
  return(names(tokens)[!dup_tokens])
}

#' @importFrom methods is
clean <- function(old_tokens_files) {
  old_tokens <- lapply(old_tokens_files, readRDS)
  names(old_tokens) <- old_tokens_files
  class_tokens <- sapply(old_tokens, is)
  class_tokens <- ifelse(endsWith(class_tokens, "Token1.0"), "token", "bearer")
  
  c(clean_bearer(old_tokens[class_tokens != "token"]),
    clean_tokens(old_tokens[class_tokens == "token"]))
}

#' Twitter Tokens sitrep
#' 
#' Get a situation report of your current tokens; useful for diagnosing 
#' problems and when upgrading from rtweet 0.7.0 to 1.0.0 .
#' 
#' Searches for old tokens on the user folder, if duplicate tokens are found 
#' they are deleted. Tokens are then moved to the new location. 
#' @return Invisible the path of the tokens
#' @export
#' @examples
#' auth_sitrep()
auth_sitrep <- function() {
  old_tokens_files <- find_old_tokens()
  
  if (requireNamespace("rappdirs", quietly = TRUE) && getRversion() < "4.0.0") {
    rappdirs_path <- getOption("rtweet:::config_dir", rappdirs::user_config_dir("rtweet"))
    rappdirs_tokens <- list.files(rappdirs_path, pattern = "*.rds", all.files = TRUE, full.names = TRUE)
  } else {
    rappdirs_tokens <- NULL
  }
  
  if (getRversion() >= "4.0.0") {
    final_path <- getOption("rtweet:::config_dir", tools::R_user_dir("rtweet", "config"))
    if (dir.exists(final_path)) {
      new_tokens <- list.files(final_path, pattern = "*.rds", 
                               all.files = TRUE, full.names = TRUE)
    } else {
      dir.create("/home/lluis/.config/R/rtweet", showWarnings = FALSE, 
                 recursive = TRUE)
      new_tokens <- NULL
    }
  } else {
    new_tokens <- NULL
    final_path <- rappdirs_path
  }
  
  all_tokens <- c(old_tokens_files, rappdirs_tokens, new_tokens)
  
  if (is.null(all_tokens)) {
    inform("No tokens where found! See auth_as help file.")
    return(NULL)
  }
  cleaned_tokens <- clean(all_tokens)
  mv <- move_tokens(cleaned_tokens, final_path)
  if (length(mv) == 0) {
    inform("Tokens found but no action needed!")
    NULL 
  } else {
    invisible(mv)
  }
}


move_tokens <- function(tokens, folder) {
  file_names <- basename(tokens)
  file_names <- gsub("\\.([0-9]*)rds", "\\1.rds", file_names, ignore.case = TRUE)
  file_names <- gsub("^\\.", "", file_names)
  new_names <- file.path(folder, file_names)
  moving_files <- tokens != new_names
  file.path()
  file.rename(from = tokens[moving_files], to = new_names[moving_files])
  if (any(moving_files)) {
    inform(c("Moving and renaming tokens:", 
             paste0(tokens[moving_files], " to ", new_names[moving_files])))
  }
  tokens[moving_files] <- new_names[moving_files]
  tokens
}

raw_auth <- function(auth) {
  c("app" = auth$app$appname, "user_id" = auth$credentials$user_id, key = auth$app$key)
}

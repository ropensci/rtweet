#' Open link to tweet, user or list in the default browser
#'
#' @param tw required The rtweet-object to open
#' @param mode optional Either 't', 'l' or 'u' for tweet, list or user-objects
#' @examples
#'
#' \dontrun{
#'
#' open_in_browser(tw, 'u')
#'
#' }
#' @export

open_in_browser <- function(tw, mode = "t") {
  rowNum <- nrow(tw)
  if (rowNum <= 10) {
    if (mode == "u") {
      for (i in 1:rowNum) {
        browseURL(paste("https://twitter.com/", tw[i,]$screen_name, sep = ""))
      }
    } else if (mode == "l") {
      for (i in 1:rowNum) {
        browseURL(paste("https://twitter.com/", tw[i,]$uri, sep = ""))
      }
    } else {
      for (i in 1:rowNum) {
        browseURL(paste("https://twitter.com/", tw[i,]$screen_name, "/status/", tw[i,]$status_id, sep = ""))
      }
    }
  } else if (grepl(readline(paste("You are about to open", rowNum, "links. Continue? (y/n)    ", sep = " ")), 'y')) {
    if (mode == "u") {
      for (i in 1:rowNum) {
        browseURL(paste("https://twitter.com/", tw[i,]$screen_name, sep = ""))
      }
    } else if (mode == "l") {
      for (i in 1:rowNum) {
        browseURL(paste("https://twitter.com/", tw[i,]$uri, sep = ""))
      }
    } else {
      for (i in 1:rowNum) {
        browseURL(paste("https://twitter.com/", tw[i,]$screen_name, "/status/", tw[i,]$status_id, sep = ""))
      }
    }
  }
  
}
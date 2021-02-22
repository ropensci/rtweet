#' @title Create a Tweet Embed 
#' @description Twitter API GET call to retieve the tweet in embedded form
#' @param screen_name character, screen name of the user
#' @param status_id character, status id
#' @param ... parameters to pass to the GET call
#' @return character
#' @details Arguments to pass to the API call can be found \href{https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-oembed}{here}
#' @examples 
#' name   <- 'kearneymw'
#' status <- '1087047171306856451'
#' 
#' tweet_embed(screen_name = name, status_id = status)
#' 
#' tweet_embed(
#'  screen_name = name,
#'  status_id = status,
#'  hide_thread = TRUE, 
#'  hide_media = FALSE, 
#'  align = 'center'
#' )
#' 
#' @seealso 
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname tweet_embed
#' @export 
#' @importFrom httr GET content
tweet_embed <- function(screen_name,status_id,...){
  
  stem <- 'https://publish.twitter.com/oembed'
  
  l <- list(...)
  l$url <- sprintf('https://twitter.com/%s/status/%s',screen_name,status_id)
  lpaste <- paste(names(l),as.character(l)%>%tolower(),sep='=',collapse = '&')
  
  URI <- paste(stem,lpaste,sep = '?')
  ret <- URI%>%httr::GET()%>%httr::content()
  ret$html
  
}


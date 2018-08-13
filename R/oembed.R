#' Embed tweets
#' 
#' Embed tweets in Shiny or R markdown documents.
#' 
#' @param screen_name,status_id Screename and status if of tweet to emebed.
#' @param ... Any argument to pass to API call, 
#' see \href{https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-oembed.html}{official documentation} 
#' for the complete list.
#' 
#' @examples 
#' \dontrun{
#' ## search for 1000 tweets mentioning Hillary Clinton
#' hrc <- search_tweets(q = "hillaryclinton", n = 1000)
#' 
#' ## sample tweet at random
#' rand <- hrc[sample(nrow(hrc), 1),]
#' 
#' get_embed(rand$screen_name, rand$status_id)
#' }
#' 
#' @details In R markdown use \code{\link{embed_tweet}} in your code chunk set \code{asis = TRUE}. 
#' In Shiny user \code{\link[shiny]{htmlOutput}}.
#' 
#' @importFrom utils URLencode
#' 
#' @export
#' @rdname oembed
get_embed <- function(screen_name, status_id, ...){
  
  if(missing(screen_name) || missing(status_id))
    stop("missing screen_name or status_id.")
  
  url <- URLencode(
    paste0(
      "https://twitter.com/", screen_name[1], "/status/", status_id[1]
    )
  )
  
  parsed <- httr::parse_url("https://publish.twitter.com/oembed")
  parsed$query <- list(
    url = url,
    ...
  )
  built <- httr::build_url(parsed)
  
  response <- httr::GET(built)
  httr::warn_for_status(response)
  httr::content(response)
}

#' @export
#' @rdname oembed
embed_tweet <- function(screen_name, status_id, ...){
  
  embed <- get_embed(screen_name, status_id, ...)
  
  return(embed[["html"]])
}
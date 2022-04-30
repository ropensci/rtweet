#' Create a Tweet Embed 
#' 
#' Twitter API GET call to retieve the tweet in embedded form.
#' 
#' @param screen_name character, screen name of the user
#' @param status_id character, status id
#' @param ... parameters to pass to the GET call. See
#'   <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/get-statuses-oembed>
#'   for details.
#' @return character
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
#' @seealso [httr::GET()],[httr::content()]
#' @rdname tweet_embed
#' @export 
tweet_embed <- function(screen_name,status_id,...){
  # https://developer.twitter.com/en/docs/twitter-for-websites/timelines/guides/oembed-api
  
  params <- list(
    url = sprintf('https://twitter.com/%s/status/%s', screen_name, status_id),
    ...
  )
  resp <- httr::GET("https://publish.twitter.com/oembed", query = params)
  check_status(resp, "oembed")
  
  json <- from_js(resp)
  json$html
}


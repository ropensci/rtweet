#' @title Collect statuses contained in a thread
#' @description Return all statuses that are part of a thread
#' @param tw \code{\link[rtweet]{lookup_statuses}} output containing
#'  at least the last status in the thread
#' @return \code{\link[rtweet]{lookup_statuses}} tibble
#' @details This function traverses up the thread until it gets to the root status
#' @examples 
#' tw <- lookup_statuses('1084143184664510470')
#' tw_thread <- tw%>%tweet_threading()
#' tw_thread
#' @seealso 
#'  \code{\link[rtweet]{lookup_statuses}}
#' @rdname tweet_threading
#' @export 
tweet_threading <- function(tw){
  
  last_found <- FALSE
  
  while(!last_found){
    
    nr         <- nrow(tw)
    last_found <- is.na(tw$reply_to_status_id[nr])
    tw_tail    <- lookup_statuses(tw$reply_to_status_id[nr])
    tw         <- rbind(tw,tw_tail)
    
  }
  
  tw
}
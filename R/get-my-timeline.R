#' Get _your_ timeline
#'
#' Returns a collection of the most recent Tweets and Retweets posted by the
#' authenticating user and the users they follow. The home timeline is central to how
#' most users interact with the Twitter service.\cr
#' \cr
#' The _authenticating user_ is determined from the `token`.
#'
#' @md
#' @param n Number of tweets to return per timeline. Defaults to 100.
#'   Must be of length 1 or equal to length of user.
#' @param max_id Character, status_id from which returned tweets
#'   should be older than.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list object. By default, \code{parse =
#'   TRUE} saves users from the time (and frustrations) associated
#'   with disentangling the Twitter API return objects.
#' @param check Logical indicating whether to remove check available
#'   rate limit. Ensures the request does not exceed the maximum
#'   remaining number of calls.  Defaults to TRUE.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param ... Further arguments passed on as parameters in API query.
#' @return A tbl data frame of tweets data with users data attribute.
#' @seealso
#'   \url{https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-home_timeline}
#' @examples
#'
#' \dontrun{
#'
#' tweets_from_me_and_the_ppl_i_follow <- get_my_timeline(n = 3200)
#'
#' }
#' @family tweets
#' @export
get_my_timeline <- function(n = 100,
                            max_id = NULL,
                            parse = TRUE,
                            check = TRUE,
                            token = NULL,
                            ...) {

  tok <- check_token(token)

  args <- list(
    user = tok$credentials$screen_name,
    n = n,
    home = TRUE,
    max_id = max_id,
    parse = parse,
    check = check,
    token = token
  )
  do.call("get_timeline_", args)
}

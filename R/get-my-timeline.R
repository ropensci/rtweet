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
#' @param max_id Character, returns results with an ID less than (that is,
#'   older than) or equal to `max_id`.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list object. By default, \code{parse =
#'   TRUE} saves users from the time (and frustrations) associated
#'   with disentangling the Twitter API return objects.
#' @param check Logical indicating whether to remove check available
#'   rate limit. Ensures the request does not exceed the maximum
#'   remaining number of calls.  Defaults to TRUE.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
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

  user <- home_user()

  args <- list(
    user = user,
    n = n,
    home = TRUE,
    max_id = max_id,
    parse = parse,
    check = check,
    token = token
  )
  do.call("get_timeline_", args)
}

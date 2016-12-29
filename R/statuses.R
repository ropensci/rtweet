#' lookup_tweets
#'
#' @description Returns Twitter user data_frame object for
#'   specified user_ids or screen_names.
#'
#' @param statuses User id or screen name of target user.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable @describeIn tokens.
#' @param parse Logical, indicating whether or not to parse
#'   return object into data frame(s).
#' @param usr Logical indicating whether to return users data frame.
#'   Defaults to true.
#' @param clean_tweets logical indicating whether to remove non-ASCII
#'   characters in text of tweets. defaults to FALSE.
#' @param as_double logical indicating whether to handle ID variables
#'   as double (numeric) class. By default, this is set to FALSE, meaning
#'   ID variables are treated as character vectors. Setting this to
#'   TRUE can provide performance (speed and memory) boost but can also
#'   lead to issues when printing and saving, depending on the format.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # lookup tweets data via status_id vector
#' statuses <- c("567053242429734913", "266031293945503744",
#'   "440322224407314432")
#' statuses <- lookup_statuses(statuses)
#' statuses
#'
#' # view users data for these statuses via tweets_data()
#' users_data(statuses)
#' }
#'
#' @return json response object (max is 18000 per token)
#' @family tweets
#' @export
lookup_statuses <- function(statuses,
                            token = NULL,
                            parse = TRUE,
                            usr = TRUE,
                            clean_tweets = FALSE,
                            as_double = FALSE) {

    if (is.list(statuses)) {
        statuses <- unlist(statuses)
    }

    if (length(statuses) > 18000) {
        statuses <- statuses[1:18000]
    }

    n.times <- ceiling(length(statuses) / 100)

    from <- 1

    twt <- vector("list", n.times)

    for (i in seq_len(n.times)) {
        to <- from + 99

        if (to > length(statuses)) {
            to <- length(statuses)
        }

        twt[[i]] <- .status_lookup(
            statuses[from:to],
            token, parse = parse)

        from <- to + 1

        if (from > length(statuses)) break
    }

    if (parse) {
        twt <- parse.piper(twt, usr = usr)
    }

    twt
}

.status_lookup <- function(statuses, token = NULL, parse) {

    query <- "statuses/lookup"

    if (is.list(statuses)) {
        statuses <- unlist(statuses)
    }

    stopifnot(is.atomic(statuses))

    if (length(statuses) > 100) {
        statuses <- statuses[1:100]
    }

    params <- list(id = paste(statuses, collapse = ","))

    url <- make_url(
        query = query,
        param = params)

    token <- check_token(token, query = "statuses/lookup")

    resp <- TWIT(get = TRUE, url, token)

    from_js(resp)
}

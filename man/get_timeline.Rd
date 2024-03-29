% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeline.R
\name{get_timeline}
\alias{get_timeline}
\alias{get_my_timeline}
\alias{get_timelines}
\title{Get one or more user timelines}
\usage{
get_timeline(
  user = NULL,
  n = 100,
  since_id = NULL,
  max_id = NULL,
  home = FALSE,
  parse = TRUE,
  check = TRUE,
  retryonratelimit = NULL,
  verbose = TRUE,
  token = NULL,
  ...
)

get_my_timeline(
  n = 100,
  since_id = NULL,
  max_id = NULL,
  parse = TRUE,
  check = TRUE,
  retryonratelimit = NULL,
  verbose = TRUE,
  token = NULL,
  ...
)
}
\arguments{
\item{user}{Character vector of screen names or user ids.
See \code{\link[=as_screenname]{as_screenname()}} for more details.}

\item{n}{Desired number of results to return. Results are downloaded
in pages when \code{n} is large; the default value will download a single
page. Set \code{n = Inf} to download as many results as possible.

The Twitter API rate limits the number of requests you can perform
in each 15 minute period. The easiest way to download more than that is
to use \code{retryonratelimit = TRUE}.

You are not guaranteed to get exactly \code{n} results back. You will get
fewer results when tweets have been deleted or if you hit a rate limit.
You will get more results if you ask for a number of tweets that's not
a multiple of page size, e.g. if you request \code{n = 150} and the page
size is 200, you'll get 200 results back.}

\item{since_id}{Supply a vector of ids or a data frame of previous results to
find tweets \strong{newer} than \code{since_id}.}

\item{max_id}{Supply a vector of ids or a data frame of previous results to
find tweets \strong{older} than \code{max_id}.}

\item{home}{Logical, indicating whether to return a "user" timeline
(the default, what a user has tweeted/retweeted) or a "home" timeline
(what the user would see if they logged into twitter).}

\item{parse}{If \code{TRUE}, the default, returns a tidy data frame. Use \code{FALSE}
to return the "raw" list corresponding to the JSON returned from the
Twitter API.}

\item{check}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}}

\item{retryonratelimit}{If \code{TRUE}, and a rate limit is exhausted, will wait
until it refreshes. Most Twitter rate limits refresh every 15 minutes.
If \code{FALSE}, and the rate limit is exceeded, the function will terminate
early with a warning; you'll still get back all results received up to
that point. The default value, \code{NULL}, consults the option
\code{rtweet.retryonratelimit} so that you can globally set it to \code{TRUE},
if desired.

If you expect a query to take hours or days to perform, you should not
rely solely on \code{retryonratelimit} because it does not handle other common
failure modes like temporarily losing your internet connection.}

\item{verbose}{Show progress bars and other messages indicating current
progress?}

\item{token}{Use this to override authentication for
a single API call. In many cases you are better off changing the
default for all calls. See \code{\link[=auth_as]{auth_as()}} for details.}

\item{...}{Further arguments passed on as parameters in API query.}
}
\value{
A tbl data frame of tweets data with users data attribute.
}
\description{
\code{get_timeline()} returns the timeline of any Twitter user (i.e. what they
have tweeted). \code{get_my_timeline()} returns the home timeline for the
authenticated user (i.e. the tweets you see when you log into Twitter).
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
At most up to 3,200 of a user's most recent Tweets can be retrieved.
}
\references{
\url{https://developer.twitter.com/en/docs/twitter-api/v1/tweets/timelines/overview}
}
\seealso{
\code{\link[=user_timeline]{user_timeline()}}, \code{\link{rtweet-deprecated}}

Other tweets: 
\code{\link{get_favorites}()},
\code{\link{get_mentions}()},
\code{\link{lists_statuses}()},
\code{\link{lookup_tweets}()},
\code{\link{search_tweets}()}
}
\concept{tweets}

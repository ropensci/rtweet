% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_destroy.R
\name{post_destroy}
\alias{post_destroy}
\title{Delete status of user's Twitter account \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}}
\usage{
post_destroy(destroy_id, token = NULL)
}
\arguments{
\item{destroy_id}{To delete a status, supply the single status ID here. If a
character string is supplied, overriding the default (NULL), then a destroy
request is made (and the status text and media attachments) are irrelevant.}

\item{token}{Use this to override authentication for
a single API call. In many cases you are better off changing the
default for all calls. See \code{\link[=auth_as]{auth_as()}} for details.}
}
\description{
Deletes a status of user's profile.
}
\references{
\url{https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/post-statuses-destroy-id}
}
\seealso{
\code{\link[=tweet_delete]{tweet_delete()}} \code{\link{rtweet-deprecated}}
}

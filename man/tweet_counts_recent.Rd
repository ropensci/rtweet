% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tweet_counts.R
\name{tweet_counts_recent}
\alias{tweet_counts_recent}
\alias{tweet_counts_all}
\title{Count tweets}
\usage{
tweet_counts_recent(query, ..., token = NULL, parse = TRUE, verbose = FALSE)

tweet_counts_all(query, ..., token = NULL, parse = TRUE, verbose = FALSE)
}
\arguments{
\item{query}{One query for matching Tweets.}

\item{...}{Other arguments passed to the API.}

\item{token}{These endpoints only accept a bearer token (can be created via
\code{\link[=rtweet_app]{rtweet_app()}}). In most cases you
are better of changing the default for all calls via \code{\link[=auth_as]{auth_as()}}.}

\item{parse}{If \code{TRUE}, the default, returns a tidy data frame. Use \code{FALSE}
to return the "raw" list corresponding to the JSON returned from the
Twitter API.}

\item{verbose}{A logical value to provide more information about the
paginated queries (if any) and to store the data of each page.}
}
\value{
The number of tweets for a given granularity
}
\description{
Count tweets
}
\examples{
if (FALSE) {
  tcr <- tweet_counts_recent(query = "#rtweet", parse = FALSE)
  tca <- tweet_counts_all(query = "#rtweet", parse = FALSE)
}
}
\references{
\url{https://developer.twitter.com/en/docs/twitter-api/tweets/counts/api-reference/get-tweets-counts-all}

\url{https://developer.twitter.com/en/docs/twitter-api/tweets/counts/api-reference/get-tweets-counts-recent}
}

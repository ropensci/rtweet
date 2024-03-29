% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stream.R
\name{stream_tweets}
\alias{stream_tweets}
\title{Collect a live stream of Twitter data}
\usage{
stream_tweets(
  q = "",
  timeout = 30,
  parse = TRUE,
  token = NULL,
  file_name = NULL,
  verbose = TRUE,
  append = TRUE,
  ...
)
}
\arguments{
\item{q}{Query used to select and customize streaming collection
method.  There are four possible methods:
\enumerate{
\item The default, \code{q = ""}, returns a small random sample of all
publicly available Twitter statuses.
\item To filter by keyword, provide a comma separated character string with
the desired phrase(s) and keyword(s).
\item Track users by providing a comma separated list of user IDs or
screen names.
\item Use four latitude/longitude bounding box points to stream by geo
location. This must be provided via a vector of length 4, e.g.,
\code{c(-125, 26, -65, 49)}.
}}

\item{timeout}{Integer specifying number of seconds to stream tweets for.
Stream indefinitely with \code{timeout = Inf}.

The stream can be interrupted at any time, and \code{file_name} will still be
valid file.}

\item{parse}{Use \code{FALSE} to opt-out of parsing the tweets.}

\item{token}{Use this to override authentication for
a single API call. In many cases you are better off changing the
default for all calls. See \code{\link[=auth_as]{auth_as()}} for details.}

\item{file_name}{Character with name of file. If not specified,
will write to a temporary file \code{stream_tweets*.json}.}

\item{verbose}{If \code{TRUE}, display a progress bar.}

\item{append}{If \code{TRUE}, will append to the end of \code{file_name}; if
\code{FALSE}, will overwrite.}

\item{...}{Other arguments passed in to query parameters.}
}
\value{
A tibble with one row per tweet
}
\description{
Streams public statuses to a file via one of the following four methods:
\enumerate{
\item Sampling a small random sample of all publicly available tweets
\item Filtering via a search-like query (up to 400 keywords)
\item Tracking via vector of user ids (up to 5000 user_ids)
\item Location via geo coordinates (1-360 degree location boxes)
}

Learn more in \code{vignette("stream", package = "rtweet")}
}
\details{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\references{
They were removed from the website.

The webpages describing how it used to work were removed.
}
\seealso{
\code{\link[=filtered_stream]{filtered_stream()}}, \code{\link{rtweet-deprecated}}
}

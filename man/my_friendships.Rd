% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/friends.R
\name{my_friendships}
\alias{my_friendships}
\title{Lookup friendship information between users.}
\usage{
my_friendships(user, parse = FALSE, token = NULL)
}
\arguments{
\item{user}{Character vector of screen names or user ids.
See \code{\link[=as_screenname]{as_screenname()}} for more details.}

\item{parse}{If \code{TRUE}, the default, returns a tidy data frame. Use \code{FALSE}
to return the "raw" list corresponding to the JSON returned from the
Twitter API.}

\item{token}{Use this to override authentication for
a single API call. In many cases you are better off changing the
default for all calls. See \code{\link[=auth_as]{auth_as()}} for details.}
}
\description{
Gets information on friendship between authenticated user and up
to 100 other users.
}
\references{
\url{https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-friendships-lookup}
}
\seealso{
Other friends: 
\code{\link{lookup_friendships}()}
}
\concept{friends}

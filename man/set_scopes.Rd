% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scopes.R
\name{set_scopes}
\alias{set_scopes}
\title{Scopes of the OAuth2 token}
\usage{
set_scopes(read = TRUE, write = TRUE, tweet_moderate = TRUE, regenerate = TRUE)
}
\arguments{
\item{read}{Allow to read.}

\item{write}{Allow to write/manage?}

\item{tweet_moderate}{Allow to hide or show replies to your Tweets.}

\item{regenerate}{Allow to use the token for more than 2 hours.}
}
\value{
A character with all the possible scopes or those allowed.
}
\description{
Permissions given to a token of a Twitter account.
By default it allows everything.
}
\examples{
set_scopes()
}
\references{
\url{https://developer.twitter.com/en/docs/authentication/oauth-2-0/authorization-code}
}

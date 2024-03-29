% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compliance.R
\name{job_compliance}
\alias{job_compliance}
\title{Job compliance}
\usage{
job_compliance(ids, name, type, resumable = TRUE, token = NULL)
}
\arguments{
\item{ids}{Ids of tweets or users to check.}

\item{name}{Name of the job.}

\item{type}{Type of ids: either 'tweets' or 'users'.}

\item{resumable}{A logical value if the job is resumable.}

\item{token}{Bearer token used.}
}
\description{
Job compliance
}
\references{
\url{https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference}
Submit job: \url{https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/post-compliance-jobs}
Job status: \url{https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/get-compliance-jobs-id}
Jobs list: \url{https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/get-compliance-jobs}
}
\keyword{internal}

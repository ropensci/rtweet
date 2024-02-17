#' Job compliance
#'
#' @param ids Ids of tweets or users to check.
#' @param name Name of the job.
#' @param type Type of ids: either 'tweets' or 'users'.
#' @param resumable A logical value if the job is resumable.
#' @param token Bearer token used.
#'
#' @references <https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference>
#' Submit job: <https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/post-compliance-jobs>
#' Job status: <https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/get-compliance-jobs-id>
#' Jobs list: <https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/get-compliance-jobs>
#' @keywords internal
job_compliance <- function(ids, name, type, resumable = TRUE, token = NULL) {
  # To store the token at the right place: see ?httr2::oauth_cache_path
  withr::local_envvar(HTTR2_OAUTH_CACHE = auth_path())
  type <- match.arg(type, c("tweets", "users"))
  stopifnot(is_logical(resumable))
  stopifnot(is.character(name) && length(name) == 1)
  req_jobs <- endpoint_v2("compliance/jobs", 150/(60*15), c("tweets", "users"))
  resp <- httr2::req_perform(req_jobs)
}

# Check guide if resumable = FALSE requires different handling
upload_ids <- function(upload_url, ids) {

}

job_status <- function(job_id, token = NULL) {
  url <- paste0("compliance/jobs/", job_id)
  req_jobs <- endpoint_v2(url, 150/(60*15), c("tweets", "users"))
}

job_list <- function(type, status = "all", token = NULL) {
  type <- match.arg(type, c("tweets", "users"))
  status <- match.arg(type, c("created", "in_progress", "failed", "complete", "all"))
  req_jobs <- endpoint_v2("compliance/jobs",
                          150/(60*15), set_scopes())
  # Add expiration date of jobs (1 week after creation)
}

job_download <- function() {

}

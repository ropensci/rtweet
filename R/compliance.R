#' Job compliance
#'
#' @references <https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference>
#' Submit job: <https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/post-compliance-jobs>
#' Job status: <https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/get-compliance-jobs-id>
#' Jobs list: <https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/api-reference/get-compliance-jobs>
job_compliance <- function(ids, name, type, resumable = TRUE, token = NULL) {

  type <- match.arg(type, c("tweets", "users"))

  rate <- 150/(60*15)
  token <- check_token_v2(token)
  req_jobs <- endpoint_v2(token, "compliance/jobs", rate)
  resp <- req_perform(req_archive)
}

# Check guide if resumable = FALSE requires different handling
upload_ids <- function(upload_url, ids) {

}

job_status <- function(job_id, token = NULL) {
  url <- paste0("compliance/jobs/", job_id)
  rate <- 150/(60*15)
  token <- check_token_v2(token)
  req_jobs <- endpoint_v2(token, url, rate)
}

job_list <- function(type, status = "all", token = NULL) {
  type <- match.arg(type, c("tweets", "users"))
  status <- match.arg(type, c("created", "in_progress", "failed", "complete", "all"))
  url <- "compliance/jobs"
  rate <- 150/(60*15)
  token <- check_token_v2(token)
  req_jobs <- endpoint_v2(token, url, rate)
  # Add expiration date of jobs (1 week after creation)
}

job_download <- function() {

}

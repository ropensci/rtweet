#rtlines <- function(x) `class<-`(x, "rtlines")

#as_rtlines <- function(x) UseMethod("as_rtlines")

#as_rtlines.character <- function(x) {
#  statuses <- good_lines(x)
#  attr(statuses, "limit") <- parse_streamlimit(x)
#  rtlines(statuses)
#}

#print.rtlines <- function(x, n = 10L, ...) {
#  wdt <- getOption("width") - 34L
#  if (wdt < 1) {
#    wdt <- 5
#  }
#  lmt <- limits_data(x)
#  x <- as.character(x)
#  len <- length(x)
#  cat(paste0("> attr(", quote(x), ", \"limit\")"), fill = TRUE)
# print(lmt)
#  if (len > n) {
#    x <- x[seq_len(n)]
#  }
#  cat(paste("# Number of Twitter statuses:", len), fill = TRUE)
#  x <- paste0(substr(x, 1, wdt), "...")
#  NextMethod()
#}

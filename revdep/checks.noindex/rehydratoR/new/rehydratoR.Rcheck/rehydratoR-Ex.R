pkgname <- "rehydratoR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rehydratoR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("rehydratoR")
### * rehydratoR

flush(stderr()); flush(stdout())

### Name: rehydratoR
### Title: Get tweets for given statuses (tweet IDs).
### Aliases: rehydratoR

### ** Examples

## Not run: 
##D # Get Twitter api keys from https://apps.twitter.com
##D consumerKey <- ''
##D consumerSecret <- ''
##D accessToken <- ''
##D accessTokenSecret <- ''
##D 
##D # Read tweet ids
##D tweet_ids <- data.frame(read.table(tweet_ids_file, numerals = 'no.loss'))
##D 
##D # Download tweets
##D tweets <- rehydratoR(consumerKey, consumerSecret, accessToken, accessTokenSecret, tweet_ids)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

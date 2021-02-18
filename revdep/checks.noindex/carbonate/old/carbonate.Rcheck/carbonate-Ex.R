pkgname <- "carbonate"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('carbonate')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("browse")
### * browse

flush(stderr()); flush(stdout())

### Name: .browse
### Title: open $uri to in browser window
### Aliases: .browse carbon-browse

### ** Examples

x <- carbon$new('x <- 1')
if(interactive())
 x$browse()




cleanEx()
nameEx("carbonate")
### * carbonate

flush(stderr()); flush(stdout())

### Name: .carbonate
### Title: Carbonate script lines to a carbon image
### Aliases: .carbonate carbon-carbonate carbonate

### ** Examples

if(interactive()){
 x <- carbon$new('x <- 1')
 x$carbonate()
 }



cleanEx()
nameEx("rtweet")
### * rtweet

flush(stderr()); flush(stdout())

### Name: .rtweet
### Title: send tweets using carbonate outputs
### Aliases: .rtweet carbon-rtweet

### ** Examples

## Not run: 
##D x <- carbonate::carbon$new()
##D x$carbonate(code = readLines(system.file('DESCRIPTION',package='carbonate')))
##D x$carbonate(code = 'x+2')
##D 
##D # using default status value (x$tweet_status)
##D x$rtweet(system.file('figures/hex_black_small.png',package='carbonate'))
##D 
##D x$rtweet(status = 'these are two pngs',media = x$carbons,media_format='png')
##D x$rtweet(status = 'this is a gif', media = x$carbons,media_format='gif')
##D 
## End(Not run)



cleanEx()
nameEx("uri")
### * uri

flush(stderr()); flush(stdout())

### Name: .uri
### Title: URI constructor
### Aliases: .uri carbon-uri

### ** Examples

if(interactive()){

x <- carbon$new('x <- 1')

# populate from self$code
x$uri()

# enter manually
x$uri(code = 'x <- y + 3')

}



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

### rtweet: Collecting Twitter data

# Obtaining and Using Twitter API Access Tokens

This vignette covers how to obtain and use Twitter API 
access tokens for use in the `rtweet` package.

``` r
#install from CRAN
#install.packages("rtweet")

#install from github (most recent, development version)
#devtools::install_github("mkearney/rtweet")

library(rtweet)
```

## Create Twitter App

To create a Twitter app, go to [http://apps.twitter.com/app/new](http://apps.twitter.com/app/new) 
and provide a `Name`, `Description`, `Website`, and `Callback URL`.
The first three fields are up to you (notes: app name must be unique and
URL must be real--usisng your Twitter profile link works). However, for `Callback URL`, make sure to
enter the following: `http://127.0.0.1:1410`

<p align="center">
<img src="files/creating.png" alt="creating">
</p>

Once you've successfully created an app, locate 
and record your consumer, or API, keys. To do this, navigate to the
`Keys and Access Tokens` tab.

<p align="center">
<img src="files/created.png" alt="created">
</p>

The two keys of interest (consumer key and consumer 
secret key) should appear like the screen shot below.

<p align="center">
<img src="files/keys.png" alt="keys">
</p>

Following the steps outlined above, it's possible to create multiple 
Twitter apps, thereby generating multiple tokens. Twitter discourages 
abusing their API rate limits (these are like speed limits, 
regulating the amount of requests you can make within a given period 
of time). Abusing Twitter rate limits can even result in Twitter 
completely revoking your API access. Fortunately, using a small 
number of tokens seems to fall well below their threshold of concern.

## Using Tokens with `rtweet`

For this example, I created two Twitter apps to demonstrate how to 
store more than one access token. The steps are basically the same
if you only created one app.

With the access keys identified earlier, use the `create_token()` 
function and store the output as `twitter_tokens`.

``` r
twitter_tokens <- c(
  create_token(app = rtweet_tokens, 
    consumer_key = "XZgqotgOZNKlLFJqFbd8NjUtL",
    consumer_secret = "1rDnU3H3nrxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
  create_token(app = rtweet_roauth,
    consumer_key = "XZgqotgOZNKlLFJqFbd8NjUtL",
    consumer_secret = "1rDnU3H3nrxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  )
```

## Saving Tokens

Technically, you now have enough to start using `rtweet` functions--
you'd just need to set the token argument within functions to the
`twitter_tokens` object created earlier. Rather than specifying 
tokens for each function or creating new tokens for every session, 
you can store your tokens as an environment variable.

To do this, I recommend saving your `twitter_tokens` object in your 
home directory. To locate your home directory enter 
`normalizePath("~/")` into your R console (the home directory on 
my mac, for example, is "/Users/mwk/"). Use the `save()` function
to save your token object and the `file` argument to specify 
the desired location. Rather than manually typing out the file 
path using my home directory + "/twitther_tokens", I used the code
below to make things easier:

```{r, echo = TRUE, eval = FALSE}
home_directory <- normalizePath("~/")
file_name <- paste0(home_directory, "/", "twitter_tokens")
save(twitter_tokens, file = file_name)
```

## Environment Variable

To easily access your token(s) in future sessions (a best practice [recommended by Hadley](https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd)), 
create a plain text file containing the path where you saved your token object. 
You can do this in any text editor like TextEdit or Notepad. Or, 
if you're using Rstudio, (File > New File > Text File). Alternatively,
you can do this in R by using the code below (modifying TWITTER_PAT as
necessary to match the file_name you used in the previous step).

``` r
cat("TWITTER_PAT=/Users/mwk/twitter_tokens\n", file = paste0(home_directory, "/.Renviron"))
```

Restart R so it can process your environment variable (since we saved it as 
".Renviron", you likely won't see the file in your finder on startup and 
you should be able to use `rtweet` functions now and in the future without
having to deal with tokens ever again!

That's it!

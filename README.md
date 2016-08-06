# rtweet: Collecting Twitter Data

R package for collecting Twitter data via Twitter's REST and stream API's.

Key features

- Gather **tweet** data by searching `search_tweets()` past tweets or 
by streaming `stream_tweets()` live tweets. Easily return data on 
thousands of tweets at a time.

- Gather **user** data by looking up `lookup_users()` information
on specific Twitter users. Easily return data on thousands of users
at a time.

- Organized and easily translatable data formats. Functions return
data frames that are **ready** for data analysis the moment they
are received.

- Tweet data functions return not only text of tweets, but a host of 
other variables, including the number of times a tweet has been 
retweeted (`retweet_count`) and favorited (`favorite_count`).
To gauge user interactions, there are also seperate variables that
identify replies by source tweet (`in_reply_to_status_id_str`) 
and by source user ID (`in_reply_to_status_id_str`). Variables also
indicate whether the tweet quotes another tweet (`is_quote_status`), 
and, if so, the quoted tweet ID (`quoted_status_id_str`).

## Install
To get the current released version from CRAN:
```{r}
install.packages("rtweet")
library(rtweet)
```

To get the current development version from github:
```{r}
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("mkearney/rtweet")
```

## Obtaining access tokens
1. To create Twitter app(s) [and secure access to oauth tokens necessary for API queries]
visit http://apps.twitter.com/app/new
2. Enter information in `Name`, `Description`, `Website`, and `Callback URL` 
fields like example provided below. For `Callback URL` make sure to copy/paste 
the following: `http://127.0.0.1:1410`
3. Once the app is created, copy and paste `consumer key` and `consumer secret key` 
into the `create_token()` function (see demo below).

| Field           | Enter                                   |
|-----------------|-----------------------------------------|
| Name            | rtweet_app                              |
| Description     | Twitter follows and selective exposure  |
| Website         | http://twitter.com/kearneymw            |
| Callback URL    | http://127.0.0.1:1410                   |

## Using Tokens in R
Using the information obtained from `obtaining access tokens`
above, generate a token via the `create_token` function.

Modify the code below by replacing `appX_name` with name of your 
application and `xxxx...` with the appropriate alpha-numeric keys 
associated with your app (see: 'obtaining access tokens').

If this is the first time running `create_token` for an app, a 
web browser will automatically pop up. Select yes/agree to 
authorize once for each app. 

```{r, echo = TRUE, eval = FALSE}
library(rtweet)

twitter_tokens <- c(
  create_token(app = "app1_name",
    consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
    consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
  create_token(app = "app2_name",
    consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
    consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
```

You can save your token(s) object anywhere, but it's important to know
*where* you saved it. For that reason, I recommend saving your
token(s) object in your home directory. To locate your home directory
enter `normalizePath("~/")` into your R console. Save the token(s)
object like this:

```{r, echo = TRUE, eval = FALSE}
home_directory <- normalizePath("~/")

file_name <- paste0(home_directory, "/", "twitter_tokens")

save(twitter_tokens, file = file_name)
```

To create an environmental variable to access your token(s) later 
on (a best practice [recommended by Hadley](https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd)),
open a new plain text document. You can do this in any text 
editor like TextEdit or Notepad. Or, if you're using Rstudio, 
(File > New File > Text File).

In the blank plain text document, type the text below where 
`blahblahblahblahblahblah` is the path to wherever you've saved 
the token(s). On my mac, for example, it looks like this: 
`TWITTER_PAT=/Users/mwk/twitter_tokens`. Make sure the last line
of the document is left empty, otherwise R won't read the file.

```
TWITTER_PAT=blahblahblahblahblahblah

```

Save the file to your home directory, which, again, you can find 
by entering `normalizePath("~/")` in the R console.

```{r, echo = TRUE, eval = FALSE}
normalizePath("~/")
```

Restart R so it can process your environment variable on startup
and then call the `get_tokens` function.

```{r, echo = TRUE, eval = FALSE}
library(rtweet)

tokens <- get_tokens()

elect16 <- search_tweets(q = "election2016", count = 500, token = tokens[[1]])
elect16
```

Or, more conveniently, if you followed the steps above correctly, you 
don't have to load your tokens at all. The `rtweet` functions will fetch 
your tokens for you!

```{r, echo = TRUE, eval = FALSE}
library(rtweet)

elect16 <- search_tweets(q = "election2016", count = 500)
elect16
```

## Package description

More technical description: An implementation of calls designed to extract
and organize Twitter data via Twitter's REST and stream
API's. Functions formulate GET and POST requests and
convert response objects to more user friendly structures,
e.g., data frames or lists. Specific consideration is
given to functions designed to return tweets, friends,
and followers.

## Contact
Email me at mkearney@ku.edu

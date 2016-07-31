# rtweet: Collecting Twitter Data

- R package for collecting Twitter Data in R via Twitter's REST and stream API's

## Install
- to install `rtweet` run the following code in R:
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
- Using the information obtained from `obtaining access tokens`
above, generate a token via the `create_token` function.
- Modify the code below by replacing `appX_name` with name of your 
application and `xxxx...` with the appropriate alpha-numeric keys 
associated with your app (see: 'obtaining access tokens').
- If this is the first time running `create_token` for an app, a 
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

- You can save your token(s) object anywhere, but it's important to know
*where* you saved it. For that reason, I recommend saving your
token(s) object in your home directory. To locate your home directory
enter `normalizePath("~/")` into your R console. Save the token(s)
object like this:

```{r, echo = TRUE, eval = FALSE}
home_directory <- normalizePath("~/")

file_name <- paste0(home_directory, "/", "twitter_tokens")

save(twitter_tokens, file = file_name)
```

- To create an environmental variable to access your token(s) later 
on (a best practice [recommended by Hadley](https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd)),
open a new plain text document. You can do this in any text 
editor like TextEdit or Notepad. Or, if you're using Rstudio, 
(File > New File > Text File).

- In the blank plain text document, type the text below where 
`blahblahblahblahblahblah` is the path to wherever you've saved 
the token(s). On my mac, for example, it looks like this: 
`TWITTER_PAT=/Users/mwk/twitter_tokens`. Make sure the last line
of the document is left empty, otherwise R won't read the file.

```
TWITTER_PAT=blahblahblahblahblahblah

```

- Save the file to your home directory, which, again, you can find 
by entering `normalizePath("~/")` in the R console.

```{r, echo = TRUE, eval = FALSE}
normalizePath("~/")
```

- Restart R so it can process your environment variable on startup
and then call the `get_tokens` function.

```{r, echo = TRUE, eval = FALSE}
library(rtweet)

tokens <- get_tokens()

elect16 <- search_tweets(q = "election2016", count = 500, tokens[[1]])
elect16
```

- Or, more conveniently, if you followed the steps above correctly, you 
don't have to load your tokens at all. The `rtweet` functions will fetch 
your tokens for you!

```{r, echo = TRUE, eval = FALSE}
library(rtweet)

elect16 <- search_tweets(q = "election2016", count = 500)
elect16
```

## contact
- email me at mkearney@ku.edu

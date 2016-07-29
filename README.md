# rtweet

## about rtweet
- R package consisting of functions designed to interact with twitter's API

## install
- to install `rtweet` run the following code in R:
```{r}
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("mkearney/rtweet")
```

## obtaining access tokens
1. To create Twitter app(s) [and secure access to oauth tokens necessary for API queries]
visit http://apps.twitter.com/app/new
2. Enter information in `Name`, `Description`, `Website`, and `Callback URL` 
fields like example provided below. For `Callback URL` make sure to copy/paste 
the following: `http://127.0.0.1:1410`
3. Once the app is created, copy and paste `consumer key` and `consumer secret key` 
into the `create_token()` function (see demo below).

|                 |                                         |
|-----------------|-----------------------------------------|
| Name            | rtweet_app                              |
| Description     | Twitter follows and selective exposure  |
| Website         | http://twitter.com/kearneymw            |
| Callback URL    | http://127.0.0.1:1410                   |

## contact
- email me at mkearney@ku.edu

----------------------------------------
----------------------------------------


## demo
```{r}
library(rtweet)
```

### create_token()
```{r, echo = TRUE, eval = FALSE}
### first time running create_token() function should open web browser 
### select yes/agree to authorize each app
### replace 'appX_name' with name of your application (see: 'obtaining access tokens')
### replace 'xxxx' with alpha-numeric keys associated with your apps
tokens <- c(
  create_token(app = "app1_name",
    consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
    consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
  create_token(app = "app2_name",
    consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
    consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
### save tokens for easy use in future sessions
### according to Dr. Hadley Wickham best practice for save location...
filename <- normalizePath("~/")
save(tokens, file = filename)
```

```{r, echo = TRUE, eval = FALSE}
### to load tokens in new session
tokens <- get_tokens()
### or don't even do this - functions are built to fetch your tokens for you!
```

### search_tweets()
```{r, echo = TRUE, eval = FALSE}
### search tweets via REST API
### (number of tweets returned varies; broader searchers are best)
elect16 <- search_tweets(
  q = "election2016", count = 500)
elect16
```

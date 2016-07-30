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

## Obtaining access tokens
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

## Creating tokens in R

- Using the information obtained from `obtaining access tokens`
above, generate a token via the `create_token` function.

- If this is the first time running `create_token` for an
app, a web browser should pop up. Select yes/agree to 
authorize once for each app. In the example below, you should
replace 'appX_name' with name of your application 
(see: 'obtaining access tokens') and replace 'xxxx...' with 
alpha-numeric keys associated with your apps, which you can find by
clicking a link that says something like 'managing keys and access 
tokens') on the screen that pops up after submitting/creating a 
new Twitter app.

```{r, echo = TRUE, eval = FALSE}
library(rtweet)

tokens <- c(
  create_token(app = "app1_name",
    consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
    consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
  create_token(app = "app2_name",
    consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
    consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
```

- Create an environmental variable using [steps outlined by Hadley](https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd).
Translated to this particular application, open a blank plain text 
document, which you can do in TextEdit, Notepad, or RStudio 
(File > New File > Text File) and type the following:

```
TWITTER_PAT=blahblahblahblahblahblah
```

where 'blahblahblahblahblahblah' is the path to wherever you've saved 
the token(s). Since it's recommended that you save the plain text file 
in your home directory, I saved my tokens there as well (as seen in 
code below). You can find your home directory by entering 
`normalizePath("~/")` in the R console. Finally, save the text file
you just created to the home directory as well, and you're basically
done.

```{r, echo = TRUE, eval = FALSE}
filename <- normalizePath("~/")
save(tokens, file = filename)
```

- Restart R so it can register your new file/path and then load your
tokens using the `get_tokens` function.

```{r, echo = TRUE, eval = FALSE}
tokens <- get_tokens()
```

Or, more conveiently, don't load your tokens and instead let 
`rtweet` functions fetch your tokens for you!

```{r, echo = TRUE, eval = FALSE}
elect16 <- search_tweets(q = "election2016", count = 500)
elect16
```

## contact
- email me at mkearney@ku.edu

----------------------------------------
----------------------------------------

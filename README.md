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
into the `get_token()` function (see demo below).

|                 |                                         |
|-----------------|-----------------------------------------|
| Name            | rtweet_app                                |
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

### get_token()
```{r, echo = TRUE, eval = FALSE}
### first time running get_token() function should open web browser 
### select yes/agree to authorize each app
### replace 'appX_name' with name of your application (see: 'obtaining access tokens')
### replace 'xxxx' with alpha-numeric keys associated with your apps
tokens <- c(get_token(app = "app1_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
            get_token(app = "app2_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
            get_token(app = "app3_name",
                      consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxx",
                      consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
### save tokens for easy use in future sessions
### take note of working directory
save(tokens, file = "tokens")
getwd()
```

```
##  [1] "/Users/mwk/r"
```

```{r, echo = TRUE, eval = FALSE}
### to load tokens in new session
load("/Users/mwk/r/tokens") # specify whole path if changing working directory
```

### search_tweets()
```{r, echo = TRUE, eval = FALSE}
### search tweets via REST API
### (number of tweets returned varies; broader searchers are best)
elect16 <- search_tweets(q = "election2016",
  count = 500, token = tokens[[2]])
elect16
```

```
## Source: local data frame [900 x 116]
## 
##             status_id
##                 <chr>
## 1  750027959646453760
## 2  750027943057977344
## 3  750027784836227072
## 4  750027704158879744
## 5  750027639931502592
## 6  750027345151557632
## 7  750027283990089728
## 8  750027096173449216
## 9  750027093384294401
## 10 750027057745129472
## ..                ...
## Variables not shown: text <chr>, truncated <chr>, result_type <chr>,
##   created_at <date>, source <chr>, in_reply_to_status_id <chr>,
##   in_reply_to_user_id <chr>, in_reply_to_screen_name <chr>,
##   is_quote_status <chr>, retweet_count <chr>, favorite_count <chr>,
##   favorited <chr>, retweeted <chr>, lang <chr>, quoted_status_id <chr>,
##   place_id <chr>, place_url <chr>, place_type <chr>, place_name <chr>,
##   place_full_name <chr>, place_country_code <chr>, place_country <chr>,
##   place_long1 <list>, place_long2 <list>, place_long3 <list>, place_long4
##   <list>, place_lat1 <list>, place_lat2 <list>, place_lat3 <list>,
##   place_lat4 <list>, entities_hashtag <list>,
##   entities_user_mentions_user_id <list>,
##   entities_user_mentions_screen_name <list>, entities_user_mentions_name
##   <list>, entities_urls_expanded <list>, entities_media_id <list>,
##   entities_media_source_status_id <list>, entities_media_source_user_id
##   <list>, entities_media_expanded_url <list>, user_id <chr>, name <chr>,
##   screen_name <chr>, location <chr>, description <chr>, url <chr>,
##   entities_description_url <list>, entities_url <list>, protected <lgl>,
##   followers_count <int>, friends_count <int>, listed_count <int>,
##   favourites_count <int>, utc_offset <int>, time_zone <chr>, geo_enabled
##   <lgl>, verified <lgl>, statuses_count <int>, retweet_status_id <chr>,
##   retweet_text <chr>, retweet_truncated <chr>, retweet_result_type <chr>,
##   retweet_created_at <date>, retweet_source <chr>,
##   retweet_in_reply_to_status_id <chr>, retweet_in_reply_to_user_id <chr>,
##   retweet_in_reply_to_screen_name <chr>, retweet_is_quote_status <chr>,
##   retweet_retweet_count <chr>, retweet_favorite_count <chr>,
##   retweet_favorited <chr>, retweet_retweeted <chr>, retweet_lang <chr>,
##   retweet_quoted_status_id <chr>, retweet_user_id <chr>, retweet_name
##   <chr>, retweet_screen_name <chr>, retweet_location <chr>,
##   retweet_description <chr>, retweet_url <chr>,
##   retweet_entities_description_url <list>, retweet_entities_url <list>,
##   retweet_protected <lgl>, retweet_followers_count <int>,
##   retweet_friends_count <int>, retweet_listed_count <int>,
##   retweet_favourites_count <int>, retweet_utc_offset <int>,
##   retweet_time_zone <chr>, retweet_geo_enabled <lgl>, retweet_verified
##   <lgl>, retweet_statuses_count <int>, retweet_place_id <chr>,
##   retweet_place_url <chr>, retweet_place_type <chr>, retweet_place_name
##   <chr>, retweet_place_full_name <chr>, retweet_place_country_code <chr>,
##   retweet_place_country <chr>, retweet_place_long1 <list>,
##   retweet_place_long2 <list>, and 15 more <...>.
```


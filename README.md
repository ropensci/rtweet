<!-- README.md is generated from README.Rmd. Please edit that file -->
rtweet: Collecting Twitter Data
===============================

[![CRAN Version](http://www.r-pkg.org/badges/version/rtweet)](http://cran.r-project.org/package=rtweet) ![Downloads](http://cranlogs.r-pkg.org/badges/rtweet)

R package for collecting Twitter data via Twitter's REST and stream API's.

Key features

-   ***SAVING DATA AS CSV***: Several people have asked how to save data as a CSV file (or something they can open with Excel). There is now a `save_as_csv` function in the development (github) version of `rtweet`. If you'd like to save CSV files using the CRAN version, then [use this code](https://gist.github.com/mkearney/7474b64f9db177435de540f5fa63a087) until the next update is posted to CRAN.

-   ***UPDATE***: Most functions now return data tables for both tweets **and** users. So, if you search for tweets, you also get data for the users responsible for the returned tweets. If you look up users, you also get the most recent tweet for each user.

-   Gather **tweet data** by searching past tweets `search_tweets()`, streaming live tweets `stream_tweets()`, or collecting tweets from a user's timeline `get_timeline()`. Easily return data on **thousands** of tweets at a time.

-   Gather **user data** by looking up Twitter users `lookup_users()`. Easily return data on thousands of users.

-   Gather **followers** and **friends** data by collecting the ids of accounts *following* a user `get_followers()` or the ids of accounts *followed by* a user `get_friends()`.

-   Organized and easily translatable data formats. Functions return tibble data tables **ready** for data analysis.

-   Tweet data functions return not only text of tweets, but a host of other variables (up t 27 columns), including the number of times a tweet has been retweeted (`retweet_count`) and favorited (`favorite_count`). To gauge user interactions, there are also seperate variables that identify replies by source tweet (`in_reply_to_status_id_str`) and by source user ID (`in_reply_to_status_id_str`). Variables also indicate whether the tweet quotes another tweet (`is_quote_status`), and, if so, the quoted tweet ID (`quoted_status_id_str`).

Install
-------

To get the current released version from CRAN:

``` r
install.packages("rtweet")
library(rtweet)
```

To get the current development version from github:

``` r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("mkearney/rtweet")
```

Getting started
---------------

-   *Quick authorization method*: go (here)\[<http://apps.twitter.com/app/new>\] and create a Twitter app. In the callback field, enter: `http://127.0.0.1:1410`
-   Enter your consumer (api) and secret keys

``` r
token <- create_token(app = "rtweet_tokens", # whatever you named app
  consumer_key = "XZgqotgOZNKlLFJqFbd8NjUtL",
  consumer_secret = "1rDnU3H3nrxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
# I xxx'd out the secret key, but you get the idea
```

``` r
tw <- search_tweets("r", n = 1200, token = token, lang = "en")
#> Searching for tweets...
#> Finished collecting tweets!
print(tw, width = 500)
#> # A tibble: 1,200 x 27
#>             created_at          status_id retweet_count favorite_count
#>                 <time>              <chr>         <int>          <int>
#> 1  2016-08-16 19:57:06 765714031621451777          1220           1972
#> 2  2016-08-16 17:26:22 765676100747833345          1696           7688
#> 3  2016-08-15 23:16:23 765401795258482689          1902           6674
#> 4  2016-08-16 20:39:09 765724613707911168             0              0
#> 5  2016-08-16 20:39:09 765724613682733056          2421              0
#> 6  2016-08-16 20:39:08 765724612860588032             0              0
#> 7  2016-08-16 20:39:08 765724612697190400             1              0
#> 8  2016-08-16 20:39:08 765724611006832640             0              0
#> 9  2016-08-16 20:39:08 765724610939629568           170              0
#> 10 2016-08-16 20:39:08 765724610516045824             1              0
#>                                                                                                                                            text
#>                                                                                                                                           <chr>
#> 1                                                                         H A L E B\nF O R E V E R.\n#PrettyLittleLiars https://t.co/SxM0NznqfC
#> 2  NEW VIDEO!Which side R U on? I WAS BORED SO I JUST MADE A DISS TRACK #Justindeactivatedparty #SelenaEndedJustinParty https://t.co/jvxEuBDVXn
#> 3                                                                                         Rest in Peace R 2 ❤️ D 2️⃣...... https://t.co/oOPGmWlbM6
#> 4      #US #Stamps US Revenue #Stamp, Scott #R 79, $1.60 Green Foreign Exchange… https://t.co/bwaqfBubQ2 #USA #American https://t.co/WWLSxgWti3
#> 5                                                              RT @sudinyainyai: R' charge no 1. Pocketrocketman ! #MAS https://t.co/SYKSFLZDRl
#> 6                                                         Would it be weird to start a snapstreak with a random person? https://t.co/FIbLg2H7KX
#> 7                                                                                                           RT @hailey3201: I d o n o t c a r e
#> 8                                                                                @NotoriousD_R_E \U0001f612 you was gon wear a hat anyways Bruh
#> 9                                  RT @TheFunnyVine: A sneak peek at the opening of the second season of Stranger Thing https://t.co/AHTal2olYV
#> 10     /r/IndieDev Post: How do you organize your workspace/project directories?: So, I've been lo... https://t.co/BjY6rpfTi7 #reddit #indiedev
#>    in_reply_to_status_id in_reply_to_user_id is_quote_status
#>                    <chr>               <chr>           <lgl>
#> 1                   <NA>                <NA>           FALSE
#> 2                   <NA>                <NA>           FALSE
#> 3                   <NA>                <NA>           FALSE
#> 4                   <NA>                <NA>           FALSE
#> 5                   <NA>                <NA>           FALSE
#> 6                   <NA>                <NA>           FALSE
#> 7                   <NA>                <NA>           FALSE
#> 8     765723973233569792           101614801           FALSE
#> 9                   <NA>                <NA>           FALSE
#> 10                  <NA>                <NA>           FALSE
#>    quoted_status_id              source  lang    user_id user_mentions
#>               <chr>               <chr> <chr>      <chr>        <list>
#> 1              <NA>  Twitter Web Client    en  109303622     <lgl [1]>
#> 2              <NA>  Twitter Web Client    en  910874659     <lgl [1]>
#> 3              <NA>  Twitter for iPhone    en   75641903     <lgl [1]>
#> 4              <NA>             dlvr.it    en 4741461974     <lgl [1]>
#> 5              <NA> Twitter for Android    en  403866129     <chr [1]>
#> 6              <NA>             dlvr.it    en 2663919020     <lgl [1]>
#> 7              <NA>  Twitter for iPhone    en 2520839442     <chr [1]>
#> 8              <NA>  Twitter for iPhone    en 2738847143     <chr [1]>
#> 9              <NA>  Twitter for iPhone    en  126112324     <chr [1]>
#> 10             <NA>         twitterfeed    en 2811716900     <lgl [1]>
#>     hashtags      urls is_retweet  retweet_status_id  place_name
#>       <list>    <list>      <lgl>              <chr>       <chr>
#> 1  <chr [1]> <lgl [1]>      FALSE               <NA> Burbank, CA
#> 2  <chr [2]> <chr [1]>      FALSE               <NA>        <NA>
#> 3  <lgl [1]> <lgl [1]>      FALSE               <NA>        <NA>
#> 4  <chr [6]> <chr [1]>      FALSE               <NA>        <NA>
#> 5  <chr [1]> <lgl [1]>       TRUE 765557886399320064        <NA>
#> 6  <lgl [1]> <chr [1]>      FALSE               <NA>        <NA>
#> 7  <lgl [1]> <lgl [1]>       TRUE 765724577695662080        <NA>
#> 8  <lgl [1]> <lgl [1]>      FALSE               <NA>        <NA>
#> 9  <lgl [1]> <chr [1]>       TRUE 765718563139256320        <NA>
#> 10 <chr [2]> <chr [1]>      FALSE               <NA>        <NA>
#>          country long1 long2 long3 long4  lat1  lat2  lat3  lat4
#>            <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  United States    NA    NA    NA    NA    NA    NA    NA    NA
#> 2           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 3           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 4           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 5           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 6           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 7           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 8           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 9           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 10          <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> # ... with 1,190 more rows
```

``` r
print(users_data(tw), width = 500)
#> # A tibble: 1,200 x 27
#>             created_at          status_id retweet_count favorite_count
#>                 <time>              <chr>         <int>          <int>
#> 1  2016-08-16 19:57:06 765714031621451777          1220           1972
#> 2  2016-08-16 17:26:22 765676100747833345          1696           7688
#> 3  2016-08-15 23:16:23 765401795258482689          1902           6674
#> 4  2016-08-16 20:39:09 765724613707911168             0              0
#> 5  2016-08-16 20:39:09 765724613682733056          2421              0
#> 6  2016-08-16 20:39:08 765724612860588032             0              0
#> 7  2016-08-16 20:39:08 765724612697190400             1              0
#> 8  2016-08-16 20:39:08 765724611006832640             0              0
#> 9  2016-08-16 20:39:08 765724610939629568           170              0
#> 10 2016-08-16 20:39:08 765724610516045824             1              0
#>                                                                                                                                            text
#>                                                                                                                                           <chr>
#> 1                                                                         H A L E B\nF O R E V E R.\n#PrettyLittleLiars https://t.co/SxM0NznqfC
#> 2  NEW VIDEO!Which side R U on? I WAS BORED SO I JUST MADE A DISS TRACK #Justindeactivatedparty #SelenaEndedJustinParty https://t.co/jvxEuBDVXn
#> 3                                                                                         Rest in Peace R 2 ❤️ D 2️⃣...... https://t.co/oOPGmWlbM6
#> 4      #US #Stamps US Revenue #Stamp, Scott #R 79, $1.60 Green Foreign Exchange… https://t.co/bwaqfBubQ2 #USA #American https://t.co/WWLSxgWti3
#> 5                                                              RT @sudinyainyai: R' charge no 1. Pocketrocketman ! #MAS https://t.co/SYKSFLZDRl
#> 6                                                         Would it be weird to start a snapstreak with a random person? https://t.co/FIbLg2H7KX
#> 7                                                                                                           RT @hailey3201: I d o n o t c a r e
#> 8                                                                                @NotoriousD_R_E \U0001f612 you was gon wear a hat anyways Bruh
#> 9                                  RT @TheFunnyVine: A sneak peek at the opening of the second season of Stranger Thing https://t.co/AHTal2olYV
#> 10     /r/IndieDev Post: How do you organize your workspace/project directories?: So, I've been lo... https://t.co/BjY6rpfTi7 #reddit #indiedev
#>    in_reply_to_status_id in_reply_to_user_id is_quote_status
#>                    <chr>               <chr>           <lgl>
#> 1                   <NA>                <NA>           FALSE
#> 2                   <NA>                <NA>           FALSE
#> 3                   <NA>                <NA>           FALSE
#> 4                   <NA>                <NA>           FALSE
#> 5                   <NA>                <NA>           FALSE
#> 6                   <NA>                <NA>           FALSE
#> 7                   <NA>                <NA>           FALSE
#> 8     765723973233569792           101614801           FALSE
#> 9                   <NA>                <NA>           FALSE
#> 10                  <NA>                <NA>           FALSE
#>    quoted_status_id              source  lang    user_id user_mentions
#>               <chr>               <chr> <chr>      <chr>        <list>
#> 1              <NA>  Twitter Web Client    en  109303622     <lgl [1]>
#> 2              <NA>  Twitter Web Client    en  910874659     <lgl [1]>
#> 3              <NA>  Twitter for iPhone    en   75641903     <lgl [1]>
#> 4              <NA>             dlvr.it    en 4741461974     <lgl [1]>
#> 5              <NA> Twitter for Android    en  403866129     <chr [1]>
#> 6              <NA>             dlvr.it    en 2663919020     <lgl [1]>
#> 7              <NA>  Twitter for iPhone    en 2520839442     <chr [1]>
#> 8              <NA>  Twitter for iPhone    en 2738847143     <chr [1]>
#> 9              <NA>  Twitter for iPhone    en  126112324     <chr [1]>
#> 10             <NA>         twitterfeed    en 2811716900     <lgl [1]>
#>     hashtags      urls is_retweet  retweet_status_id  place_name
#>       <list>    <list>      <lgl>              <chr>       <chr>
#> 1  <chr [1]> <lgl [1]>      FALSE               <NA> Burbank, CA
#> 2  <chr [2]> <chr [1]>      FALSE               <NA>        <NA>
#> 3  <lgl [1]> <lgl [1]>      FALSE               <NA>        <NA>
#> 4  <chr [6]> <chr [1]>      FALSE               <NA>        <NA>
#> 5  <chr [1]> <lgl [1]>       TRUE 765557886399320064        <NA>
#> 6  <lgl [1]> <chr [1]>      FALSE               <NA>        <NA>
#> 7  <lgl [1]> <lgl [1]>       TRUE 765724577695662080        <NA>
#> 8  <lgl [1]> <lgl [1]>      FALSE               <NA>        <NA>
#> 9  <lgl [1]> <chr [1]>       TRUE 765718563139256320        <NA>
#> 10 <chr [2]> <chr [1]>      FALSE               <NA>        <NA>
#>          country long1 long2 long3 long4  lat1  lat2  lat3  lat4
#>            <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  United States    NA    NA    NA    NA    NA    NA    NA    NA
#> 2           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 3           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 4           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 5           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 6           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 7           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 8           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 9           <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> 10          <NA>    NA    NA    NA    NA    NA    NA    NA    NA
#> # ... with 1,190 more rows
```

-   ***Recommended authorization method***: [Obtaining and using access tokens](https://github.com/mkearney/rtweet/blob/master/vignettes/tokens.md) (vignette showing how to *sustainably* setup authorization to use Twitter's APIs)

-   Function demo: [Collecting and analyzing tweets using the `search_tweets()` function](https://github.com/mkearney/rtweet/blob/master/vignettes/search_tweets.md) (vignette showing how to use the `search_tweets()` function)

Package description
-------------------

More technical description: An implementation of calls designed to extract and organize Twitter data via Twitter's REST and stream API's. Functions formulate GET and POST requests and convert response objects to more user friendly structures, e.g., data frames or lists. Specific consideration is given to functions designed to return tweets, friends, and followers.

Contact
-------

Email me at <mkearney@ku.edu>

To Do List
----------

-   API Functions
-   `get_retweeters()` Retrieve users retweeting a status (in progress)
-   `get_list()` Retrieve users in list
-   `search_users()` Search for users by name, interest, etc.
-   `lookup_tweets()` Look up tweets via status\_id.

-   Vignettes
-   Guide to `stream_tweets()`
-   Guide to `get_friends()` and `get_followers()`

-   Documentation
-   Search query syntax
-   Stream syntax i.e., filter vs tracking vs location

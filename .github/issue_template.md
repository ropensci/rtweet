<!-- This is an issue template for bugs and requests for R pkg rtweet -->

<!-- If you've encountered a likely bug in rtweet, please take a few seconds to 
look through existing issues for a similar issue. If you don't see a related 
issue, please complete the prompts below to make it easier to replicate and 
[hopefully] resolve your issue.  -->

### Problem

<!-- Succinctly describe the problem (be as specific as you think necessary) -->

### Expected behavior

<!-- Describe the behavior/result you expected -->

### Reproduce the problem

<!-- Describe and provide relevant code to reproduce the problem -->
<!-- If code doesn't always produce error, provide approximate code anyway -->

``` r
## insert code here

```

### rtweet version

<!-- run the code below and copy/paste the output -->

``` r
## copy/paste output
packageVersion("rtweet")
```


### Session info

<!-- run the code below and copy/paste the output -->

``` r
## copy/paste output
sessionInfo()
```

### Token

<!-- run the code below and copy/paste the output. if you don't feel comfortable 
sharing that information, then share the first 3-4 characters for the 
oauth_app ($APP_NAME$) and key ($KEY$) as they appear in the printed output 
#> <Token>
#> <oauth_endpoint>
#>  request:   https://api.twitter.com/oauth/request_token
#>  authorize: https://api.twitter.com/oauth/authenticate
#>  access:    https://api.twitter.com/oauth/access_token
#> <oauth_app> {{$APP_NAME$}}
#>   key:    {{$KEY$}}****************
#>   secret: <hidden>
#> <credentials> oauth_token, oauth_token_secret
-->

``` r
## copy/paste output
rtweet::get_token()
```

<!-- If you think the problem may be related to features/limitations of 
Twitter's API, you can find more information about Twitter's APIs here: 
https://developer.twitter.com/en/docs.html -->

<!-- Thank you for using and improving rtweet!  -->

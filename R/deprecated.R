#' Deprecated functions in rtweet
#'
#' These functions do not work due to the drop of API v1 in favor of API v2.
#' They will be removed in the next version of this package.
#' \itemize{
#'  \item `clean_tweets()`: No replacement.
#'  \item `collections()`: No replacement.
#'  \item `direct_messages()`: No replacement (yet?).
#'  \item `do_call_rbind()`: No replacement (if any it will be a method).
#'  \item `get_favorites()`: Use `tweet_liking_users()`.
#'  \item `get_followers()`: Use `user_follwing()`.
#'  \item `get_friends()`: Use `user_followers()`.
#'  \item `network_data()` and `network_graph()`: No replacement.
#'  \item `lat_lng()`: No replacement.
#'  \item `list_memberships()`: Use `list_users()`.
#'  \item `get_mentions()`: Use `user_mentions()`
#'  \item `user_block()`:
#'  \item `post_favorite()`:
#'  \item `post_list()`: `lists_subscribers()`, `list_subscriptions()`, `list_members()`, `lists_users()`
#'  \item `post_tweet()`: Use `tweet_post()`.
#'  \item `post_follow()`:
#'  \item `post_destroy()`: Use `tweet_delete()`.
#'  \item `search_fullarchive()`: Use `tweet_search_all()`.
#'  \item `search_30d()`: Use `tweet_search_recent()`.
#'  \item `rate_limit()`: No replacement (already implemented now).
#'  \item `save_as_csv()`: No replacement.
#'  \item `search_tweets()`: Use `tweet_search_recent()`.
#'  \item `search_tweets2()`: Use `tweet_search_recent()`.
#'  \item `search_users()`: Use `user_search()`.
#'  \item `lookup_tweets()`: Use `tweet_get()`, `tweet_retweeted_by()`.
#'  \item `stream_tweets()`: Use `filtered_stream()`, `stream_add_rule()`, `stream_rm_rule()` and `sample_stream()`.
#'  \item `get_timeline()`: Use `user_timeline()`.
#'  \item `get_my_timeline()`: Use `user_timeline()`.
#'  \item `get_token()`: Use `auth_get()`.
#'  \item `get_tokens()`: Use `auth_get()`.
#'  \item `get_trends()`: No replacement.
#'  \item `trends_available()`: No replacement.
#'  \item `ts_data()`: No replacement.
#'  \item `ts_plot()`: No replacement.
#'  \item `tweet_shot()`: No replacement.
#'  \item `tweet_threading()`: Use `tweet_get()`.
#' }
#'
#' @name rtweet-deprecated
NULL

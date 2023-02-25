# Helper function to test a tweet and learn about what is returned by each
# combination of field and expansion.
helper <- function(tweet = "1615009611186069504") {
  se <- set_expansions(user = NULL)
  sf <- set_fields()
  out <- vector("list", (sum(lengths(sf)))*(length(se)))
  out_sf <- vector("list", (length(se)))
  out_se <- vector("list", sum(lengths(sf)))
  i <- 1
  for (i_se in seq_along(se)) {
    for (i_sf in seq_along(sf)) {
      for (i_sfi in seq_along(sf[[i_sf]])) {

        sf_test <- list(sf[[i_sf]][i_sfi])
        names(sf_test) <- names(sf)[i_sf]
        out[[i]] <- tweet_get(tweet,
                              expansions = se[i_se], fields = sf_test,
                              parse = FALSE)
        out_sf[[i]] <- sf_test
        out_se[[i]] <- se[i_se]
        i <- i + 1
      }
    }
  }
  l2df <- list2DF(list(out = out, fields = out_sf, expansion = out_se))
  l2df
}

if (FALSE) {
  l2df <- helper()
  inc <- lapply(l2df$out, function(x){names(x[[1]]$includes)})
  inc[lengths(inc) == 0] <- NA
  df <- data.frame(field = unlist(out_sf, recursive = TRUE, FALSE),
                   expansion = unlist(l2df$expansion),
                   includes = unlist(inc))
  l <- lapply(l2df$out, function(x){
    names(list_minus(x[[1]]$data, c("edit_history_tweet_ids", "id", "text")))})
  df$data <- l
  df <- distinct(df)
}
# Expansions that produces "includes":
#     attachments.media_keys, author_id, edit_history_tweet_ids, geo.place_id,
#     referenced_tweets.id.author_id
# These fields create a data with its name:
#     attachments, conversation_id, created_at, entities, geo, lang,
#     public_metrics, possibly_sensitive, reply_settings, author_id
#
# attachments.media_keys is required for any media.fields (go to includes)
# geo.place_id is required for any place.fields (go to includes)
# attachments.poll_ids is required by any poll.fields (go to includes)
# referenced_tweets.id is required by any tweet.fields (go to includes)
# author_id, entities.mentions.username, in_reply_to_user_id, referenced_tweets.id.author_id is required by any user.fields (go to includes)
#

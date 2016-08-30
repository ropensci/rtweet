
#'@export
tweets <- setClass("tweets", slots = c(
	created_at = "POSIXct",
	status_id = "character",
	user_id = "character",
	screen_name = "character",
	followers_count = "numeric",
	friends_count = "numeric",
	statuses_count = "numeric",
	location = "character",
	verified = "logical",
	text = "character",
	retweet_count = "numeric",
	favorite_count = "numeric",
	in_reply_to_status_id = "character",
	in_reply_to_user_id = "character",
	in_reply_to_screen_name = "character",
	is_quote_status = "logical",
	quoted_status_id = "character",
	source = "character",
	lang = "character",
	user_mentions = "character",
	hashtags = "character",
	urls = "character",
	is_retweet = "logical",
	retweet_status_id = "character",
	place_name = "character",
	country = "character",
	long1 = "numeric",
	long2 = "numeric",
	long3 = "numeric",
	long4 = "numeric",
	lat1 = "numeric",
	lat2 = "numeric",
	lat3 = "numeric",
	lat4 = "numeric"))

#'make_tweets
#'@description convert to S4 class
#'@param x Tweets data frame
#'@importFrom dplyr left_join
#'@importFrom methods new
#'@export
make_tweets <- function(x) {
	ux <- users_data(x)
	ux <- ux[, !names(ux) %in% c("lang", "screen_name", "created_at")]
	x <- dplyr::left_join(x, ux, by = "user_id")

	urls <- sapply(x$urls, paste, collapse = " ")
	user_mentions <- sapply(x$user_mentions, paste, collapse = " ")
	hashtags <- sapply(x$hashtags, paste, collapse = " ")

	new("tweets",
		created_at = as.POSIXct(x$created_at),
		status_id = as.character(x$status_id),
		user_id = as.character(x$user_id),
		screen_name = as.character(x$screen_name),
		followers_count = as.double(x$followers_count),
		friends_count = as.double(x$friends_count),
		statuses_count = as.double(x$statuses_count),
		location = as.character(x$location),
		verified = as.logical(x$verified),
		text = as.character(x$text),
		retweet_count = as.double(x$retweet_count),
		favorite_count = as.double(x$favorite_count),
		in_reply_to_status_id = as.character(x$in_reply_to_status_id),
		in_reply_to_user_id = as.character(x$in_reply_to_user_id),
		#in_reply_to_screen_name = as.character(x$in_reply_to_screen_name,
		is_quote_status = as.logical(x$is_quote_status),
		quoted_status_id = as.character(x$quoted_status_id),
		source = as.character(x$source),
		lang = as.character(x$lang),
		user_mentions = as.character(x$user_mentions),
		hashtags = as.character(x$hashtags),
		urls = as.character(x$urls),
		is_retweet = as.logical(x$is_retweet),
		retweet_status_id = as.character(x$retweet_status_id),
		place_name = as.character(x$place_name),
		country = as.character(x$country),
		long1 = as.numeric(x$long1),
		long2 = as.numeric(x$long2),
		long3 = as.numeric(x$long3),
		long4 = as.numeric(x$long4),
		lat1 = as.numeric(x$lat1),
		lat2 = as.numeric(x$lat2),
		lat3 = as.numeric(x$lat3),
		lat4 = as.numeric(x$lat4))
}

trunc_text <- function(txt, n) {
	paste0(strtrim(encodeString(txt), width = n), " ...")
}

#'@export
setMethod("show",
	"tweets",
	function(object) {
		x <- object

		n <- floor((getOption("width") - nchar("Tweets")) / 2.75)

		twt_df <- data.frame(
			created_at = x@created_at,
			retweets = x@retweet_count,
			favorites = x@favorite_count,
			text = trunc_text(x@text, 40))

		usr_df <- data.frame(
			screen_name = x@screen_name,
			followers = x@followers_count,
			friends = x@friends_count,
			statuses = x@statuses_count,
			location = x@location,
			verified = x@verified)

		stars <- paste(rep("*", n), collapse = "")

		cat(stars, "Tweets", stars, fill = TRUE)

		print(twt_df, row.names = TRUE,
			print.gap = 1L, max = 40, na.print = "NA")

		cat("\n")
		cat(stars, "Users", stars, fill = TRUE)

		print(usr_df, row.names = TRUE,
			print.gap = 1L, max = 60, na.print = "NA")
		cat("\n")
	}
)

#' plot tweets
#'
#' @description Plot tweets class data
#'
#' @param x tweets data frame object
#' @param \dots Other arguments passed to plot
#' @import ggplot2
#' @export
plot.rt_df <- function(x, ...) {
  x$group <- sample(c("a", "b", "c", "d", "e"), nrow(x), replace = TRUE)
  x$followers_count <- log(x$followers_count + 2)
  x$friends_count <- log(x$friends_count + 2)
  ggplot(x, aes_string(x = "friends_count", y = "followers_count",
      color = "group", alpha = .85)) +
    theme_minimal() + geom_point(size = 5) +
    labs(x = "# of Friends", y = "# of Followers",
      title = "rtweet: Collecting Twitter Data") +
    theme(legend.position = "none",
    	plot.title = element_text(hjust = .45),
      text = element_text(
      	family = "Georgia", size = 14,
        face = "bold", color = "#555555"),
      axis.text = element_blank())
}

#' Class "rt_df" for Tweets data
#'
#' @description Tweets data frame
#'
#' @name rt_df-class
#' @docType class
#' @keywords classes
#' @examples
#'
#' showClass("rt_df")
#' methods(class = "rt_df")
#' @export
rt_df <- setClass("rt_df", contains = "data.frame")

#' rt_data
#'
#' @description Convert tweets list to rt_df
#'
#' @param object Tweets class list object
#' @keywords classes
#' @examples
#' \dontrun{
#' tw <- search_tweets("twitter")
#' tw <- rt_data(tw)
#' tw
#' }
#' @importFrom dplyr as_data_frame
#' @export
rt_data <- function(object) {
	cols <- slotNames(object)
	data <- lapply(cols, function(x) slot(object, x))
  names(data) <- cols
  data <- as_data_frame(data)
  new("rt_df", data)
}


#' Class-tweets
#'
#' @description Tweets data list class
#'
#' @docType class
#' @keywords classes
#' @export
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
	#in_reply_to_screen_name = "character",
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


#' make_tweets
#'
#' @description Convert API data to S4 Class list object
#'
#' @name make_tweets
#' @param x Tweets data frame
#' @keywords classes
#' @import methods
#' @importFrom dplyr left_join
#' @export
make_tweets <- function(x) {
	ux <- users_data(x)
	ux <- ux[, !names(ux) %in% c("lang", "screen_name", "created_at")]
	x <- left_join(x, ux, by = "user_id")

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
		#in_reply_to_screen_name = as.character(x$in_reply_to_screen_name),
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

#' Method show
#'
#' @description Show method for tweets
#' @param object tweets object
#'
#' @docType class
#' @exportMethod show
setMethod("show",  "tweets", function(object) print.tweets(object))

#' print tweets
#'
#' @description print tweets class object
#' @param x tweets class object
#' @param \dots Other arguments passed to print
#' @export
print.tweets <- function(x, ...) {
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

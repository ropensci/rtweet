#' clean_tweets
#'
#' @description Returns list of cleaned words for each observation.
#'   Useful in analysis of tweet text.
#'
#' @param tweets Character vector of tweets text. May also provide
#'   data frame or list object with "text" named object containing
#'   tweet text.
#' @param min Numeric, minimum number of ocurrences to include in
#'   returned object. By default, \code{min = 0}, all words (except
#'   those excluded by stopwords and exclude_words) are returned.
#'   To only return words mentioned at least 3 times, for example,
#'   set (\code{min = 3}).
#' @param stopwords Character, words to exclude. By default,
#'   \code{stopwords = NULL}, uses a generic list of stopwords.
#' @param exclude_words Character, other words to exclude in
#'   addition to generic search terms.
#' @examples
#' \dontrun{
#' # search for 1000 tweets mentioning Hillary Clinton
#' hrc <- search_tweets(q = "hillaryclinton", count = 1000)
#'
#' # lookup returned user_id values
#' users <- lookup_users(hrc$user_id)
#' users
#'
#' # merge data objects
#' dat <- dplyr::left_join(hrc, users, by = "user_id")
#' dat
#'
#' # clean tweet text for each user
#' dat$words <- clean_tweets(dat, exclude_words = "hillaryclinton")
#' dat$words
#' }
#'
#' @return list object top words
#' @export
clean_tweets <- function(tweets, min = 0, stopwords = NULL,
                         exclude_words = NULL) {

  if (is.data.frame(tweets)) {
    if ("text" %in% names(tweets)) {
      tweets <- tweets$text
    } else {
      stop("Must supply character vector of tweets.", call. = FALSE)
    }
  }
  tweets <- tweets[!is.na(tweets)]
  tweets <- gsub("(RT|via)", "", tweets)
  tweets <- gsub("@\\w+", "", tweets)
  tweets <- gsub("\\w+'\\w+", "", tweets)
  tweets <- gsub("[[:punct:]]", "", tweets)
  tweets <- gsub("htt\\w+", "", tweets)
  tweets <- gsub("\\n", "", tweets)
  tweets <- gsub("[[:digit:]]", "", tweets)
  tweets <- gsub("[^[:alnum:] ]", "", tweets)
  tweets <- tolower(tweets)
  tweets <- lapply(tweets, function(x)
    unlist(strsplit(trimws(x), split = " ")))

  if (is.null(stopwords)) {
    stopwords <- c("a", "a's", "able", "about", "above", "according",
      "accordingly", "across", "actually", "after", "afterwards",
      "again", "against", "ain't", "all", "allow", "allows", "im",
      "almost", "alone", "along", "already", "also", "although",
      "always", "am", "among", "amongst", "an", "and", "another",
      "any", "anybody", "anyhow", "anyone", "anything", "anyway",
      "anyways", "anywhere", "amp", "apart", "appear", "appreciate",
      "appropriate", "are", "aren't", "around", "as", "aside",
      "ask", "asking", "associated", "at", "available", "away",
      "awfully", "b", "be", "became", "because", "become",
      "becomes", "becoming", "been", "before", "beforehand",
      "behind", "being", "believe", "below", "beside", "besides",
      "best", "better", "between", "beyond", "both", "brief",
      "but", "by", "c", "c'mon", "c's", "came", "can", "can't",
      "cannot", "cant", "cause", "causes", "certain", "certainly",
      "changes", "clearly", "co", "com", "come", "comes", "concerning",
      "consequently", "consider", "considering", "contain",
      "containing", "contains", "corresponding", "could", "couldn't",
      "course", "currently", "d", "definitely", "described",
      "despite", "did", "didn't", "different", "do", "does",
      "doesn't", "doing", "don't", "done", "down", "downwards",
      "during", "e", "each", "edu", "eg", "eight", "either",
      "else", "elsewhere", "enough", "entirely", "especially",
      "et", "etc", "even", "ever", "every", "everybody", "everyone",
      "everything", "everywhere", "ex", "exactly", "example",
      "except", "f", "far", "few", "fifth", "first", "five",
      "followed", "following", "follows", "for", "former",
      "formerly", "forth", "four", "from", "further", "furthermore",
      "g", "get", "gets", "getting", "given", "gives", "go", "guys",
      "goes", "going", "gone", "got", "gotten", "greetings", "guy",
      "h", "had", "hadn't", "happens", "hardly", "has", "hasn't",
      "have", "haven't", "having", "he", "he's", "hello", "help",
      "hence", "her", "here", "here's", "hereafter", "hereby",
      "herein", "hereupon", "hers", "herself", "hi", "him", "hey",
      "himself", "his", "hither", "hopefully", "how", "howbeit",
      "however", "i", "i'd", "i'll", "i'm", "i've", "ie", "if",
      "ignored", "immediate", "in", "inasmuch", "inc", "indeed",
      "indicate", "indicated", "indicates", "inner", "insofar",
      "instead", "into", "inward", "is", "isn't", "it", "it'd",
      "it'll", "it's", "its", "itself", "j", "just", "k", "keep",
      "keeps", "kept", "know", "knows", "known", "l", "last",
      "lately", "later", "latter", "latterly", "least", "less",
      "lest", "let", "let's", "like", "liked", "likely", "little",
      "look", "looking", "looks", "ltd", "m", "mainly", "many",
      "may", "maybe", "me", "mean", "meanwhile", "merely",
      "might", "more", "moreover", "most", "mostly", "much",
      "must", "my", "myself", "n", "name", "namely", "nd",
      "near", "nearly", "necessary", "need", "needs", "neither",
      "never", "nevertheless", "new", "next", "nine", "no",
      "nobody", "non", "none", "noone", "nor", "normally",
      "not", "nothing", "novel", "now", "nowhere", "o", "obviously",
      "of", "off", "often", "oh", "ok", "okay", "old", "on",
      "once", "one", "ones", "only", "onto", "or", "other",
      "others", "otherwise", "ought", "our", "ours", "ourselves",
      "out", "outside", "over", "overall", "own", "p", "particular",
      "particularly", "per", "perhaps", "placed", "please",
      "plus", "possible", "presumably", "probably", "provides",
      "q", "que", "quite", "qv", "r", "rather", "rd", "re",
      "they'd", "they'll", "they're", "they've", "try", "trying",
      "twice", "two", "u", "un", "under", "unfortunately",
      "unless", "unlikely", "until", "unto", "up", "upon",
      "us", "use", "used", "useful", "uses", "using", "usually",
      "uucp", "v", "value", "various", "very", "via", "viz",
      "vs", "w", "want", "wants", "was", "wasn't", "way", "we",
      "we'd", "we'll", "we're", "we've", "welcome", "well",
      "went", "were", "weren't", "what", "what's", "whatever",
      "when", "whence", "whenever", "where", "where's", "whereafter",
      "whereas", "whereby", "wherein", "whereupon", "wherever",
      "whether", "which", "while", "whither", "who", "who's",
      "whoever", "whole", "whom", "whose", "why", "will", "willing",
      "wish", "with", "within", "without", "won't", "wonder",
      "would", "would", "wouldn't", "x", "y", "yes", "yet",
      "you", "you'd", "you'll", "you're", "you've", "your",
      "yours", "yourself", "yourselves", "z", "zero", "really",
      "reasonably", "regarding", "regardless", "regards", "relatively",
      "respectively", "right", "s", "said", "same", "saw",
      "say", "saying", "says", "second", "secondly", "see",
      "seeing", "seem", "seemed", "seeming", "seems", "seen",
      "self", "selves", "sensible", "sent", "serious", "seriously",
      "seven", "several", "shall", "she", "should", "shouldn't",
      "since", "six", "so", "some", "somebody", "somehow",
      "someone", "something", "sometime", "sometimes", "somewhat",
      "somewhere", "soon", "sorry", "specified", "specify",
      "specifying", "still", "sub", "such", "sup", "sure",
      "t", "t's", "take", "taken", "tell", "tends", "th", "than",
      "thank", "thanks", "thanx", "that", "that's", "thats",
      "the", "their", "theirs", "them", "themselves", "then",
      "thence", "there", "there's", "thereafter", "thereby",
      "therefore", "therein", "theres", "thereupon", "these",
      "they", "think", "third", "this", "thorough", "thoroughly",
      "those", "though", "three", "through", "throughout",
      "thru", "thus", "to", "together", "too", "took", "toward",
      "towards", "tried", "tries", "truly")
  }

  tweets <- lapply(tweets, function(x) unlist(sapply(
    unlist(x), function(y) y[!y %in% stopwords], USE.NAMES = FALSE)))

  tweets <- lapply(tweets, function(x) unlist(sapply(
    unlist(x), function(y) y[!y %in% exclude_words], USE.NAMES = FALSE)))

  tweets <- lapply(tweets, function(x) unlist(sapply(
    unlist(x), function(y) y[y != ""], USE.NAMES = FALSE)))

  freq <- table(unlist(tweets))

  tweets <- lapply(tweets, function(x) {
    x[x %in% names(freq)[freq >= min]]
  })

  tweets
}

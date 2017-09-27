#' emojis data
#'
#' This data comes from Unicode.org, 
#' \url{http://unicode.org/emoji/charts/full-emoji-list.html}. The data are
#' codes and descriptions of emojis.
#' 
#' @docType data
#' @name emojis
#' @format A tibble with two variables and 2,623 observations.
#' @examples
#' emojis
"emojis"

#' langs data
#'
#' This data comes from the Library of Congress, 
#' \url{http://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt}. The data are
#' descriptions and codes associated with internationally recognized languages.
#' Variables include translations for each language represented as
#' bibliographic, terminologic, alpha, english, and french.
#' 
#' @docType data
#' @name langs
#' @format A tibble with five variables and 486 observations.
#' @examples
#' langs
"langs"


#' stopwordslangs data
#'
#' This data comes form a group of Twitter searches conducted on 2017-09-27.
#' The data are commonly observed words associated with 10 different languages,
#' including "ar", "en", "es", "fr", "in", "ja", "pt", ru", "tr", and "und".
#' Variables include term (the stop words), lang, and z (standard deviation
#' units of logged word occurence in stopwords sample).
#'
#' @docType data
#' @name stopwordslangs
#' @format A tibble with three variables and 2,931 observations
#' @examples
#' stop_words("en")
"stopwordslangs"

#' return stop words by language
#'
#' @param lang Language to filter on must be one of "ar", "en", "es", "fr",
#'   "in", "ja", "pt", ru", "tr", or "und".
#' @param n Number of stopwords to return.
#' @return Character vector of stop words. These words are all lower case and contain
#'   no punctuation.
#' @export
stop_words <- function(lang = NULL, n = NULL) {
  if (!is.null(lang)) {
    if (!lang %in% c("ar", "en", "es", "fr", "in", "ja", "pt", "ru", "tr", "und")) {
      stop(
        'lang must be one of "ar", "en", "es", "fr", "in", "ja", "pt", ru", "tr", or "und".',
        call. = FALSE
      )
    }
    swl <- subset(stopwordslangs, lang == lang)
  } else {
    swl <- stopwordslangs
  }
  if (!is.null(n) && n < nrow(swl)) {
    swl <- swl[seq_len(n), ]
  }
  swl$term
}

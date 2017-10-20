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
#' Variables include word (potential stop words), lang (two or three word code),
#' and p (p value associated with frequency position along a normal distribution
#' with higher values meaning the word occurs more frequently and lower values
#' meaning the words occur less frequently).
#'
#' @docType data
#' @name stopwordslangs
#' @format A tibble with three variables and 24,000 observations
#' @examples
#' stopwordslangs
"stopwordslangs"

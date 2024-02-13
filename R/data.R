#' Defunct: Emojis codes and descriptions data.
#'
#' This data comes from "Unicode.org":
#' <https://unicode.org/emoji/charts/full-emoji-list.html>.
#' The data are codes and descriptions of Emojis.
#'
#' @docType data
#' @name emojis
#' @format A tibble with two variables and 2,623 observations.
NULL

#' Defunct: Language codes recognized by Twitter data.
#'
#' This data comes from the Library of Congress,
#' <https://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt>. The data are
#' descriptions and codes associated with internationally recognized languages.
#' Variables include translations for each language represented as
#' bibliographic, terminological, alpha, English, and French.
#'
#' @docType data
#' @name langs
#' @format A tibble with five variables and 486 observations.
NULL


#' Defunct: Twitter stop words in multiple languages data.
#'
#' This data comes form a group of Twitter searches conducted at
#' several times during the calendar year of 2017. The data are
#' commonly observed words associated with 10 different languages,
#' including `c("ar", "en", "es", "fr", "in", "ja", "pt", "ru",
#' "tr", "und")`. Variables include `"word"` (potential stop
#' words), `"lang"` (two or three word code), and `"p"`
#' (probability value associated with frequency position along a
#' normal distribution with higher values meaning the word occurs more
#' frequently and lower values meaning the words occur less
#' frequently).
#'
#' @docType data
#' @name stopwordslangs
#' @format A tibble with three variables and 24,000 observations
NULL

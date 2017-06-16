
#' plain text tweets
#'
#' @param txt Tweet text, character vector.
#' @param tokenize Logical indicating whether to split each tweet into
#'   a vector of individual words. Defaults to FALSE.
#' @param hashtags Logical indicating whether to treat hashtags as
#'   normal text. Defaults to false.
#' @param mentions Logical indicating whether to treat user mentions
#'   as normal text. Defaults to false.
#' @param links Logical indicating whether to treat links as normal
#'   text. Defaults to false.
#' @return Stripped and cleaned up version of tweet text.
#' @export
plain_tweets <- function(txt, tokenize = FALSE,
                         hashtags = FALSE,
                         mentions = FALSE,
                         links = FALSE) {
    stopifnot(is.character(txt))
    txt <- tolower(txt)
    if (!links) {
        txt <- gsub("(http|www\\.)\\S{1,}", "", txt, perl = TRUE)
        txt <- gsub("[[:alnum:]]{1,}\\.(com|edu|net|org)\\S{0,}", "", txt)
    }
    if (!mentions) {
        txt <- gsub("@\\S{1,}", "", txt, perl = TRUE)
    }
    if (!hashtags) {
        txt <- gsub("#\\S{1,}", "", txt, perl = TRUE)
    }
    txt <- gsub("^rt ", "", txt)
    txt <- gsub("b\\/c", "bc", txt)
    txt <- gsub("w\\/o", "wo", txt)
    txt <- gsub("i\'ll", "i will", txt)
    txt <- gsub("amp;", " ", txt)
    txt <- gsub("\u0027|\u2019|\u2018", "", txt)
    txt <- gsub("\\'|\\-", "", txt)
    txt <- gsub("[[:punct:]]", " ", txt)
    txt <- gsub("[a-z]{0,}[[:digit:]]{1,}[a-z]{0,}", "", txt)
    txt <- gsub("[^\\w]", " ", txt, perl = TRUE)
    txt <- gsub("\\s{2,}", " ", txt, perl = TRUE)
    txt <- trimws(txt)
    if (tokenize) {
        strsplit(txt, " ")
    } else {
        txt
    }
}

#' word frequency table
#'
#' @param data Data frame with names text character column or character
#'   vector with text desired to be split into words and counted.
#' @param text If data frame is provided to data argument, then text is
#'   used to determine which column to use to make the word frequency
#'   table. Defaults to "text".
#' @param split Logical indicating whether to split character vector
#'   into words using empty spaces. Defaults to TRUE. If manually split
#'   prior to using this function, set this to FALSE.
#' @return Data frame with word and n (count) column.
word_freq <- function(data, text = "text", split = TRUE) {
  if (all(is.data.frame(data), isTRUE("text" %in% names(data)))) {
    data <- data[[text]]
  }
  stopifnot(is.character(data))
  if (split) {
    data <- unlist(plain_tweets(data, tokenize = TRUE))
    data <- data[!data %in% c(" ", "")]
  }
  data <- table(data)
  data <- data.frame(
    word = names(data),
    n = as.integer(data),
    stringsAsFactors = FALSE
  )
  if (requireNamespace("tibble", quietly = TRUE)) {
    data <- tibble::as_tibble(data)
  }
  data
}

#' word_n
#'
#' @param data Data frame with character column used to create word frequency
#'   table.
#' @param group Optional, name of group variable. Uses group if grouped_df is
#'   provided to data argument.
#' @param \dots Args passed to word_freq. Mostly used to specify name of text
#'   column, e.g., text = "hashtags", or split = FALSE.
#'
#' @return Data frame with columns word, n, and if group is provided, then a
#'   third grouping column.
#' @export
word_n <- function(data, group = NULL, ...) {
  if (inherits(data, "grouped_df")) {
    group <- names(attr(data, "labels"))
    data <- data.frame(data)
  }
  if (!is.null(group)) {
    wds <- groupfun(data, group, word_freq, ...)
    wds[order(wds[[3]], wds$n, decreasing = TRUE), ]
  } else {
    wds <- word_freq(data)
    wds[order(wds$n, decreasing = TRUE), ]
  }
}

#' stopwords
#'
#' @name stopwords
stopwords <- strsplit("the,to,a,you,and,of,i,is,for,in,this,on,my,your,it,with,at,that,me,are,be,im,have,its,all,if,so,we,can,from,now,do,out,about,but,get,not,like,by,how,as,was,an,today,just,what,will,our,who,dont,new,they,more,us,when,up,one,has,time,his,he,know,day,or,people,u,go,need,no,here,make,want,good,see,their,some,been,life,cant,were,back,great,she,got,best,rt,still,think,why,only,right,them,him,had,her,look,never,thank,would,after,youre,much,than,someone,first,please,there,should,did,also,thats,yeah,oh,also,well,then,these,use,am,bc,using,too,id,via,ive,w,because,thats,way,yes,could,would,may,which,ever,says,cc,re,very,really,such,etc", ",")[[1]]

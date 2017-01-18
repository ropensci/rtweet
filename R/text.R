
combn <- function(x, b, n.words) {
    e <- b + n.words - 1L
    if (e > length(x)) return(NA_character_)
    if (length(b:e) < n.words) return(NA_character_)
    paste(x[b:e], collapse = " ")
}

text_words <- function(txt) {
    txt <- gsub("http\\S+", " ", txt)
    txt <- gsub("\n", " ", txt)
    txt <- gsub("@\\S+", " ", txt)
    txt <- gsub("#\\S+", " ", txt)
    txt <- iconv(txt, "latin1", "ASCII", "")
    txt <- gsub("s's", "s", txt)
    txt <- gsub("'s ", " ", txt)
    txt <- gsub("won't ", "would not ", txt)
    txt <- gsub("can't ", "cannot ", txt)
    txt <- gsub("n't ", " not ", txt)
    txt <- gsub("'re ", " are ", txt)
    txt <- gsub("'ve ", " have ", txt)
    txt <- gsub("'ll ", " will ", txt)
    txt <- gsub("i'm ", "i am ", txt)
    txt <- gsub("[[:punct:]]", " ", txt)
}

wordphrases <- function(txt,
                        phrase.length = 4L,
                        num = FALSE,
                        excluded = NULL,
                        stop.words = NULL) {
    d <- tolower(txt)
    if (!is.null(excluded)) {
        d <- gsub(excluded, "", d)
    }
    d <- gsub("http\\S+", " ", d)
    d <- gsub("\n", " ", d)
    d <- gsub("@\\S+", " ", d)
    d <- gsub("#\\S+", " ", d)
    d <- iconv(d, "latin1", "ASCII", "")
    d <- gsub("s's", "s", d)
    d <- gsub("'s ", " ", d)
    d <- gsub("won't ", "would not ", d)
    d <- gsub("can't ", "cannot ", d)
    d <- gsub("n't ", " not ", d)
    d <- gsub("'re ", " are ", d)
    d <- gsub("'ve ", " have ", d)
    d <- gsub("'ll ", " will ", d)
    d <- gsub("i'm ", "i am ", d)
    d <- gsub("[[:punct:]]", " ", d)
    if (!num) {
        d <- gsub("[0-9]", " ", d)
    }
    d <- trimws(
        gsub("(      )|(     )|(    )|(   )|(  )",
             " ", d))
    w <- strsplit(d, " ")
    if (!is.null(stop.words)) {
        w <- lapply(w, function(x) x[!x %in% stop.words])
    }
    phrases <- lapply(w, function(x) {
        n <- length(x)
        if (n <= phrase.length) {
            return(NA_character_)
        } else {
            out <- vector("list", phrase.length)
            for (j in seq_len(phrase.length)) {
                i <- seq(j, n, phrase.length)
                out[[j]] <- lapply(i, function(k)
                    combn(x, k, phrase.length))
            }
        }
        return(do.call("c", out))
    })
    wpdf <- unlist(phrases, use.names = FALSE)
    phrases <- lapply(phrases, unlist)
    pl <- lapply(
        phrases, function(x) {
            x <- unlist(x, use.names = TRUE)
            x <- x[!is.na(x)]
            return(x)
        })
    pl[vapply(pl, length, double(1)) == 0L] <- NA_character_
    wpdf <- unlist(pl, use.names = FALSE)
    wpdf <- as.data.frame(
        sort(table(wpdf), decreasing = TRUE),
        stringsAsFactors = FALSE)
    names(wpdf) <- c("wordphrase", "n")
    keepers <- pl %>%
        lapply(pertweet, wpdf) %>%
        unlist(use.names = FALSE) %>%
        unique
    wpdf <- wpdf[wpdf$wordphrase %in% keepers, ]
    if (phrase.length == 3L) {
        uqphrase <- strsplit(wpdf$wordphrase, " ")
        divs <- seq(1, phrase.length, 2)
        frnt <- lapply(uqphrase, function(x) x[1:2])
        bck <- lapply(uqphrase, function(x) x[2:3])
        frnt <- vapply(frnt, paste, collapse = " ", character(1))
        bck <- vapply(bck, paste, collapse = " ", character(1))
        wpdf <- wpdf[!duplicated(frnt) &
                     !duplicated(bck), ]
    }
    if (phrase.length == 4L) {
        uqphrase <- strsplit(wpdf$wordphrase, " ")
        divs <- seq(1, phrase.length, 2)
        frnt <- lapply(uqphrase, function(x) x[1:2])
        mdl <- lapply(uqphrase, function(x) x[2:3])
        bck <- lapply(uqphrase, function(x) x[3:4])
        frnt <- vapply(frnt, paste, collapse = " ", character(1))
        mdl <- vapply(mdl, paste, collapse = " ", character(1))
        bck <- vapply(bck, paste, collapse = " ", character(1))
        uqdf <- data.frame(frnt, mdl, bck,
                           stringsAsFactors = FALSE)
        uqdf <- matrix(duplicated(
            c(uqdf$frnt, uqdf$mdl, uqdf$bck)),
                       ncol = 3)
        wpdf <- wpdf[rowSums(uqdf) > 1, ]
    }
    if (phrase.length == 5L) {
        uqphrase <- strsplit(wpdf$wordphrase, " ")
        divs <- seq(1, phrase.length, 2)
        frnt <- lapply(uqphrase, function(x) x[1:2])
        mdl1 <- lapply(uqphrase, function(x) x[2:3])
        mdl2 <- lapply(uqphrase, function(x) x[3:4])
        bck <- lapply(uqphrase, function(x) x[4:5])
        frnt <- vapply(frnt, paste, collapse = " ", character(1))
        mdl1 <- vapply(mdl1, paste, collapse = " ", character(1))
        mdl2 <- vapply(mdl2, paste, collapse = " ", character(1))
        bck <- vapply(bck, paste, collapse = " ", character(1))
        uqdf <- data.frame(frnt, mdl1,
                           mdl2, bck,
                           stringsAsFactors = FALSE)
        uqdf <- matrix(duplicated(
            c(uqdf$frnt, uqdf$mdl1, uqdf$mdl2, uqdf$bck)),
                       ncol = 4)
        wpdf <- wpdf[rowSums(uqdf) > 2, ]
    }
    if (phrase.length == 6L) {
        uqphrase <- strsplit(wpdf$wordphrase, " ")
        divs <- seq(1, phrase.length, 2)
        frnt <- lapply(uqphrase, function(x) x[1:2])
        mdl1 <- lapply(uqphrase, function(x) x[2:3])
        mdl2 <- lapply(uqphrase, function(x) x[3:4])
        mdl3 <- lapply(uqphrase, function(x) x[4:5])
        bck <- lapply(uqphrase, function(x) x[5:6])
        frnt <- vapply(frnt, paste, collapse = " ", character(1))
        mdl1 <- vapply(mdl1, paste, collapse = " ", character(1))
        mdl2 <- vapply(mdl2, paste, collapse = " ", character(1))
        mdl3 <- vapply(mdl3, paste, collapse = " ", character(1))
        bck <- vapply(bck, paste, collapse = " ", character(1))
        uqdf <- data.frame(frnt, mdl1,
                           mdl2, mdl3, bck,
                           stringsAsFactors = FALSE)
        uqdf <- matrix(duplicated(
            c(uqdf$frnt, uqdf$mdl1, uqdf$mdl2, uqdf$mdl3, uqdf$bck)),
                       ncol = 5)
        wpdf <- wpdf[rowSums(uqdf) > 3, ]
    }
    row.names(wpdf) <- NULL
    wpdf
}

pertweet <- function(x, wpdf, max.return = 1L) {
    counts <- wpdf$n[match(x, wpdf$wordphrase)]
    x <- x[order(counts, decreasing = TRUE)]
    x[seq_len(max.return)]
}

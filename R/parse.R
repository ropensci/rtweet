
parser.old <- function(x, n = NULL,
                       return_tweets = TRUE,
                       return_users = TRUE,
                       clean_tweets = FALSE,
                       as_double = FALSE) {

    tweets <- data.frame()
    users <- data.frame()

    if (all(is.data.frame(x), isTRUE("id_str" %in% names(x)))) {
        if (return_tweets) {
            tweets <- parse_tweets(x, clean_tweets = clean_tweets,
                                   as_double = as_double)
        }
        if (return_users) {
            users <- parse_users(x, as_double = as_double)
        }
    } else {
        stopifnot(is.list(x))
        if (return_tweets) {
            tweets <- bply(x, parse_tweets, clean_tweets = clean_tweets,
                           as_double = as_double)
        }
        if (return_users) {
            users <- bply(x, parse_users, as_double = as_double)
        }
    }
    if (return_tweets) {
        if (!is.null(tweets[["status_id"]])) {
            tweets <- tweets[!is.na(tweets$status_id), ]
            ##tweets <- tweets[row.names(unique(tweets[, 1:13])), ]
            row.names(tweets) <- NULL
        }
        tweets <- return_n_rows(tweets, n)
    }
    if (return_users) {
        if (!is.null(users[["user_id"]])) {
                                        #users <- filter_na_rows(users)
            users <- users[!is.na(users$user_id), ]
                                        #users <- unique(users)
        }
        users <- return_n_rows(users, n)
    }
    list(tweets = tweets, users = users)
}

parse_fs <- function(x, n = NULL, as_double = FALSE) {
    if (identical(length(x), 1)) {
        next_cursor <- x[[1]][["next_cursor_str"]]
        if (as_double) {
            x <- as.double(x[[1]][["ids"]])
        } else {
            x <- as.character(x[[1]][["ids"]])
        }
    } else if (all(c("ids", "next_cursor_str") %in% names(x))) {
        next_cursor <- x[["next_cursor_str"]]
        if (as_double) {
            x <- as.double(x[["ids"]])
        } else {
            x <- as.character(x[["ids"]])
        }
    } else if (length(x) > 1) {
        next_cursor <- unlist(lapply(x, function(x)
            x[[1]][["next_cursor_str"]]),
            use.names = FALSE)
        next_cursor <- return_last(next_cursor)
        x <- unlist(lapply(x, function(x) x[[1]][["ids"]]),
                    use.names = FALSE)
        if (as_double) {
            x <- as.double(x)
        } else {
            x <- as.character(x)
        }
    }
    x <- return_n_rows(x, n)
    x <- data.frame(x, stringsAsFactors = FALSE)
    names(x) <- "ids"

    attr(x, "next_cursor") <- next_cursor
    x
}

parse.piper.fs <- function(f, n = NULL, as_double = FALSE) {
    if (as_double) {
        df <- f %>% plyget("ids") %>%
            make.vector() %>%
            as.double() %>%
            as.df("user_id") %>%
            tryCatch(error = function(e) return(NULL))
    } else {
        df <- f %>% plyget("ids") %>%
            make.vector() %>%
            as.character() %>%
            as.df("user_id") %>%
            tryCatch(error = function(e) return(NULL))
    }
    if (is.null(df)) return(f)
    next_cursor <- f %>%
        plyget("next_cursor") %>%
        return_last() %>%
        unL() %>%
        tryCatch(error = function(e) return(NULL))
    if (is.null(next_cursor))  {
        next_cursor <- NA_character_
    }
    attr(df, "next_cursor") <- next_cursor
    if (!is.null(n)) {
        if (n < NROW(df)) df <- df[seq_len(n), ]
    }
    df
}

make.vector <- function(x) do.call("c", x)


plyget <- function(x, f, ...) {
    if (!is.function(f)) {
        if (any(is.data.frame(x),
                f %in% names(x))) return(x[[f]])
        lapply(x, function(x) x[[f]])
    } else if (is.data.frame(x)) {
        f(x, ...)
    } else {
        lapply(x, f, ...)
    }
}


plycp <- function(x, var) {
    itornot <- as.numeric(plyget(x, NROW))
    if (any(identical(length(itornot), 1L),
            !isTRUE(all(itornot[1:2] > 50L)))) {
        plyget(x, pcpr, var)
    } else {
        plyget(x, plyget, pcpr, var)
    }
}
pcpr <- function(x, var) {
    if (!is.data.frame(x)) {
        NA
    } else {
        paste(x[[var]], collapse = " ")
    }
}

getifelse <- function(x, var) {
    if (!is.recursive(x)) {
        return(rep(NA, nrows(x)))
    } else if (is.data.frame(x)) {
        xvar <- x[[var]]
        if (!is.null(xvar)) return(xvar)
    } else {
        xvar <- plyget(x, var)
        xvar[vapply(xvar, length, double(1)) == 0L] <- NA
        return(xvar)
    }
    rep(NA, nrows(x))
}

pastena <- function(x, rev = FALSE) {
    if (is.null(x)) return(NA)
    if (all(is.na(x))) return(x)
    if (rev) x <- rev(x)
    x <- paste(x, collapse = " ")
    x[x %in% c("", "NA", " ")] <- NA
    x
}

unL <- function(x, rec = FALSE) {
    x[vapply(x, length, double(1)) == 0L] <- NA
    x <- unlist(x, use.names = FALSE, recursive = rec)
    x[x == ""] <- NA
    unname(x)
}

plydf <- function(x, var) {
    plyget(plyget(x, getifelse, var), pastena)
}

boxem <- function(x) {
    if (is.array(x)) {
        paste(x, collapse = " ")
    } else {
        NA
    }
}

plyboxem <- function(x) {
    if (!any("array" %in% unL(lapply(x, class)))) {
        lapply(x, function(y) plyget(y, pastena))
    } else {
        plyget(x, boxem)
    }
}

dots <- function(...) {
    as.character(eval(substitute(alist(...))))
}

as.df <- function(x, ...) {
    vars <- dots(...)
    if (identical(vars, "FALSE")) {
        as.data.frame(x, stringsAsFactors = FALSE)
    } else if (identical(length(vars), 0L)) {
        if (is.matrix(x)) {
            x <- lapply(seq_len(ncol(x)), function(i) x[,i])
        }
        vars <- unL(lapply(x, function(x) class(x)[[1]]))
        vars <- abbreviate(vars, 3)
        vars <- mapply(paste0, vars,
                       rep(".", length(vars)),
                       seq_along(vars))
    } else {
        if (any(grepl("^c\\(", vars),
                identical("varnames", vars))) vars <- unL(list(...))
    }
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    names(x) <- vars
    x
}

is.nothing <- function(x = NULL) {
    if (is.null(x)) return(TRUE)
    if (identical(length(x), 0L)) return(TRUE)
    if (all(vapply(x, length, double(1)) == 0L))
        return(TRUE)
    if (all(is.na(unlist(x, recursive = FALSE,
                         use.names = FALSE)))) return(TRUE)
    FALSE
}

is.smth <- function(x) {
    !is.nothing(x)
}

countrows <- function(x) {
    if (!is.data.frame(x)) {
        vapply(x, NROW, double(1))
    } else {
        NROW(x)
    }
}

nrows <- function(x) {
    sum(as.double(
        plyget(x, NROW),
        na.rm = TRUE))
}

iserror <- function(x) {
    x <- tryCatch(x, error = function(e) return(TRUE))
    isTRUE(x)
}


ifelsepipe <- function(x, cond, f = NA) {
    if (is.null(x)) return(rep(f, nrows(x)))
    if (is.character(cond)) {
        if (cond %in% names(x)) {
            return(x[[cond]])
        } else {
            return(rep(f, nrows(x)))
        }
    } else if (cond) {
        return(x)
    }
    rep(f, nrows(x))
}

is.na.not <- function(x) !is.na(x)

#' parse_data
#'
#' Returns Parses tweets and users data
#'
#' @param rt Nested list converted from json structure
#' @param tw Logical indicating whether to include user
#'   obj (users data) as attribute. Defaults to true. If
#'   was generated from users-oriented function, e.g.,
#'   lookup_users(), set to false.
#' @return Data frame of tweets or users data with the other (tweets/users)
#'   included as an attribute. If something breaks during the parsing process,
#'   this function has a fail safe that will return a list (presumably with
#'   unbalanced lengths).
#' @export
parse_data <- function(rt, tw = TRUE) {
    if (tw) {
        parse.piper(rt, usr = TRUE)
    } else {
        parse.piper.usr(rt, tw = TRUE)
    }
}

#' parser
#'
#' Returns Parses tweets and users data
#'
#' @param rt Nested list converted from json structure
#' @param att Logical indicating whether to include user
#'   obj (users data) as attribute if tweets data provided to
#'   rt argument or tweets obj (tweets data) as attribute if
#'   users data provided to rt. Defaults to true.
#' @return Data frame of tweets or users data with the other (tweets/users)
#'   included as an attribute. If something breaks during the parsing process,
#'   this function has a fail safe that will return a list (presumably with
#'   unbalanced lengths).
#' @export
parser <- function(rt, att = TRUE) {
    if (all(c("friends_count", "description") %in% names(rt[[1]]))) {
        parse.piper.usr(rt, tw = att)
    } else {
        parse.piper(rt, usr = att)
    }
}

parse.piper <- function(rt, usr = TRUE) {
    rt <- get.status.obj(rt)
    if ("full_text" %in% names(rt)) {
        names(rt)[names(rt) == "text"] <- "texttrunc"
        names(rt)[names(rt) == "full_text"] <- "text"
    }
    ## if empty return empty df
    if (identical(length(rt), 0L)) {
        if (usr) attr(rt, "users") <- data.frame()
        return(rt)
    }
    if (usr) {
        users <- parse.piper.usr(rt)
        uservars <- list(
            screen_name = users[["screen_name"]],
            user_id = users[["user_id"]])
        if (requireNamespace("tibble", quietly = TRUE)) {
            users <- tryCatch(tibble::as_tibble(users),
                              error = function(e) return(users))
        }
        rt <- c(uservars,
                atomic.parsed(rt),
                entities.parsed(rt),
                place.parsed(rt))
    } else {
        rt <- c(atomic.parsed(rt),
                entities.parsed(rt),
                place.parsed(rt))
    }
    rt <- tryCatch(
        as.data.frame(rt, stringsAsFactors = FALSE),
        error = function(e)
            return(rt))
    if (usr) {
        attr(rt, "users") <- users
    }
    if (requireNamespace("tibble", quietly = TRUE)) {
        rt <- tryCatch(tibble::as_tibble(rt),
                       error = function(e) return(rt))
    }
    rt
}

get.status.obj <- function(x) {
    if (is.null(x)) return(data.frame())
    if (any(isTRUE("statuses" %in% names(x)),
            isTRUE("statuses" %in% names(x[[1]])))) {
        x <- plyget(x, "statuses")
    } else if (any(isTRUE("status" %in% names(x)),
                   isTRUE("status" %in% names(x[[1]])))) {
        x <- plyget(x, "status")
    }
    if (is.null(names(x))) {
        x <- x[!vapply(x, is.null, logical(1))]
    }
    x
}

rtstatus.safecheck <- function(x) {
    if (all(
        isTRUE("retweeted_status" %in% names(x)),
        isTRUE("id_str" %in% names(x[["retweeted_status"]])))
        ) {
        x <- x[["retweeted_status"]]
        return(x[["id_str"]])
    } else if (isTRUE("retweeted_status_id_str" %in% names(x))) {
        return(x[["retweeted_status_id_str"]])
    } else {
        return(rep(NA_character_, NROW(x)))
    }
}
qtstatus.safecheck <- function(x) {
    if (isTRUE("quoted_status_id_str" %in% names(x))) {
        return(x[["quoted_status_id_str"]])
    } else {
        return(rep(NA_character_, NROW(x)))
    }
}
atomic.parsed <- function(rt) {
    list(
        created_at = format_date(unL(plyget(rt, "created_at"))),
        status_id = unL(plyget(rt, "id_str")),
        text = unL(plyget(rt, "text")),
        retweet_count = unL(plyget(rt, "retweet_count")),
        favorite_count = unL(plyget(rt, "favorite_count")),
        is_quote_status = is.na.not(unL(plyget(rt, qtstatus.safecheck))),
        quote_status_id = unL(plyget(rt, qtstatus.safecheck)),
        is_retweet = is.na.not(unL(plyget(rt, rtstatus.safecheck))),
        retweet_status_id = unL(plyget(rt, rtstatus.safecheck)),
        in_reply_to_status_status_id = unL(plyget(rt, "in_reply_to_status_id_str")),
        in_reply_to_status_user_id = unL(plyget(rt, "in_reply_to_user_id_str")),
        in_reply_to_status_screen_name = unL(plyget(rt, "in_reply_to_screen_name")),
        lang = unL(plyget(rt, "lang")),
        source = unL(gsub(
            "^[^>]*>|</a>$", "",
            plyget(rt, "source")))
    )
}

coords.parsed <- function(rt) {
    ## geo coordinates
    coordinates <- tryCatch(
        plyget(rt, "geo") %>%
        plyget("coordinates") %>%
        plyget(unL) %>%
        plyget(paste, collapse = " ") %>%
        unL, error = function(e)
            return(NULL))
    if (!is.null(coordinates)) return(coordinates)
    coordinates <- tryCatch(
        plyget(rt, "coordinates") %>%
        plyget("coordinates") %>%
        plyget(unL) %>%
        plyget(paste, collapse = " ") %>%
        unL, error = function(e)
            return(NULL))
    if (!is.null(coordinates)) return(coordinates)
    rep(NA, nrows(rt))
}

coords.type.parsed <- function(rt) {
    ## geo coordinates
    coordinates_type <- tryCatch(
        plyget(rt, "geo") %>%
        plyget("type") %>%
        plyget(unL) %>%
        plyget(paste, collapse = " ") %>%
        unL, error = function(e)
            return(NULL))
    if (!is.null(coordinates_type)) return(coordinates_type)
    coordinates_type <- tryCatch(
        plyget(rt, "geo") %>%
        plyget("type") %>%
        plyget(unL) %>%
        plyget(paste, collapse = " ") %>%
        unL, error = function(e)
            return(NULL))
    if (!is.null(coordinates_type)) return(coordinates_type)
    rep(NA, nrows(rt))
}

entities.parsed <- function(rt) {
    ## entities
    rt <- plyget(rt, "entities")
    media <- plyget(rt, media.parsed)
    if (is.null(media)) {
        n <- NROW(rt)
        media <- list(
            media_id = rep(NA, n),
            media_url = rep(NA, n),
            media_url_expanded = rep(NA, n)
        )
    }
    urls <- plyget(rt, urls.parsed)
    if (is.null(urls)) {
        n <- NROW(rt)
        urls <- list(
            urls = rep(NA, n),
            urls_display = rep(NA, n),
            urls_expanded = rep(NA, n)
        )
    }
    mentions <- plyget(rt, user_mentions.parsed)
    if (is.null(mentions)) {
        n <- NROW(rt)
        mentions <- list(
            mentions_screen_name = rep(NA, n),
            mentions_user_id = rep(NA, n)
        )
    }
    symbols <- plyget(rt, symbols.parsed)
    if (is.null(symbols)) {
        n <- NROW(rt)
        symbols <- list(
            symbols = rep(NA, n)
        )
    }
    hashtags <- plyget(rt, hashtags.parsed)
    if (is.null(hashtags)) {
        n <- NROW(rt)
        hashtags <- list(
            hashtags = rep(NA, n)
        )
    }

    list(
        media_id = plyget(media, "media_id") %>%
            unL,
        media_url = plyget(media, "media_url") %>%
            unL,
        media_url_expanded = plyget(media, "media_url_expanded") %>%
            unL,
        urls = plyget(urls, "urls") %>%
            unL,
        urls_display = plyget(urls, "urls_display") %>%
            unL,
        urls_expanded = plyget(urls, "urls_expanded") %>%
            unL,
        mentions_screen_name = plyget(mentions, "mentions_screen_name") %>%
            unL,
        mentions_user_id = plyget(mentions, "mentions_user_id") %>%
            unL,
        symbols = plyget(symbols, "symbols") %>%
            unL,
        hashtags = plyget(hashtags, "hashtags") %>%
            unL
    )

}

media.parsed <- function(rt) {
    n <- NROW(rt)
    ## media object
    media <- rt %>% plyget("media")

    ## if null fill media obj with NAs
    ## else extract each media var
    if (is.null(media)) {
        media <- list(
            media_id = rep(NA_character_, n),
            media_url = rep(NA_character_, n),
            media_url_expanded = rep(NA_character_, n)
        )
    } else {
        media <- list(
            media_id = media %>%
                plycp("id_str") %>%
                plyget(unL) %>%
                unL(),
            media_url = media %>%
                plycp("media_url") %>%
                plyget(unL) %>%
                unL(),
            media_url_expanded = media %>%
                plycp("expanded_url") %>%
                plyget(unL) %>%
                unL()
        )
    }
    media
}

urls.parsed <- function(rt) {
    n <- NROW(rt)
    ## urls object
    urls <- rt %>% plyget("urls")

    ## if null fill urls obj with NAs
    ## else extract each urls var
    if (is.null(urls)) {
        urls <- list(
            urls = rep(NA_character_, n),
            urls_display = rep(NA_character_, n),
            urls_expanded = rep(NA_character_, n)
        )
    } else {
        urls <- list(
            urls = plycp(urls, "urls") %>%
                plyget(unL) %>%
                unL(),
            urls_display = plycp(urls, "display_url") %>%
                plyget(unL) %>%
                unL(),
            urls_expanded = plycp(urls, "expanded_url") %>%
                plyget(unL) %>%
                unL()
        )
    }
    urls
}

user_mentions.parsed <- function(rt) {
    n <- NROW(rt)
    ## user_mentions object
    user_mentions <- plyget(rt, "user_mentions")

    ## if null fill user_mentions obj with NAs
    ## else extract each user_mentions var
    if (is.null(user_mentions)) {
        user_mentions <- list(
            mentions_screen_name = rep(NA_character_, n),
            mentions_user_id = rep(NA_character_, n)
        )
    } else {
        user_mentions <- list(
            mentions_screen_name = user_mentions %>%
                plycp("screen_name") %>%
                plyget(unL) %>%
                unL(),
            mentions_user_id = user_mentions %>%
                plycp("id_str") %>%
                plyget(unL) %>%
                unL()
        )
    }
    user_mentions
}

symbols.parsed <- function(rt) {
    n <- NROW(rt)
    ## symbols object
    symbols <- plyget(rt, "symbols")

    ## if null fill symbols obj with NAs
    ## else extract each symbols var
    if (is.null(symbols)) {
        symbols <- list(
            symbols = rep(NA_character_, n)
        )
    } else {
        symbols <- list(
            symbols = symbols %>%
                plycp("text") %>%
                plyget(unL) %>%
                unL()
        )
    }
    symbols
}

hashtags.parsed <- function(rt) {
    n <- NROW(rt)
    ## hashtags object
    hashtags <- plyget(rt, "hashtags")

    ## if null fill hashtags obj with NAs
    ## else extract each hashtags var
    if (is.null(hashtags)) {
        hashtags <- list(
            hashtags = rep(NA_character_, n)
        )
    } else {
        hashtags <- list(
            hashtags = hashtags %>%
                plycp("text") %>%
                plyget(unL) %>%
                unL()
        )
    }
    hashtags
}


place.parsed <- function(rt) {
    coordinates <- coords.parsed(rt)
    rt <- plyget(rt, "place")
    list(
        coordinates = coordinates,
        place_id = plyget(rt, place_id.parsed) %>%
            unL,
        place_type = plyget(rt, place_type.parsed) %>%
            unL,
        place_name = plyget(rt, place_name.parsed) %>%
            unL,
        place_full_name = plyget(rt, place_full_name.parsed) %>%
            unL,
        country_code = plyget(rt, country_code.parsed) %>%
            unL,
        country = plyget(rt, country.parsed) %>%
            unL,
        bounding_box_coordinates = plyget(rt, bounding_box_coordinates.parsed) %>%
            unL,
        bounding_box_type = plyget(rt, bounding_box_type.parsed) %>%
            unL)
}

place_id.parsed <- function(rt) {
    plyget(rt, getifelse, "id") %>%
        plyget(unL) %>%
        unL()
}
place_type.parsed <- function(rt) {
    plyget(rt, getifelse, "place_type") %>%
        plyget(unL) %>%
        unL()
}
place_name.parsed <- function(rt) {
    plyget(rt, getifelse, "name") %>%
        plyget(unL) %>%
        unL()
}
place_full_name.parsed <- function(rt) {
    plyget(rt, getifelse, "full_name") %>%
        plyget(unL) %>%
        unL()
}
country_code.parsed <- function(rt) {
    plyget(rt, getifelse, "country_code") %>%
        plyget(unL) %>%
        unL()
}
country.parsed <- function(rt) {
    plyget(rt, getifelse, "country") %>%
        plyget(unL) %>%
        unL()
}
bounding_box_coordinates.parsed <- function(rt) {
    plyget(rt, getifelse, "bounding_box") %>%
        plyget(getifelse, "coordinates") %>%
        plyboxem %>%
        plyget(unL) %>%
        unL()
}
bounding_box_type.parsed <- function(rt) {
    plyget(rt, getifelse, "bounding_box") %>%
        plyget(getifelse, "type") %>%
        plyget(unL) %>%
        unL()
}

get.user.obj <- function(x) {
    if (any("user" %in% names(x),
            "user" %in% names(x[[1]]))) {
        x <- plyget(x, "user")
    }
    if (is.null(names(x))) {
        x <- x[!vapply(x, is.null, logical(1))]
    }
    x
}
atomic.parsed.usr <- function(rt) {
    atom <- list(
        user_id = unL(plyget(rt, getifelse, "id_str")),
        name = unL(plyget(rt, getifelse, "name")),
        screen_name = unL(plyget(rt, getifelse, "screen_name")),
        location = unL(plyget(rt, getifelse, "location")),
        description = unL(plyget(rt, getifelse, "description")),
        protected = unL(plyget(rt, getifelse, "protected")),
        followers_count = unL(plyget(rt, getifelse, "followers_count")),
        friends_count = unL(plyget(rt, getifelse, "friends_count")),
        listed_count = unL(plyget(rt, getifelse, "listed_count")),
        created_at = format_date(unL(plyget(rt, getifelse, "created_at"))),
        favourites_count = unL(plyget(rt, getifelse, "favourites_count")),
        utc_offset = unL(plyget(rt, getifelse, "utc_offset")),
        time_zone = unL(plyget(rt, getifelse, "time_zone")),
        geo_enabled = unL(plyget(rt, getifelse, "geo_enabled")),
        verified = unL(plyget(rt, getifelse, "verified")),
        statuses_count = unL(plyget(rt, getifelse, "statuses_count")),
        lang = unL(plyget(rt, getifelse, "lang")),
        contributors_enabled = unL(plyget(rt, getifelse, "contributors_enabled")),
        is_translator = unL(plyget(rt, getifelse, "is_translator")),
        is_translation_enabled = unL(plyget(rt, getifelse, "is_translation_enabled")),
        profile_background_color = unL(plyget(rt, getifelse, "profile_background_color")),
        profile_background_image_url = unL(plyget(rt, getifelse, "profile_background_image_url")),
        profile_background_image_url_https = unL(plyget(rt, getifelse, "profile_background_image_url_https")),
        profile_background_tile = unL(plyget(rt, getifelse, "profile_background_tile")),
        profile_image_url = unL(plyget(rt, getifelse, "profile_image_url")),
        profile_image_url_https = unL(plyget(rt, getifelse, "profile_image_url_https")),
        profile_image_url = unL(plyget(rt, getifelse, "profile_image_url")),
        profile_image_url_https = unL(plyget(rt, getifelse, "profile_image_url_https")),
        profile_link_color = unL(plyget(rt, getifelse, "profile_link_color")),
        profile_sidebar_border_color = unL(plyget(rt, getifelse, "profile_sidebar_border_color")),
        profile_sidebar_fill_color = unL(plyget(rt, getifelse, "profile_sidebar_fill_color")),
        profile_text_color = unL(plyget(rt, getifelse, "profile_text_color")),
        profile_use_background_image = unL(plyget(rt, getifelse, "profile_use_background_image")),
        default_profile = unL(plyget(rt, getifelse, "default_profile")),
        default_profile_image = unL(plyget(rt, getifelse, "default_profile_image")),
        profile_banner_url = unL(plyget(rt, getifelse, "profile_banner_url"))
    )
    lgs <- vapply(atom, length, double(1))
    if (sum(lgs == 0, na.rm = TRUE) > 0L) {
        for (i in seq_len(sum(lgs == 0))) {
            atom[[i]] <- rep(rep(NA, max(lgs)))
        }
    }
    atom[lgs == 0] <- rep(
        list(rep(NA, max(lgs))), sum(lgs == 0))
    atom
}


#' parse.piper.usr
#'
#' Returns users data parsed via pipe
#'
#' @param rt Nested list converted from json structure
#' @param tw Logical indicating whether to include status
#'   obj (tweets data) as attribute. Defaults to false.
#' @noRd
parse.piper.usr <- function(rt, tw = FALSE) {
    rt <- get.user.obj(rt)
    if (tw) {
        tweets <- parse.piper(rt, usr = FALSE)
    }
    rt <- atomic.parsed.usr(rt)
    rt <- tryCatch(
        as.data.frame(rt, stringsAsFactors = FALSE),
        error = function(e) return(rt))
    if (requireNamespace("tibble", quietly = TRUE)) {
        rt <- tryCatch(tibble::as_tibble(rt),
                          error = function(e) return(rt))
    }
    if (tw) {
        if (requireNamespace("tibble", quietly = TRUE)) {
            tweets <- tryCatch(tibble::as_tibble(tweets),
                              error = function(e) return(tweets))
        }
        attr(rt, "tweets") <- tweets
    }
    rt
}

make.pcp <- function(x) {
    attr(x, "lst") <- tidy.pcp(x)
    x
}

tidy.pcp <- function(x) {
    x <- strsplit(x, " ")
    lapply(x, tolower)
}

pop.pcp <- function(x, ...) {
    x <- x %>%
        tidy.pcp %>%
        unlist(use.names = FALSE) %>%
        table %>%
        as.df(c("variable", "value"))
    x <- x[order(x$value, decreasing = TRUE), ]
    row.names(x) <- NULL
    x
}

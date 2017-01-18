
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
    rt
}
x <- list(list(a = TRUE))

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
        created_at = rt %>%
            plyget("created_at") %>%
            unL %>%
            format_date,
        status_id = rt %>%
            plyget("id_str") %>%
            unL,
        text = rt %>%
            plyget("text") %>%
            unL,
        retweet_count = rt %>%
            plyget("retweet_count") %>%
            unL,
        favorite_count = rt %>%
            plyget("favorite_count") %>%
            unL,
        is_quote_status = rt %>%
            plyget(qtstatus.safecheck) %>%
            unL %>%
            is.na.not,
        quote_status_id = rt %>%
            plyget(qtstatus.safecheck) %>%
            unL,
        is_retweet = rt %>%
            plyget(rtstatus.safecheck) %>%
            unL %>%
            is.na.not,
        retweet_status_id = rt %>%
            plyget(rtstatus.safecheck) %>%
            unL,
        in_reply_to_status_status_id = rt %>%
            plyget("in_reply_to_status_id_str") %>%
            unL,
        in_reply_to_status_user_id = rt %>%
            plyget("in_reply_to_user_id_str") %>%
            unL,
        in_reply_to_status_screen_name = rt %>%
            plyget("in_reply_to_screen_name") %>%
            unL,
        lang = rt %>%
            plyget("lang") %>%
            unL,
        source = gsub(
            "^[^>]*>|</a>$", "",
            plyget(rt, "source") %>%
            unL)
    )
}

coords.parsed <- function(rt) {
    ## geo coordinates
    coordinates <- tryCatch(
        rt %>%
        plyget("geo") %>%
        plyget("coordinates") %>%
        plyget(unL) %>%
        plyget(paste, collapse = " ") %>%
        unL, error = function(e)
            return(NULL))
    if (!is.null(coordinates)) return(coordinates)
    coordinates <- tryCatch(
        rt %>%
        plyget("coordinates") %>%
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
        rt %>%
        plyget("geo") %>%
        plyget("type") %>%
        plyget(unL) %>%
        plyget(paste, collapse = " ") %>%
        unL, error = function(e)
            return(NULL))
    if (!is.null(coordinates_type)) return(coordinates_type)
    coordinates_type <- tryCatch(
        rt %>%
        plyget("geo") %>%
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
    media <- rt %>%
        plyget(media.parsed)
    if (is.null(media)) {
        n <- NROW(rt)
        media <- list(
            media_id = rep(NA, n),
            media_url = rep(NA, n),
            media_url_expanded = rep(NA, n)
        )
    }
    urls <- rt %>%
        plyget(urls.parsed)
    if (is.null(urls)) {
        n <- NROW(rt)
        urls <- list(
            urls = rep(NA, n),
            urls_display = rep(NA, n),
            urls_expanded = rep(NA, n)
        )
    }
    mentions <- rt %>%
        plyget(user_mentions.parsed)
    if (is.null(mentions)) {
        n <- NROW(rt)
        mentions <- list(
            mentions_screen_name = rep(NA, n),
            mentions_user_id = rep(NA, n)
        )
    }
    symbols <- rt %>%
        plyget(symbols.parsed)
    if (is.null(symbols)) {
        n <- NROW(rt)
        symbols <- list(
            symbols = rep(NA, n)
        )
    }
    hashtags <- rt %>%
        plyget(hashtags.parsed)
    if (is.null(hashtags)) {
        n <- NROW(rt)
        hashtags <- list(
            hashtags = rep(NA, n)
        )
    }
    list(
        media_id = media %>%
            plyget("media_id") %>%
            unL,
        media_url = media %>%
            plyget("media_url") %>%
            unL,
        media_url_expanded = media %>%
            plyget("media_url_expanded") %>%
            unL,
        urls = urls %>%
            plyget("urls") %>%
            unL,
        urls_display = urls %>%
            plyget("urls_display") %>%
            unL,
        urls_expanded = urls %>%
            plyget("urls_expanded") %>%
            unL,
        mentions_screen_name = mentions %>%
            plyget("mentions_screen_name") %>%
            unL,
        mentions_user_id = mentions %>%
            plyget("mentions_user_id") %>%
            unL,
        symbols = symbols %>%
            plyget("symbols") %>%
            unL,
        hashtags = hashtags %>%
            plyget("hashtags") %>%
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
            urls = urls %>%
                plycp("urls") %>%
                plyget(unL) %>%
                unL(),
            urls_display = urls %>%
                plycp("display_url") %>%
                plyget(unL) %>%
                unL(),
            urls_expanded = urls %>%
                plycp("expanded_url") %>%
                plyget(unL) %>%
                unL()
        )
    }
    urls
}

user_mentions.parsed <- function(rt) {
    n <- NROW(rt)
    ## user_mentions object
    user_mentions <- rt %>%
        plyget("user_mentions")

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
    symbols <- rt %>%
        plyget("symbols")

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
    hashtags <- rt %>%
        plyget("hashtags")

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
        place_id = rt %>%
            plyget(place_id.parsed) %>%
            unL,
        place_type = rt %>%
            plyget(place_type.parsed) %>%
            unL,
        place_name = rt %>%
            plyget(place_name.parsed) %>%
            unL,
        place_full_name = rt %>%
            plyget(place_full_name.parsed) %>%
            unL,
        country_code = rt %>%
            plyget(country_code.parsed) %>%
            unL,
        country = rt %>%
            plyget(country.parsed) %>%
            unL,
        bounding_box_coordinates = rt %>%
            plyget(bounding_box_coordinates.parsed) %>%
            unL,
        bounding_box_type = rt %>%
            plyget(bounding_box_type.parsed) %>%
            unL)
}

place_id.parsed <- function(rt) {
    rt %>%
        plyget(getifelse, "id") %>%
        plyget(unL) %>%
        unL()
}
place_type.parsed <- function(rt) {
    rt %>%
        plyget(getifelse, "place_type") %>%
        plyget(unL) %>%
        unL()
}
place_name.parsed <- function(rt) {
    rt %>%
        plyget(getifelse, "name") %>%
        plyget(unL) %>%
        unL()
}
place_full_name.parsed <- function(rt) {
    rt %>%
        plyget(getifelse, "full_name") %>%
        plyget(unL) %>%
        unL()
}
country_code.parsed <- function(rt) {
    rt %>%
        plyget(getifelse, "country_code") %>%
        plyget(unL) %>%
        unL()
}
country.parsed <- function(rt) {
    rt %>%
        plyget(getifelse, "country") %>%
        plyget(unL) %>%
        unL()
}
bounding_box_coordinates.parsed <- function(rt) {
    rt %>%
        plyget(getifelse, "bounding_box") %>%
        plyget(getifelse, "coordinates") %>%
        plyboxem %>%
        plyget(unL) %>%
        unL()
}
bounding_box_type.parsed <- function(rt) {
    rt %>%
        plyget(getifelse, "bounding_box") %>%
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
        user_id = rt %>%
            plyget("id_str") %>%
            unL,
        name = rt %>%
            plyget("name") %>%
            unL,
        screen_name = rt %>%
            plyget("screen_name") %>%
            unL,
        location = rt %>%
            plyget("location") %>%
            unL,
        description = rt %>%
            plyget("description") %>%
            unL,
        protected = rt %>%
            plyget("protected") %>%
            unL,
        followers_count = rt %>%
            plyget("followers_count") %>%
            unL,
        friends_count = rt %>%
            plyget("friends_count") %>%
            unL,
        listed_count = rt %>%
            plyget("listed_count") %>%
            unL,
        created_at = rt %>%
            plyget("created_at") %>%
            unL %>%
            format_date,
        favourites_count = rt %>%
            plyget("favourites_count") %>%
            unL,
        utc_offset = rt %>%
            plyget("utc_offset") %>%
            unL,
        time_zone = rt %>%
            plyget("time_zone") %>%
            unL,
        geo_enabled = rt %>%
            plyget("geo_enabled") %>%
            unL,
        verified = rt %>%
            plyget("verified") %>%
            unL,
        statuses_count = rt %>%
            plyget("statuses_count") %>%
            unL,
        lang = rt %>%
            plyget("lang") %>%
            unL,
        contributors_enabled = rt %>%
            plyget("contributors_enabled") %>%
            unL,
        is_translator = rt %>%
            plyget("is_translator") %>%
            unL,
        is_translation_enabled = rt %>%
            plyget("is_translation_enabled") %>%
            unL,
        profile_background_color = rt %>%
            plyget("profile_background_color") %>%
            unL,
        profile_background_image_url = rt %>%
            plyget("profile_background_image_url") %>%
            unL,
        profile_background_image_url_https = rt %>%
            plyget("profile_background_image_url_https") %>%
            unL,
        profile_background_tile = rt %>%
            plyget("profile_background_tile") %>%
            unL,
        profile_image_url = rt %>%
            plyget("profile_image_url") %>%
            unL,
        profile_image_url_https = rt %>%
            plyget("profile_image_url_https") %>%
            unL,
        profile_image_url = rt %>%
            plyget("profile_image_url") %>%
            unL,
        profile_image_url_https = rt %>%
            plyget("profile_image_url_https") %>%
            unL,
        profile_link_color = rt %>%
            plyget("profile_link_color") %>%
            unL,
        profile_sidebar_border_color = rt %>%
            plyget("profile_sidebar_border_color") %>%
            unL,
        profile_sidebar_fill_color = rt %>%
            plyget("profile_sidebar_fill_color") %>%
            unL,
        profile_text_color = rt %>%
            plyget("profile_text_color") %>%
            unL,
        profile_use_background_image = rt %>%
            plyget("profile_use_background_image") %>%
            unL,
        default_profile = rt %>%
            plyget("default_profile") %>%
            unL,
        default_profile_image = rt %>%
            plyget("default_profile_image") %>%
            unL,
        profile_banner_url = rt %>%
            plyget("profile_banner_url") %>%
            unL
    )
    lgs <- vapply(atom, length, double(1))
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
    if (tw) {
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

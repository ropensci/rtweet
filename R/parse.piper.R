
## utility functions
## map
plyget <- function(., f, ...) {
    if (is.atomic(f)) {
        if (identical(length(f), 1L)) {
            if (is.data.frame(.)) return(.[[f]])
            lapply(., function(x) x[[f]])
        } else {
            if (is.data.frame(.)) return(.[f])
            lapply(., function(x) x[f])
        }
    } else {
        if (is.data.frame(.)) return(f(., ...))
        lapply(., f, ...)
    }
}

## get if else NA
getifelse <- function(x, var) {
    if (!is.null(names(x))) {
        x[[var]]
    } else {
        if (is.null(x)) x <- NA
        x
    }
}

## collapse and set empty to NA
pastena <- function(x, rev = FALSE) {
    if (is.null(x)) return(NA)
    if (all(is.na(x))) return(x)
    if (rev) x <- rev(x)
    x <- paste(x, collapse = ",")
    x[x == ""] <- NA
    x
}

## fast unlist
unL <- function(x) {
    x[vapply(x, is.null, logical(1))] <- NA
    x[vapply(x, length, double(1)) == 0L] <- NA
    x <- unlist(x, use.names = FALSE)
    x[vapply(x, function(x) identical(x, ""),
             logical(1))] <- NA
    x
}

## map df
plydf <- function(., var) {
    plyget(plyget(., getifelse, var), pastena)
}

## unbox place coords
boxem <- function(x) {
    if (is.array(x)) {
        paste0(x, collapse = ",")
    } else {
        NA
    }
}

## apply boxem to various conditions
plyboxem <- function(x) {
    if (!any("array" %in% unL(lapply(x, class)))) {
        lapply(x, function(y) plyget(y, pastena))
    } else {
        plyget(x, boxem)
    }
}

## make df var reader
dots <- function(...) {
    as.character(eval(substitute(alist(...))))
}

## easy as df
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
        structure(as.data.frame(
            x, stringsAsFactors = FALSE), names = vars)
    } else {
        if (any(grepl("^c\\(", vars),
                identical("varnames", vars))) vars <- unL(list(...))
        structure(as.data.frame(
            x, stringsAsFactors = FALSE), names = vars)
    }
}

## is null or empty
is.nothing <- function(x) {
    if (is.null(x)) return(TRUE)
    if (identical(length(x), 0L)) return(TRUE)
    if (all(vapply(x, length, double(1)) == 0L))
        return(TRUE)
    if (all(is.na(unL(x)))) return(TRUE)
    FALSE
}

## neither null nor empty
is.smth <- function(x) {
    !is.nothing(x)
}

## count observations
countrows <- function(x) {
    if (!is.data.frame(x)) {
        vapply(x, NROW, double(1))
    } else {
        NROW(x)
    }
}

## apply countrows
nrows <- function(x) {
    sum(unlist(
        plyget(x, countrows),
        use.names = FALSE)
        )
}

iserror <- function(x) {
    x <- tryCatch(x, error = function(e) return(NULL))
    if (is.null(x)) return(TRUE)
    FALSE
}

    
## safe pipe
ifelsepipe <- function(., cond, f = NA) {
    if (is.nothing(.)) return(rep(f, nrows(.)))
    if (is.character(cond)) {
        if (iserror(.[[cond]])) return(rep(f, nrows(.)))
        if (is.smth(.[[cond]])) {
            return(.[[cond]])
        }
    } else if (cond) {
        return(.)
    }
    rep(f, nrows(.))
}

## invert is.na
is.na.not <- function(x) !is.na(x)

#################################
#################################
## actual parsing


## parse using the pipe
#' parse.piper
#'
#' Returns tweets data parsed via pipe
#'
#' @param rt Nested list converted from json structure
#' @param usr Logical indicating whether to include user
#'   obj (users data) as attribute. Defaults to true.
#' @export
parse.piper <- function(rt, usr = TRUE) {
    rt <- get.status.obj(rt)
    if (usr) {
        users <- parse.piper.usr(rt)
    }
    rt <- c(atomic.parsed(rt),
      entities.parsed(rt),
      place.parsed(rt))
    varnames <- names(rt)
    rt <- as.df(rt, varnames)
    if (usr) {
        attr(rt, "users") <- users
    }
    rt
}


## reduce to statuses
get.status.obj <- function(x) {
    if (any("statuses" %in% names(x),
            "statuses" %in% names(x[[1]]))) {
        x <- plyget(x, "statuses")
    } else if (any("status" %in% names(x),
                   "status" %in% names(x[[1]]))) {
        x <- plyget(x, "status")
    }
    if (is.null(names(x))) {
        x <- x[!vapply(x, is.null, logical(1))]
    }
    x
}

## nonrecursive variables
atomic.parsed <- function(rt) {
    list(
        created_at = rt %>%
            plyget("created_at") %>%
            unL,
        user_id = rt %>%
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
            plyget(ifelsepipe, "is_quote_status", FALSE) %>%
            unL,
        quote_status_id = rt %>%
            plyget(ifelsepipe, "quoted_status", FALSE) %>%
            plyget(ifelsepipe, "id_str", NA) %>%
            unL,
        is_retweet = rt %>%
            plyget(ifelsepipe, "retweeted_status", FALSE) %>%
            plyget(ifelsepipe, "id_str", NA) %>% 
            unL %>% is.na.not,
        retweet_status_id = rt %>%
            plyget(ifelsepipe, "retweeted_status", FALSE) %>%
            plyget(ifelsepipe, "id_str", NA) %>%
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
            rt %>%
            plyget("source") %>%
            unL)
    )
}

## get coords (from 1 of 2 sources)
coords.parsed <- function(rt) {
    ## geo coordinates
    coordinates <- tryCatch(rt %>%
        plyget("geo") %>%
        plyget(plydf, "coordinates") %>%
        plyget(unL) %>%
        unL, error = function(e)
            return(NULL))
    if (!is.null(coordinates)) return(coordinates)
    coordinates <- tryCatch(rt %>%
        plyget("coordinates") %>%
        plyget(plydf, "coordinates") %>%
        plyget(unL) %>%
        unL, error = function(e)
            return(NULL))
    if (!is.null(coordinates)) return(coordinates)
    rep(NA, nrows(rt))
}

entities.parsed <- function(rt) {
    ## entities
    rt <- plyget(rt, "entities")
    list(
        entities.media.id = rt %>%
            plyget(getifelse, "media") %>% 
            plyget(plydf, "id_str") %>%
            unL,
        entities.media.media_url = rt %>%
            plyget(getifelse, "media") %>%
            plyget(plydf, "media_url") %>%
            unL,
        entities.media.expanded_url = rt %>%
            plyget(getifelse, "media") %>%
            plyget(plydf, "expanded_url") %>%
            unL,
        entities.urls.url = rt %>%
            plyget(getifelse, "urls") %>% 
            plyget(plydf, "url") %>%
            plyget(unL) %>%
            unL,
        entities.urls.display_url = rt %>%
            plyget(getifelse, "urls") %>%
            plyget(plydf, "display_url") %>%
            plyget(unL) %>%
            unL,
        entities.urls.expanded_url = rt %>%
            plyget(getifelse, "urls") %>%
            plyget(plydf, "expanded_url") %>%
            plyget(unL) %>%
            unL,
        entities.urls.display_url = rt %>%
            plyget(getifelse, "user_mentions") %>%
            plyget(plydf, "screen_name") %>%
            plyget(unL) %>%
            unL,
        entities.user_mentions.user_id = rt %>%
            plyget(getifelse, "user_mentions") %>%
            plyget(plydf, "id_str") %>%
            plyget(unL) %>%
            unL,
        entities.symbols.text = rt %>%
            plyget(getifelse, "symbols") %>%
            plyget(plydf, "text") %>%
            plyget(unL) %>%
            unL,
        entities.hashtags.text = rt %>%
            plyget(getifelse, "hashtags") %>%
            plyget(plydf, "text") %>%
            plyget(unL) %>%
            unL)
}

## place obj
place.parsed <- function(rt) {
    coordinates <- coords.parsed(rt)
    rt <- rt %>% plyget("place")
    list(
        coordinates = coordinates,
        place.id = rt %>%
            plyget(getifelse, "id") %>%
            unL,
        place.place_type= rt %>%
            plyget(getifelse, "place_type") %>%
            unL,
        place.name = rt %>%
            plyget(getifelse, "name") %>%
            unL,
        place.full_name = rt %>%
            plyget(getifelse, "full_name") %>%
            unL,
        place.country_code = rt %>%
            plyget(getifelse, "country_code") %>%
            unL,
        place.country = rt %>%
            plyget(getifelse, "country") %>%
            unL,
        place.bounding_box.coordinates = rt %>%
            plyget(getifelse, "bounding_box") %>%
            plyget(getifelse, "coordinates") %>%
            plyboxem %>%
            unL,
        place.bounding_box.type = rt %>%
            plyget(getifelse, "bounding_box") %>%
            plyget(getifelse, "type") %>%
            unL)
}

## reduce to statuses
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
    list(
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
            unL,
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
        has_extended_profile = rt %>%
            plyget("has_extended_profile") %>%
            unL,
        default_profile = rt %>%
            plyget("default_profile") %>%
            unL,
        default_profile_image = rt %>%
            plyget("default_profile_image") %>%
            unL,
        profile_banner_url = rt %>%
            plyget("profile_banner_url") %>%
            unL,
        shortened_url = rt %>%
            plyget("entities") %>%
            plyget("url") %>%
            plyget("urls") %>%
            plyget(plydf, "url") %>%
            unL,
        expanded_url = rt %>%
            plyget("entities") %>%
            plyget("url") %>%
            plyget("urls") %>%
            plyget(plydf, "expanded_url") %>%
            unL
        )
}

#' parse.piper.usr
#'
#' Returns users data parsed via pipe
#'
#' @param rt Nested list converted from json structure
#' @param tw Logical indicating whether to include status
#'   obj (tweets data) as attribute. Defaults to false.
#' @export
parse.piper.usr <- function(rt, tw = FALSE) {
    rt <- get.user.obj(rt)
    if (tw) {
        tweets <- parse.piper(rt, usr = FALSE)
    }
    rt <- atomic.parsed.usr(rt)
    varnames <- names(rt)
    rt <- as.df(rt, varnames)
    if (tw) {
        attr(rt, "tweets") <- tweets
    }
    rt
}


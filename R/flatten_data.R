
atomics <- function(x, atomics = TRUE) {
  if (atomics) {
    x[vapply(x, is.atomic, logical(1))]
  } else {
    x[!vapply(x, is.atomic, logical(1))]
  }
}

parse.data.frame <- function(.data) {
  atms <- atomics(.data)
  recs <- atomics(.data, FALSE)
  attr(atms, "recursives") <- recs
  atms
}

tblnames <- function(x, n = 10) {
  if (sign(n) == 1) {
    sort(table(unlist(lapply(tml, names))),
         decreasing = TRUE)[seq_len(n)]
  } else {
    sort(table(unlist(lapply(tml, names))))[rev(seq_len(abs(n)))]
  }
}

cleanupunlistdataframers <- function(x) {
  x <- unlist(x)
  x <- x[names(x) != ""]
  nms <- gsub("[[:digit:]]{1,}$", "", names(x))
  nmstb <- table(nms)
  dup <- names(nmstb[nmstb > 1L])
  y1 <- x[!nms %in% dup]
  y2 <- vapply(dup, function(i)
    paste(x[!nms %in% i], collapse = " "),
    character(1))
  c(y1, y2)
}

flatten_data <- function(x) {
  if (is.data.frame(x)) {
    all_parse(x)
  } else {
    x <- lapply(x, all_parse)
    nms <- table(unlist(lapply(x, names)))
    if (length(unique(nms)) > 1L) {
      kprs <- names(nms[nms == max(nms, na.rm = TRUE)])
      x <- lapply(x, function(i) return(i[names(i) %in% kprs]))
    }
    do.call("rbind", x)
  }
}

    
all_parse <- function(x) {
  if (length(x) == 1 && !is.data.frame(x) && is.recursie(x)) {
    x <- x[[1]]
  }
  if (!is.data.frame(x) && grepl("^status", names(x))) {
    x <- x[[grep("^status", names(x))]]
  }
  if (!is.data.frame(x)) {
    x <- tryCatch(as.data.frame(x), function(e) error = return(NULL))
    if (is.null(x)) return(data.frame())
  }
  x <- lapply(
    seq_len(nrow(x)),
    function(i) cleanupunlistdataframers(x[i, ])
  )
  uqs <- unique(unlist(lapply(x, names)))
  uqs <- uqs[uqs != ""]
  kpall <- structure(
    rep(NA, length(uqs)),
    names = uqs
  )
  x <- lapply(x, function(z) {
    z <- c(z, kpall[!names(kpall) %in% names(z)])
    z <- z[names(z) != ""]
    z <- z[order(names(z))]
    z
  })
  x <- tbl_df(do.call("rbind", x))
  lgl <- apply(x[, ], 2, function(i) all(i %in% c(TRUE, FALSE, NA)))
  x[, lgl] <- lapply(x[, lgl], as.logical)
  x[grep("count$", names(x))] <- lapply(
    x[grep("count$", names(x))], as.integer)
  x[grep("created_at$", names(x))] <- lapply(
    x[grep("created_at$", names(x))], as.POSIXct,
    format = "%a %b %d %T %z %Y", tz = "UTC")
  x
}

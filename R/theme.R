#' @import ggplot2
theme_rt <- function (base_size = 10,
                      base_family = "Arial Narrow",
                      fill = "#f0f0f0",
                      col = "#fafafa",
                      ...) {

    half_line <- base_size/2
    theme(line = element_line(
              colour = "#333333", size = 0.3,
              linetype = 1, lineend = "square"),
          rect = element_rect(
              fill = fill, colour = "#333333",
              size = 0.3, linetype = 1),
          text = element_text(family = base_family,
                              face = "plain", colour = "#333333",
                              size = base_size, lineheight = 0.9,
                              hjust = 0.5, vjust = 0.5, angle = 0,
                              margin = margin(),
                              debug = FALSE),
          axis.line = element_blank(),
          axis.line.x = NULL,
          axis.line.y = NULL,
          axis.text = element_text(
              size = rel(0.8), colour = "grey30"),
          axis.text.x = element_text(
              margin = margin(
                  t = 1.0 * half_line/2), vjust = 1),
          axis.text.x.top = element_text(
              margin = margin(
                  b = 1.0 * half_line/2), vjust = 0),
          axis.text.y = element_text(
              margin = margin(
                  r = 1.0 * half_line/2), hjust = 1),
          axis.text.y.right = element_text(
              margin = margin(
                  l = 1.0 * half_line/2), hjust = 0),
          axis.ticks = element_line(
              colour = "#666666", size = .5),
          axis.ticks.length = unit(half_line/3, "pt"),
          axis.title.x = element_text(
              margin = margin(
                  t = half_line * 1.5), vjust = 1),
          axis.title.x.top = element_text(
              margin = margin(
                  b = half_line), vjust = 0),
          axis.title.y = element_text(
              angle = 90, margin = margin(
                              r = half_line), vjust = 1),
          axis.title.y.right = element_text(
              angle = -90, margin = margin(
                               l = half_line), vjust = 0),
          legend.background = element_rect(
              colour = NA, fill = fill),
          legend.spacing = unit(0.2, "cm"),
          legend.spacing.x = NULL,
          legend.spacing.y = NULL,
          legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
          legend.key = element_rect(
              fill = fill, colour = col, size = .25),
          legend.key.size = unit(1.35, "lines"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(0.8)),
          legend.text.align = NULL,
          legend.title = element_blank(),
          legend.title.align = NULL,
          legend.position = "right",
          legend.direction = NULL,
          legend.justification = "center",
          legend.box = NULL,
          legend.box.margin = margin(0, 0, 0, 0, "cm"),
          legend.box.background = element_blank(),
          legend.box.spacing = unit(0.25, "cm"),
          panel.background = element_rect(
              fill = fill, colour = NA, size = .5),
          panel.border = element_blank(),
          panel.grid.major = element_line(
              colour = col, linetype = 1, size = 4),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(half_line * .5, "pt"),
          panel.spacing.x = NULL,
          panel.spacing.y = NULL,
          panel.ontop = FALSE,
          strip.background = element_rect(
              fill = "grey85", colour = NA),
          strip.text = element_text(
              colour = "#333333", size = rel(0.8)),
          strip.text.x = element_text(
              margin = margin(t = half_line, b = half_line)),
          strip.text.y = element_text(
              angle = -90, margin = margin(
                               l = half_line, r = half_line)),
          strip.placement = "inside",
          strip.placement.x = NULL,
          strip.placement.y = NULL,
          strip.switch.pad.grid = unit(0.1, "cm"),
          strip.switch.pad.wrap = unit(0.1, "cm"),
          plot.background = element_rect(
              fill = fill, colour = NA),
          plot.title = element_text(
              size = rel(1.75), hjust = 0,
              vjust = 1, margin = margin(
                             t = half_line * .25,
                             b = half_line * 1.75)),
          plot.subtitle = element_text(
              size = rel(0.9), hjust = 0,
              vjust = 1, margin = margin(
                             b = half_line * 0.9)),
          plot.caption = element_text(
              size = rel(0.9), hjust = 1,
              vjust = 1, margin = margin(
                             t = half_line * 0.9)),
          plot.margin = margin(
              half_line * 2.25, half_line * 5.5,
              half_line * 2.25, half_line * .5),
          complete = TRUE)
}

#' @importFrom ggplot2 ggplot_build geom_vline geom_hline
#' @export
theme_rtweet<- function(p,
                        base_size = 10,
                        base_family = "Consolas",
                        highlight = TRUE,
                        invert = FALSE,
                        simple = FALSE) {
    if (all(highlight, !invert)) {
        fill <- "#f6f6f6"
        col <- "#f0f0f0"
    } else if (all(!highlight, !invert)) {
        fill <- "#f0f0f0"
        col <- "#e9e9e9"
    } else if (all(highlight, invert)) {
        fill <- "#ffffff"
        col <- "#f6f6f6"
    } else if (all(!highlight, invert)) {
        fill <- "#ffffff"
        col <- "#f0f0f0"
    }
    if (!simple) {

        .p <- ggplot_build(p)
        xints <- .p$layout$panel_ranges[[1]]$x.major_source
        yints <- .p$layout$panel_ranges[[1]]$y.major_source
        p <- p + ggplot2::geom_vline(
                     xintercept = xints,
                     colour = "#66666633",
                     linetype = 2, size = .25) +
            geom_hline(
                yintercept = yints,
                colour = "#66666633",
                linetype = 2, size = .25) +
            theme_rt(
                fill = fill,
                col = col,
                base_family = base_family,
                base_size = base_size)
    } else {
        p <- p + theme_rt(
                     fill = fill,
                     col = col,
                     base_family = base_family,
                     base_size = base_size)
    }
    invisible(p)
}

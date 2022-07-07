##' add horizontal align lines layer to a tree
##'
##' 'geom_aline'align all tips to the longest one by adding 
##' padding characters to the right side of the tip.
##'
##'
##' @title geom_aline
##' @param mapping aes mapping
##' @param linetype set line type of the line, defaults to "dotted"
##' @param linewidth set width of the line, defaults to 1
##' @param ... additional parameter
##' @return aline layer
##' @export
##' @author Yu Guangchuang
geom_aline <- function(mapping=NULL, linetype="dotted", linewidth = 1, ...) {
    x <- y <- isTip <- NULL
    dot_mapping <- aes(xend=x+diff(range(x))/200, x=max(x), yend=y, subset=isTip)
    if (!is.null(mapping)) {
        dot_mapping <- modifyList(dot_mapping, mapping)
    }

    geom_segment2(dot_mapping,
                  linetype=linetype,
                  linewidth = linewidth, stat = StatTreeData, ...)
}



##' geom_segment2 support aes(subset) via setup_data
##'
##' 'geom_segment2' is a modified version of geom_segment, with subset aesthetic supported
##'
##' @title geom_segment2
##' @param mapping Set of aesthetic mappings, defaults to NULL
##' @param data A layer specific dataset -
##'             only needed if you want to override the plot defaults.
##' @param stat Name of stat to modify data.
##' @param position The position adjustment to use for overlapping points on this layer.
##' @param lineend Line end style, one of butt (default), round and square.
##' @param na.rm If "FALSE" (default), missing values are removed with a warning. If "TRUE", missing values are silently removed, logical.
##' @param show.legend Whether to show legend, logical.
##' @param inherit.aes Whether to inherit aesthetic mappings, logical, defaults to "TRUE".
##' @param nudge_x adjust the horizontal position of the segments.
##' @param arrow specification for arrow heads, as created by arrow().
##' @param arrow.fill fill color to usse for the arrow head (if closed). `NULL` means use `colour` aesthetic.
##' @param ... additional parameter
##' @importFrom ggplot2 layer
##' @export
##' @seealso
##' [geom_segment][ggplot2::geom_segment]
##' @return add segment layer
##' @author Guangchuang Yu
geom_segment2 <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", lineend = "butt",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         nudge_x = 0, arrow = NULL, arrow.fill = NULL,
                         ...) {

    default_aes <- aes_(node=~node)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSegmentGGtree,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow,
            lineend = lineend,
            na.rm = na.rm,
            nudge_x = nudge_x,
            ...
        ),
        check.aes = FALSE
    )
}


##' @importFrom ggplot2 GeomSegment
##' @importFrom ggplot2 draw_key_path
GeomSegmentGGtree <- ggproto("GeomSegmentGGtree", GeomSegment,
                             setup_data = function(data, params) {
                                 if (is.null(data$subset))
                                     return(data)
                                 data[which(data$subset),]
                             },

                             draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                                   lineend = "butt", linejoin = "round", na.rm = FALSE, nudge_x = 0) {

                                 data$x <- data$x + nudge_x

                                 data <- ggplot2::remove_missing(data, na.rm = na.rm, c("x", "y", "xend",
                                                        "yend", "linetype", "linewidth", "shape"), name = "geom_segment")
                                 if (empty(data))
                                     return(zeroGrob())
                                 if (!coord$is_linear()) {
                                     tmpgroup <- data$group
                                     starts <- subset(data, select = c(-xend, -yend))
                                     starts$group <- 1
                                     ends <- rename(subset(data, select = c(-x, -y)), c("x" = "xend", "y" = "yend"))
                                     ends$group <- 2
                                     pieces <- rbind(starts, ends)

                                     trans <- coord$transform(pieces, panel_params)
                                     starts <- trans[trans$group==1, ,drop=FALSE]
                                     ends <- trans[trans$group==2, ,drop=FALSE]
                                     ends <- rename(subset(ends, select=c(x, y)), c("xend"="x", "yend"="y"))
                                     data <- cbind(starts, ends)
                                     data$group <- tmpgroup
                                 }else{
                                     data <- coord$transform(data, panel_params)
                                 }

                                 arrow.fill <- arrow.fill %||% data$colour
                                 return(grid::segmentsGrob(data$x, data$y, data$xend, data$yend,
                                                           default.units = "native", gp = gpar(col = alpha(data$colour,
                                                           data$alpha), fill = alpha(arrow.fill, data$alpha),
                                                           lwd = data$linewidth * ggplot2::.pt, lty = data$linetype,
                                                           lineend = lineend, linejoin = linejoin), arrow = arrow)
                                       )


                                 ## data$x <- data$x - sapply(data$label, function(x) convertWidth(grobWidth(textGrob(x, gp=gpar(fontsize=.04* .pt))), "native", TRUE))
                                 ##GeomSegment$draw_panel(data = data, panel_params = panel_params, coord = coord,
                                 ##                       arrow = arrow, arrow.fill = arrow.fill,
                                 ##                       lineend = lineend, linejoin = linejoin, na.rm = na.rm)
                             }
                             )


empty <- getFromNamespace("empty", "ggplot2")
`%||%` <- getFromNamespace("%||%", "ggplot2")

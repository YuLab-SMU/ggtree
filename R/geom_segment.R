##' add horizontal align lines
##'
##'
##' @title geom_aline
##' @param mapping aes mapping
##' @param linetype line type
##' @param size line size
##' @param ... additional parameter
##' @return aline layer
##' @export
##' @author Yu Guangchuang
geom_aline <- function(mapping=NULL, linetype="dotted", size=1, ...) {
    x <- y <- isTip <- NULL
    dot_mapping <- aes(xend=x+diff(range(x))/200, x=max(x), yend=y, subset=isTip)
    if (!is.null(mapping)) {
        dot_mapping <- modifyList(dot_mapping, mapping)
    }

    geom_segment2(dot_mapping,
                  linetype=linetype,
                  size=size, stat = StatTreeData, ...)
}



##' geom_segment2 support aes(subset) via setup_data
##'
##'
##' @title geom_segment2
##' @param mapping aes mapping
##' @param data data
##' @param stat Name of stat to modify data
##' @param position position
##' @param lineend lineend
##' @param na.rm logical
##' @param show.legend logical
##' @param inherit.aes logical
##' @param nudge_x horizontal adjustment of x
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

                                 ## data$x <- data$x - sapply(data$label, function(x) convertWidth(grobWidth(textGrob(x, gp=gpar(fontsize=.04* .pt))), "native", TRUE))
                                 GeomSegment$draw_panel(data = data, panel_params = panel_params, coord = coord,
                                                        arrow = arrow, arrow.fill = arrow.fill,
                                                        lineend = lineend, linejoin = linejoin, na.rm = na.rm)
                             }
                             )





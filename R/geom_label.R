##' geom_label2 support aes(subset) via setup_data
##'
##' 'geom_label2' is a modified version of geom_label, with subset aesthetic supported
##'
##' @title geom_label2
##' @param mapping Set of aesthetic mappings, defaults to NULL.
##' @param data A layer specific dataset -
##'             only needed if you want to override the plot defaults.
##' @param ... other arguments passed on to 'layer'.
##' @param stat Name of the stat to modify data.
##' @param position The position adjustment to use for overlapping points on this layer.
##' @param family "sans" by default, can be any supported font.
##' @param parse if 'TRUE', the labels will be parsed as expressions, defaults to 'FALSE'.
##' @param nudge_x adjust the horizontal position of the labels.
##' @param nudge_y adjust the vertical position of the labels.
##' @param label.padding Amount of padding around label, defaults to 'unit(0.25, "lines")'.
##' @param label.r Use to set the radius of rounded corners of the label, defaults to 'unit(0.15, "lines")'.
##' @param label.size Size of label border, in mm, defaults to 0.25.
##' @param na.rm If "FALSE" (default), missing values are removed with a warning. If "TRUE", missing values are silently removed, logical.
##' @param show.legend Whether to show legend, logical, defaults to "NA".
##' @param inherit.aes Whether to inherit aesthetic mappings, logical, defaults to "TRUE".
##' @return label layer
##' @importFrom ggplot2 layer
##' @importFrom ggplot2 position_nudge
##' @examples
##' library(ggtree)
##' set.seed(123)
##' tr<- rtree(15)
##' x <- ggtree(tr)
##' x + geom_label2(aes(label = node, subset = isTip == FALSE))
##' @export
##' @seealso
##' [geom_label][ggplot2::geom_label]
##' @author Guangchuang Yu
##' @references
##' For more detailed demonstration of this function, please refer to chapter A.4.5 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
geom_label2 <- function(mapping = NULL, data = NULL,
                        ...,
                        stat = "identity",
                        position = "identity",
                        family = "sans",
                        parse = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = 0.25,
                        na.rm = TRUE,
                        show.legend = NA,
                        inherit.aes = TRUE) {

    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
        }

        position <- position_nudge(nudge_x, nudge_y)
    }

    default_aes <- aes_() #node=~node)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    if (parse == "emoji") {
        label_aes <- aes_string(label=paste0("suppressMessages(emoji(", as.list(mapping)$label,"))"))
        mapping <- modifyList(mapping, label_aes)
        emoji <- get_fun_from_pkg("emojifont", "emoji")
        parse <- FALSE
        family <- "EmojiOne"
    }


    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomLabelGGtree,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            parse = parse,
            family = family,
            label.padding = label.padding,
            label.r = label.r,
            label.size = label.size,
            na.rm = na.rm,
            ...),
        check.aes = FALSE
    )
}



##' @importFrom ggplot2 GeomLabel
GeomLabelGGtree <- ggproto("GeomLabelGGtree", GeomLabel,
                           setup_data = function(data, params) {
                               if (is.null(data$subset))
                                   return(data)
                               data[which(data$subset),]
                           }## ,
                           ## draw_panel = function(self, data, panel_scales, coord, parse = FALSE,
                           ##                       na.rm = FALSE,
                           ##                       label.padding = unit(0.25, "lines"),
                           ##                       label.r = unit(0.15, "lines"),
                           ##                       label.size = 0.25) {
                           ##     GeomLabel$draw_panel(data, panel_scales, coord, parse,
                           ##                         na.rm, label.padding, label.r, label.size)
                           ## },
                           ## required_aes = c("x", "y", "label"),

                           ## default_aes = aes(
                           ##     colour = "black", fill = "white", size = 3.88, angle = 0,
                           ##     hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                           ##     lineheight = 1.2
                           ## ),

                           ## draw_key = draw_key_label
                           )


